library(osmdata)
library(sf)
library(mapview)
library(dplyr)
library(nngeo)
library(dodgr)
library(tools)

#### get route spatial information from the data.gov.hk
roads<-getTDRoads()

#### filter roads by district == Kowloon
kroads<-roads %>%
  filter(district == "K")

#### find out the boundary box containing the filtered roads
kbbox <- kroads %>%
  st_transform(4326) %>%
  st_bbox()

#### visual check
kroads %>%
  mapview() %>% 
  addExtent(data=kroads)  

#### break all the linestrings to points
kpts <- kroads %>%
  st_cast("POINT")

#### visual check again
kroads %>%
  mapview() + kpts

#### get all the highway from OSM inside the kowloon bbox
hkk <- opq('China') %>%
  add_osm_feature(key='highway', 
                  bbox = kbbox) %>%
  osmdata_sf() ## Take quite a while


#### filter relevant highway
kr<-hk$osm_lines %>%
  filter((highway %in% 
                  c("motorway", "secondary", "primary", "junction", "trunk_link", "primary_link",
                    "tertiary", "secondary_link", "trunk", "bridge", "track", "road", "motorway_link") | 
            junction == "roundabout")) %>%
  st_transform(2326)

#### check
nrow(kr) #3621

#### find nearest neighbouring line and extract the road name
kroads<-kroads %>%
  by_row(..f = function(r){
    r %>%
      st_cast("POINT") %>%
      st_nn(.,kr) %>%
      unlist %>%
      kr[.,] %>%
      select(name) %>%
      st_drop_geometry %>%
      unlist %>%
      paste(collapse = "--")    
  },.to = "routename", .collate = "cols") %>%
  select(-geometry, geometry) %>%
  st_sf()

#### visual check
kroads %>%
  mapview()

#### get all motorcar roads (may include residential)

dk<-getStreetnet(kkbox)

#### for reference (may wait a while)
dk %>%
  mapview()

####

#### create graph from routes
require(stplanr, quietly = T)
require(igraph, quietly = T)

g<-kroads %>%
  st_touches(.,.) %>%
  as.matrix() %>%
  provideDimnames(base=list(as.vector(kroads$route),as.vector(kroads$route))) %>%
  graph_from_adjacency_matrix(.,mode="undirected",diag = F,
                              add.rownames = "route")  %>%
  set_vertex_attr("roadnames",value=kroads$routename) %>%
  set_vertex_attr("geometry",value=kroads$geometry)

#### visual check
g %>% 
  plot(vertex.size=1,
       vertex.label.cex = 0.5,
       vertex.label.dist = 0.25,
       vertex.label.degree = pi/2,
       layout = layout_(g, as_tree()),
       asp=0)

#### let's check with degree
names(which(degree(g) == 1)) #6 routes got degree 1

#### bind with kroads
kroads<-kroads %>%
  mutate(route=as.vector(route)) %>%
  by_row(..f=function(r){
    degree(g)[r$route]
  },.to="degree",.collate="cols") %>%
  select(-geometry,geometry) %>%
  st_sf()
  

#### visual check
kroads %>%
  mutate(deg = ifelse(degree == 1, "1", ">1")) %>%
  mapview(zcol="deg",burst=T) # 3-pair start/end

#### take out routes
sg <- g

sg <- induced.subgraph(g,V(g)[!(V(g)$name %in% mroutes$route)])

pp<-lapply(get.shortest.paths(sg,which(degree(sg) == 1)[2],mode="out")$vpath,function(p){
  do.call(rbind,lapply(names(p),function(n){
    kroads %>%
      filter(route == n)
  }))
}) 

# special handling for the cycle

sg<- induced_subgraph(g,c("34212-33701","33701-3369", "3369-33691", "33691-33671", "33671-3363",
                          "3363-3369", "3369-3370","3370-34211"))

get.shortest.paths(sg,"34212-33701",mode="out")

pp<-lapply(c("34212-33701","33701-3369", "3369-33691", "33691-33671", "33671-3363",
  "3363-3369", "3369-3370","3370-34211"),
  function(r){
    kroads %>%
      filter(route == r)
  }) %>%
  do.call(rbind,.) %>%
  mutate(routename = V(sg)[which(route  == V(sg)$name)]$roadnames) %>%
  select(-geometry,geometry)

pp %>% mapview() %>% addExtent(data=(st_bbox(pp) + c(-120,-20,30,20) ) %>% st_as_sfc)

fdk<-getStreetnet((st_bbox(pp) + c(-120,-20,30,20) ) )

fdk<-fdk %>%
  st_intersects(.,(st_bbox(pp) + c(-120,-20,30,20)) %>% st_as_sfc, sparse =F) %>%
  unlist(.) %>%
  as.vector() %>%
  fdk[.,]

fdk %>% mapview(color="red") + pp #%>% addExtent(data=(st_bbox(pp) + c(-120,-20,30,20) ) %>% st_as_sfc) 

#tro<-c("78201-88820","88820-888201", "888201-3470", "3470-3006", "3006-30069","30069-888301")

pp<-pp %>%
  extract(!sapply(.,is.null))

pp<-pp %>%
  lapply(.,function(p){
    p %>%
      filter(!route %in% mroutes$route)
  }) 

pp<-pp %>%
  sapply(.,function(p){
    p %>%
      filter(row_number() == 1 | row_number() == nrow(.)) %>%
      filter(degree == 1) %>%
      nrow(.) %>%
      equals(.,2)
  }) %>%
  which(.) %>%
  extract(pp,.) 

pp<-pp %>%
  extract2(lapply(.,nrow) %>%
            which.max()) #%>% mapview()
  # rbind(.,kroads %>% filter(route %in% c("888301-3692", "3692-46332",
  #                                        "46332-46522")) ) %>% 
  #filter( !route %in% tro ) #%>% mapview()
  
#### start and end routes are having degree 1. 
#pp[[which.max(sapply(pp,nrow))]] %>%
pp %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  filter(degree == 1) %>%
  nrow() %>%
  all(. == 2)

#pp<-pp[[which.max(sapply(pp,nrow))]]
#### visual check 
pp %>% 
  mutate(deg=ifelse(degree == 1, "1","+1")) %>%
  mapview(zcol="deg",burst=T) + mroutes

pp %>% 
  filter(degree > 2) %>% pull(route)
  by_row(..f = function(r){
    r$route
  })

incident_edges(sg,V(sg)[which(V(sg)$name == "88801-36821")],mode="total")
pp %>% pull(route)

kroads %>%
  mapview(zcol="degree")



#### get sequence of road names
require(stringi)

seqR<-pp %>%
  mutate(rname = gsub("NA","",routename)) %>%
  pull(rname) %>%
  sapply(.,function(r){unlist(strsplit(r,"--"))}) %>%
  unlist %>%
  as.vector() %>%
  stri_remove_empty() %>%  
  unique 

seqR
#### visual check seqR
pp %>%
  st_cast("POINT") %>%
  mapview(zcol="routename",burst=T) +pp[[which.max(sapply(pp,nrow))]]

seqR<-seqR[-c(4,8)]  #1
seqR<-seqR[-c(1,3,6,8)] #2
seqR<-seqR[-c(5)] #3
#sg
seqR #1
#### filter motorroads with names in seqR and union with those without names


fdk<-dk %>%
  filter(name %in% seqR | (is.na(name) & !highway %in% c("residential","unclassified",
                                                         "living_street","service"))) 
ppts<- pp %>%
  st_cast("POINT")

ppts %>%
  mapview() + pp

to<-ppts[nrow(ppts),] %>%
  st_nn(.,fdk,k=3) %>%
  unlist() %>%
  fdk[.,] %>% mapview() + ppts[nrow(ppts),] + pp
  select(to_id) %>%
  st_drop_geometry() %>%
  unlist() %>%
  as.vector()
  
from<-ppts[1,] %>%
  st_nn(.,fdk) %>%
  unlist() %>%
  fdk[.,] %>% #mapview() + ppts[1,] + pp
  select(from_id) %>%
  st_drop_geometry() %>%
  unlist() %>%
  as.vector()

ftids<- ppts %>%
  filter(row_number() %in% c(1,nrow(.))) %>%
  st_nn(.,fdk) %>%
  unlist() %>%
  fdk[.,] #%>% mapview() + pp
  select(from_id,to_id) %>%
  st_drop_geometry() %>%
  unlist(as.list(.)) %>%
  extract(.,c(1,4))

dk %>% mapview() + pp

ppts[nrow(ppts),] %>%
  st_nn(.,dk,k=3) %>%
  unlist() %>%
  dk[.,] %>% mapview() + ppts #choose to_id = "587637085"

to<-"587637085" #3

getL<-function(fdk,seqR,ppts,from,to){
  require(tools,quietly = T)
  p<-fdk %>%
    dodgr_paths(.,from = from, to = to, vertices = F) %>%
    list.flatten() 
  if(length(p) == 0){
    stop("empty route", call. = F)
  }
  
  l<- p %>%
    extract((lapply(.,function(e){
      ifelse(all(seqR %in% (fdk[e,] %>%
                              pull(name))), length(e), NA)
      
    }) %>%
      which.min)) %>%
    lapply(.,function(e){
      fdk[e,]
    }) 
  
  if(length(l) > 0){
    l <- l %>% extract2(1)
  } else{
    stop("No route with desired road names")
  }
  
  #### let's partition the motor route
  
  apply(st_distance(ppts,l),1,which.min) %>%
    matrix(ncol=2,byrow=T) %>%
    cbind(.,seq(1,nrow(ppts),by=2)) %>%
    apply(.,1,function(r){
      ll<- l[r[1]:r[2],]
      cc<- ll %>% 
        st_coordinates()
      
      hh<- st_nearest_points(ppts[r[3],],l[r[1],]) %>%
        st_coordinates() %>% extract(c(2,4))
      ee<- st_nearest_points(ppts[r[3]+1,],l[r[2],]) %>%
        st_coordinates() %>% extract(c(2,4))
      
      cc[1,1:2] <- hh
      cc[nrow(cc),1:2] <- ee
      ppts[r[3],] %>% 
        select(route,district,route_type,degree) %>%
        mutate(start_E = hh[1], start_N = hh[2],
               end_E = ee[1], end_N = ee[2]) %>% 
        st_drop_geometry() %>%
        st_set_geometry(st_linestring(cc[,1:2]) %>%
                          st_geometry()) %>%
        # st_sf() %>% 
        st_set_crs(2326)
    }) #%>%
    #do.call(rbind,.)
}

l2<-getL(dk,seqR,ppts,from,to)

l2 %>% 
  mapview() + ppts

do.call(rbind,l2)$route %in% mroutes$route

mroutes<-rbind(mroutes,do.call(rbind,l2))

mroutes %>% 
  mapview(color="red") + (kroads %>% filter(!route %in% mroutes$route))
