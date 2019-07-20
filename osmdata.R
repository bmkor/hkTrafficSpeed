library(osmdata)
library(sf)
library(mapview)
library(dplyr)
library(nngeo)
library(dodgr)
# install.packages("devtools")
# devtools::install_github("hypertidy/silicate", force=T)


### Extract all highway
### st_bbox(c(xmin=113.80,ymin=22.350,xmax= 114.2,ymax=22.5))
### covers TM routes
tmbbox<-st_bbox(c(xmin=113.80,
                  ymin=22.350,
                  xmax= 114.2,
                  ymax=22.5))

hk <- opq('China') %>%
  add_osm_feature(key='highway', 
                  bbox = tmbbox) %>%
  osmdata_sf()


### we only concern highway
r<-hk$osm_lines %>%
  dplyr::filter(hk$osm_lines$highway %in% 
                  c("motorway", "secondary", "primary", "junction", "trunk_link", "primary_link",
                    "tertiary", "trunk", "bridge", "track", "road", "motorway_link"))


### get TM points
roads<-getTDRoads()
tmroads<-roads %>%
  subset(district == "TM")
  
tm<-tmroads %>%
  st_as_sfc() %>%
  st_cast("POINT")

tmroads<-tmroads %>%
  st_as_sfc()

#### find nearest neighbouring line
tmp<-st_nn(tm, r)
rr<-r[unlist(tmp),]

rr %>%
  mapview() + tm

which(rownames(rr) %in% c("1797.1","3317.1","1657.1", "360.1", "367.1"))

r[unlist(tmp),]$osm_id %in% dtm$way_id

#### c("1797.1","3317.1","1657.1", "360.1", "367.1")
#### c("158584319", "302867489", "157811060", "47071904", "47072173")
#### cleansing

rr %>% 
  filter(!osm_id %in% c("158584319", "302867489", "157811060",
                        "47071904","47072173")) %>%
  mapview() + mapview(tmroads,color="red") + tm


r[unlist(tmp)]

unlist(tmp)[1:5]


tid<-unique(r[unlist(tmp)[1:5],]$osm_id)

frid<-dtm[which(dtm$way_id == tid[1]),]$from_id
toid<-dtm[which(dtm$way_id == tid[2]),]$to_id[4]

rpath<-dodgr_paths(dtm,from=frid,to=toid)

dtm[which(dtm$way_id == "32663665"),]$to_id[3]
dtm[which(dtm$way_id == "47072186"),]$from_id

rrpath<-dodgr_paths(dtm,
                    from=dtm[which(dtm$way_id == "32663665"),]$to_id[3],
                    to=dtm[which(dtm$way_id == "47072186"),]$from_id)



r[unlist(tmp)[which(unlist(tmp) == "2984" )],] %>% 
   mapview() + tm[1:5] + rr + mapview(r[which(r$osm_id %in% dtm[which(dtm$from_id %in% rpath[[1]][[1]]),]$way_id),],color="red") +
  mapview(r[which(r$osm_id %in% dtm[which(dtm$from_id %in% rrpath[[1]][[1]]),]$way_id),], color = "orange")


### use dodgr
m<-matrix(tmbbox,nrow=2)
colnames(m) <- c("min","max")
rownames(m) <- c("x","y")
dtm<-dodgr_streetnet(m,expand=0) %>%
  weight_streetnet(wt_profile = "motorcar")


rr[which(rr$osm_id == "112284897"),] %>%
  mapview()

class(dtm)

sg<-dtm[which(dtm$way_id == "112284897"),]

rbind(as.matrix(sg[,c("from_lon","from_lat")]), 
      as.matrix(sg[nrow(sg),c("to_lon","to_lat")]))%>%
  st_multipoint() %>%
  st_sfc() %>%
  st_set_crs(4326) %>%
  mapview() + rr
  




dodgr_contract_graph(sg)



rr[nrow(rr),]$osm_id
sapply(unique(rr$osm_id),function(id){
  which(dtm$way_id == id)
})

unique(rr$osm_id) %in% dtm$way_id

all(sapply(unique(rr$osm_id), function(id) {
  id %in% dtmw$way_id
}))

dtmw[unlist(sapply(unique(rr$osm_id), function(id){
  which(dtmw$way_id == id)
})),]



weighting_profiles$weighting_profiles$name

unique(dtmw$highway)

rr$osm_id %in% dtmw$way_id

ttr<-r[1,] %>%
  st_transform(2326)

st_buffer(ttr,5) %>%
  mapview() + ttr$geometry 

st_buffer(ttr,0.1) %>%
  st_contains(ttr)

r %>%
  mapview() + tm





### use scilicate to find the relation? Not much use
hks <- opq('China') %>%
  add_osm_feature(key='highway', 
                  bbox = st_bbox(c(xmin=113.80,
                                   ymin=22.360,
                                   xmax= 114.2,
                                   ymax=22.5))) %>%
  osmdata_sc()


# oo<-hks$object %>% 
#   filter(key == "highway", value %in% 
#            c("motorway", "secondary", "primary", "junction", "trunk_link", "primary_link",
#              "tertiary", "trunk", "bridge", "track", "road", "motorway_link")) %>%
#   select(object_) %>%
#   unique()
# 
# ee<-hks$object_link_edge %>%
#   filter(object_ == oo$object_[1]) %>%
#   select(edge_) %>%
#   unique()
# 
# vv<-hks$edge %>%
#   filter(edge_ == ee$edge_[1])
# 
# ex<-hks$vertex %>%
#   filter(vertex_ %in% c(vv$.vx0, vv$.vx1)) %>%
#   st_as_sf(coords = c("x_", "y_")) %>%
#   st_set_crs(4326)


r %>%
  mapview()
c(ex$geometry, r$geometry) %>%
  mapview()
length(r$geometry)

ttg<-r$geometry[which(r$osm_id %in% c(231698820,9409770,
                                 150053258))]

ttg %>% mapview()
ttg<-ttg %>% 
  st_cast('POINT')




r$geometry[which(r$osm_id %in% c(231698820,9409770,
                                 150053258))] %>% mapview()

as.vector(unlist(lapply(ttg,function(g){g[2]})))


hks$object %>%
  filter(object_ %in% c(231698820,9409770,
                        150053258))



vvg<-hks$vertex %>%
  filter(y_ %in% as.vector(unlist(lapply(ttg,function(g){g[2]})))) %>%
  select(vertex_)

eeg<-hks$edge %>% 
  filter(.vx0 %in% as.vector(vvg$vertex_) | .vx1 %in% as.vector(vvg$vertex_)) %>%
  select(edge_)

oog<-hks$object_link_edge %>%
  filter(edge_ %in% eeg$edge_) %>%
  select(object_)

hks$object %>%
  filter(object_ %in% oog$object_)




rg<-hks$object_link_edge %>%
  filter(object_ %in% oog$object_) %>%
  dplyr::select(edge_)

hks$object_link_edge %>%
  filter(edge_ %in% rg$edge_) %>%
  dplyr::select(object_) %>%
  unique()



hks$object %>%
  filter(object_ == oog$object_[1]) 
  
which(hk$osm_lines$osm_id == "25159468")

w<-c(r$geometry[which(r$osm_id %in% c(231698820,9409770,
                                    150053258))],
     r$geometry[unique(unlist(tmp))[unique(unlist(tmp)) != which(r$osm_id == 158584319)]] )


r %>%
  filter(osm_id  %in% c(231698820,9409770,
                        150053258))



rr<-lapply(w, function(i) {
  df<-rbind.data.frame(i[1,],i[nrow(i),])
  names(df) <- c('x','y')
  df %>%
    st_as_sf(coords=c('x','y')) %>%
    st_set_crs(4326)
})


rr<-do.call(rbind,rr)

rrc<-do.call(rbind,lapply(w, function(i) {
  rbind(i[1,],i[nrow(i),])
}))


rrv<-hks$vertex %>%
  filter(x_ %in% rrc[,1] | y_ %in% rrc[,2]) %>%
  dplyr::select(vertex_) %>%
  unique()

rre<-hks$edge %>%
  filter(.vx0 %in% rrv$vertex_ | .vx1 %in% rrv$vertex_) %>%
  dplyr::select(edge_) %>%
  unique()

rro<-hks$object_link_edge %>%
  filter(edge_ %in% rre$edge_) %>%
  dplyr::select(object_) %>%
  unique()


hk$osm_lines %>%
  filter(osm_id %in% rro$object_) %>%
  mapview() + rr$geometry


library(dodgr)


dodgr_streetnet_sc
ws<-weight_streetnet(hks)


ws %>%
  filter(edge_ %in% rre$edge_)



rr %>%
  mapview() + w

dd<-dodgr_streetnet(pts=tcoo, expand = 0) %>%
  weight_streetnet()



g0<-dd %>%
  filter(component == 1 & highway %in%
           c("motorway", "secondary", "primary", "junction", "trunk_link", "primary_link",
             "tertiary", "trunk", "bridge", "track", "road", "motorway_link"))

ww<-lapply(1:nrow(g0),function(i){
  matrix(c(as.numeric(g0[i,4:5]), as.numeric(g0[i,7:8])),nrow=2,byrow=T)
}) %>%
  st_multilinestring() %>%
  st_sfc() %>%
  st_set_crs(4326)
  
ww %>%
  mapview()


ww<-lapply(1:nrow(g0),function(i){
  matrix(c(as.numeric(g0[i,4:5]), as.numeric(g0[i,7:8])),nrow=2,byrow=T) %>%
    st_linestring() %>%
    st_sfc() %>%
    st_set_crs(4326)
})


pp<-tcoo[1,] %>% 
  st_point() %>%
  st_sfc() %>%
  st_set_crs(4326)

rp<-lapply(ww,function(r){
  r %>%
    st_contains(pp) %>%
    length()
})

wwl<-do.call(c,ww)

wwl %>%
  mapview()


apply(g0,1,function(r){
  st_linestring(matrix(c(as.numeric(r[4:5]), as.numeric(r[7:8])),nrow=2,byrow=T))  
})


unique(g0$highway)

dw<-dodgr_dists(g0,tcoo)



dd %>%
  filter(component == 1) %>%
  mapview() + tm


# dd %>%
#   filter(from_lon == tcoo[c(74,123),][1,1])
# 
# tcoo[c(74,123),] 

tcoo<-do.call(rbind,st_geometry(rr)) %>%
  as_tibble() %>%
  setNames(c("x","y")) %>%
  as.matrix()

dodgr_streetnet(pts=as.matrix(tcoo))

bb <- osmdata::getbb ("york uk")
npts <- 10

xy <- apply (bb, 1, function (i) min (i) + runif (npts) * diff (i))
dodgr_streetnet(pts=xy, expand =0 )

rr[c(74,123),] %>%
  st_geometry() %>%
  as_tibble() %>%
  setNames(c('lon', 'lat'))


hk$osm_lines %>%
  filter(osm_id %in% oog$object_) %>%
  mapview() + w + rr$geometry

hk$osm_lines %>%
  filter(osm_id %in% oog$object_[3]) %>%
  mapview() + ttg


require(silicate)

sc_edge(hks)



r[5,] #
?add_osm_feature
plot(lcnr9$osm_lines)

names(hksp$osm_lines@data)
hksp$osm_lines@data$ref


tt<-opq_osm_id(id=hk$osm_lines$osm_id[1:10],type="relation") %>%
  opq_string() %>%
  osmdata_sf()

tt$osm_multipolygons$natural

tt$osm_lines %>%
  mapview()


#available_features()


# r %>%
#   mapview()

# rr <- subset(r,r$highway %in% 
#                c("motorway", "secondary", "primary", "junction", "trunk_link", "primary_link",
#                  "tertiary", "trunk", "bridge", "track", "road", "motorway_link"))
# 
# rr %>% 
#   mapview()


#### plot
c(r$geometry, tm) %>%
  mapview()


#### find nearest neighbouring line
tmp<-st_nn(tm, r$geometry)

#### 
c(tm, r$geometry[unique(unlist(tmp))[unique(unlist(tmp)) != which(r$osm_id == 158584319)]]
  , r$geometry[which(r$osm_id %in% c(231698820,9409770,
                                     150053258))]) %>%
  mapview()



opq_string( opq('Hong Kong'))
r[1,43]
rr<-st_combine(r)
length(rr)
dim(r)
rr<-st_join(r,r)
rr %>% mapview()
tmp %>% mapview()
plot(tm)
plot(subset(hk$osm_lines, grepl("Tuen Mun",hk$osm_lines$name) ))
subset(hk$osm_lines, grepl("Tuen Mun",hk$osm_lines$name) ) %>%
  mapview()


c(st_nearest_points(rr$geometry[tmp[[1]]],tmSF[1]),st_intersection(st_nearest_points(rr$geometry[tmp[[1]]],tmSF[1]),
                                                                   rr$geometry[tmp[[1]]]),
  rr$geometry[tmp[[1]]], tmSF[1]) %>% mapview()
a<-st_intersection(st_nearest_points(rr$geometry[tmp[[1]]],tmSF[1]),
                   rr$geometry[tmp[[1]]])
b<-rr$geometry[tmp[[1]]]

a<-st_nearest_points(rr$geometry[tmp[[1]]],tmSF[1])

a<-st_transform(rr$geometry[tmp[[1]]], 2326)
b<-st_transform(tmSF[1], 2326)

n<-st_nearest_points(b,a)

aa<-st_intersection(n,a)

st_contains(a,aa)
c(a,aa) %>% mapview()

c(n,st_cast(st_difference(a, st_buffer(n, dist=1e-3)),"LINESTRING")) %>% 
  mapview()
st_buffer(n, dist=10) %>% mapview()

tol<-0.1
units(tol)<-"m"

c(aa,st_snap(a,aa,tol)) %>% mapview()

aa<-st_intersection(st_transform(a, 32636),
                st_transform(b,32636) )


st_contains(st_transform(b,32636), aa)
st_intersects(a,b)
st_transform(a,4326)
c(a,b) %>% mapview()
class(rr$geometry[tmp[[1]]])

st_snap(rr$geometry[tmp[[1]]],tmSF[1])
?st_connect
?st_intersection
st_snap(a,b)
st_crosses

tot = 1
units(tot) = "m"

st_buffer(tmSF[1],
          st_distance(tmSF[1],rr$geometry[tmp[[1]]]) + tot)

st_distance()

