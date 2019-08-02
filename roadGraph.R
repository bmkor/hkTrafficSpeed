### data cleansing
### some suspected route end-point ordering issue and one typo
require(tidyverse)

getTDRoadsInSF<-function(specURL="http://static.data.gov.hk/td/traffic-speed-map/en/tsm_dataspec.pdf"){
  require(sf,quietly = T)
  require(pdftools, quietly = T)
  require(purrrlyr)
  require(dplyr)
  tmp<-tempfile()
  roadMetaData<-specURL
  download.file(roadMetaData,dest=tmp,quiet =T)
  
  roadMetaData<-pdftools::pdf_text(tmp)
  roadMetaData<-roadMetaData[-1]
  
  res<-sapply(roadMetaData, function(x){
    stmp<-unlist(strsplit(x,"\\n"))[-(1:2)]
    gsub("^\\s","",gsub(" ROAD", "_ROAD",gsub(" ROUTE", "_ROUTE",gsub("\\s+"," ",stmp))))
  })
  names(res)<-1:length(res)
  res<-c(paste("route","start","start_E","start_N","end","end_E","end_N","district","route_type",sep=" "),as.vector(unlist(res)))
  write(res,tmp)
  roadMetaData<-read.csv(tmp,sep=" ",stringsAsFactors = F)
  rownames(roadMetaData) <- roadMetaData$route
  roadMetaData %>%
    by_row(..f = function(r){
      st_linestring(as.matrix(rbind(as.numeric(r[3:4]),r[6:7])))
    }, .to = "geom") %>%
    mutate(start = as.character(start), end = as.character(end)) %>%
    select(route,start,end,district,route_type, geom) %>%
    st_sf() %>%
    st_set_crs(2326)
}

rds<-getTDRoadsInSF()

#### Cleansing: we assume the route A-B gives start = A and end = B. Let's check####
which(sapply(1:dim(rds)[1],function(i){
  !all(unlist(strsplit(rds[i,]$route,"-")) == c(rds[i,]$start,rds[i,]$end))
})) 
# if all good, the above should be empty.
# row 93 got problems

#### correct typos ####
rds[93,] #7881 or 7891?
rds[which(rds$start == "7881"),] # seems typo in the start of route 7891-789
rds[93,]$start <- "7891"

################################################################################################
### Cleansing: Assume the point coordinate information of route A-B is correct,              ###
###            the intersecting points found from the point coordinate should match with the ###
###            start/end points in A-B                                                       ###
################################################################################################

### break all pts
getAllpts<-function(rds){
  rds %>%
    select(start,end) %>%
    st_drop_geometry() %>%
    apply(1,function(r){
      rbind(r[1],r[2])
    }) %>%
    as.vector() %>%
    data.frame(id=. ,stringsAsFactors = F, geom = rds$geom %>%
                 st_cast('POINT'))  %>% 
    st_as_sf()
}
### take out those without any intersections or id appears once only
getRoots<-function(apts){
  apts %>%
    group_by(id) %>%
    tally() %>%
    filter(n == 1) %>%
    select(id, geometry)  
}

#### get pts and roots ####
apts<-getAllpts(rds)
roots<-getRoots(apts)
#### end ####


# should be an identity matrix
all(st_equals(roots,sparse =F)  == diag(1,nrow=nrow(roots),ncol=nrow(roots),names = T))


#### routes with roots are assumed to be correct (i.e. correct geomtry) ####
rds<-rds %>% 
  mutate(is_pivot = F, route_change = F, checked = F) %>%
  select(route:route_type,is_pivot, route_change, checked, geom) %>%
  mutate(is_pivot = ifelse((start %in% roots$id | end %in% roots$id), T, F))

#### check all filtered start-end routes are correct
rds %>%
  filter(is_pivot) %>%
  mapview(legend=F)  + vrds %>%
  mapview(legend=F)



rrds<-rds %>%
  filter(is_pivot) %>%
  group_by(route) %>%
  group_split() %>%
  lapply(function(r){
    if(r$route != paste(r$start,r$end,sep='-')){
      r
    }
  }) %>% 
  do.call(rbind,.) 



rvrds<-allpts %>%
  st_touches(.,rrds,sparse =F) %>%
  which(.,arr.ind = T) %>%
  apply(1,function(r){
    allpts[r[1],]
  }) %>%
  do.call(rbind,.)

crds<-rds %>%
  filter(!is_pivot) %>%
  st_touches(rrds,sparse = F)  %>%
  which(.,arr.ind = T) %>%
  apply(1,function(r){
    rr<-rds %>%
      filter(!is_pivot)
    rr[r[1],]
  }) %>%
  do.call(rbind,.)

cvrds<-allpts %>%
  st_touches(.,crds,sparse =F)%>%
  which(.,arr.ind = T) %>%
  apply(1,function(r){
    allpts[r[1],]
  }) %>%
  do.call(rbind,.)


cvrds<-crds %>%
  select(start,end) %>%
  st_drop_geometry() %>%
  as.matrix(byrow=F) %>%
  t(.) %>%
  as.vector() %>%
  data.frame(id=. ,stringsAsFactors = F, geom = crds %>%
               st_geometry() %>%
               st_cast('POINT'))  %>% 
  st_as_sf()

rrds %>%
  mapview(legend=F) + rvrds + crds + cvrds %>%
  mapview(legend=F)




rds[c(5,16,21),]

rds[5,]$route == paste(rds[5,]$start,rds[5,]$end,sep='-')


rds %>%
  filter(is_pivot) %>%
  dim()



vrds<-allpts %>%
  st_touches(.,rds %>% 
               filter(is_pivot), sparse = F) %>%
  which(.,arr.ind = T) %>%
  apply(1,function(r){
    allpts[r[1],]
  }) %>%
  do.call(rbind,.)






o<-st_equals(roots,apts,sparse=F)
o<-roots$id[which(apply(o,1,sum) > 1)] ### should be empty
o #"46522"  "991039" "991098"


### we assume the swap is correct
rds<-rds %>% 
  filter(start %in% o | end %in% o) %>%
  group_by(route) %>%
  group_split() %>%
  lapply(function(tmp){
    tmp %>% mutate(start=tmp$end,
                   end=tmp$start,
                   route_change=T)
  }) %>%
  do.call(rbind,.) %>%
  rbind(rds %>% 
          filter(!start %in% o & !end %in% o))

#### check route ####
# apts2<-getAllpts(rds)
# roots2<-getRoots(apts2)
# o2<-st_equals(roots2,apts2,sparse=F)
# roots$id[which(apply(o2,1,sum) > 1)] ### it is empty now
# rm(apts)
# rm(roots2)
# rm(o2)

#### corrections ####

apts<-apts %>% 
  filter(id == "50079") %>%
  slice(2) %>%
  rbind(., apts %>% filter(id != "50079")) 


apts<-apts %>% 
  filter(id == "910") %>%
  slice(2) %>%
  rbind(., apts %>% filter(id != "910")) 

rds<-rds %>%
  mutate(route=ifelse(route == "4649-46497","4649p-46497",route),
         start=ifelse(route == "4649p-46497", "4649p", start )) %>%
  mutate(route=ifelse(route == "46495-4649","46495-4649p",route),
         end=ifelse(route == "46495-4649p", "4649p", end)) %>%
  mutate(route=ifelse(route == "877-46498","877p-46498",route),
         start=ifelse(route == "877p-46498", "877p", start )) %>%
  mutate(route=ifelse(route == "793-877","793-877p",route),
         end=ifelse(route == "793-877p", "877p", end)) %>%
  mutate(route=ifelse(route == "793-7918","793p-7918",route),
         start=ifelse(route == "793p-7918", "793p", start )) %>%
  mutate(route=ifelse(route == "877-793","877-793p",route),
         end=ifelse(route == "877-793p", "793p", end)) %>%
  mutate(route=ifelse(route == "910-931","910-931p",route),
         end=ifelse(route == "910-931p", "931p", end))%>%
  mutate(geom=ifelse(route=="50079-50076", 
                     apts %>% 
                       filter(id == "50079") %>% 
                       distinct() %>%
                       st_coordinates() %>%
                       rbind(.,apts %>% 
                               filter(id == "50076") %>% 
                               distinct() %>% 
                               st_coordinates()) %>%
                       as.matrix() %>% st_linestring() %>%
                       st_geometry() %>%
                       st_set_crs(2326), geom)) %>%
  mutate(geom=ifelse(route=="50078-50079", 
                     apts %>% 
                       filter(id == "50078") %>% 
                       distinct() %>%
                       st_coordinates() %>%
                       rbind(.,apts %>% 
                               filter(id == "50079") %>% 
                               distinct() %>% 
                               st_coordinates()) %>%
                       as.matrix() %>% st_linestring() %>%
                       st_geometry() %>%
                       st_set_crs(2326), geom)) %>%
  mutate(geom=ifelse(route=="910-931p", 
                     apts %>% 
                       filter(id == "910") %>%
                       distinct() %>%
                       st_coordinates() %>%
                       rbind(.,apts %>% 
                               filter(id == "931") %>% 
                               slice(1) %>%
                               st_coordinates()) %>%
                       as.matrix() %>% st_linestring() %>%
                       st_geometry() %>%
                       st_set_crs(2326), geom)) %>%
  filter(!route %in% c("4649-46497","46495-4649",
                       "877-46498","793-877",
                       "793-7918", "877-793",
                       "910-931")) %>%
  st_set_crs(2326)

#### end of corrections ####

### 46332-3692 
### "888301-3692"
### rou<-"3692-46332" 
### rou<-"46332-46522"
checkRoute<-function(routes){
  rdstmp <- routes %>%
    filter(!checked)
  if (nrow(rdstmp) == 0){
    routes
  } else {
    rds_p<-rdstmp %>% 
      filter(is_pivot)
    rds_n<-rdstmp %>% 
      filter(!is_pivot)
    rds_r<-rds_p %>%
      st_touches(.,rds_n,sparse=F) %>%
      which(., arr.ind = T) %>%
      apply(1,function(r){
        # r<-c(12,73)
        rds_n[r[2],]
        pp<-rds_p[r[1],]
        pp<-data.frame(id=c(pp$start,pp$end),
                       stringsAsFactors = F) %>%
          st_set_geometry(pp %>%
                            st_geometry() %>%
                            st_cast('POINT') )
        
        rds_n[r[2],] %>%
          group_by(route) %>%
          group_split() %>%
          lapply(function(tmp){
            ttmp<-data.frame(id=c(tmp$start,tmp$end),
                             stringsAsFactors = F) %>%
              st_set_geometry(tmp %>%
                                st_geometry() %>%
                                st_cast('POINT'))
            rc<-ttmp %>%
              st_equals(pp,sparse = F) %>%
              which(., arr.ind=T) 
            
            if (nrow(rc) > 0){
              if(ttmp$id[rc[1,1]] != pp$id[rc[1,2]]){
                tmp %>%
                  mutate(start=tmp$end,
                         end=tmp$start,
                         is_pivot = T,
                         route_change = T)
              } else {
                tmp %>%
                  mutate(is_pivot = T)
              }
            } else {
              tmp
            }
          }) %>%
          do.call(rbind,.)
      }) %>%
      do.call(rbind,.)
    
    rds_r<-rds_r[sapply(unique(rds_r$route),function(r){
      which(r==rds_r$route)[1]
    }),]
    
    rds_p %>%
      mutate(checked = T) %>%
      rbind(.,rds_n %>%
              filter(!route %in% rds_r$route)) %>%
      rbind(.,rds_r ) %>%
      rbind(.,routes %>%
              filter(checked))
  }
}

r<-rds
while(!all(r$checked)){
  r<<-checkRoute(r)  
}
r %>%
  filter(route_change & district == "TM") %>%
  mapview()


#### checkRoute(r) ####

rr<-r %>%
  group_by(route) %>%
  group_split() %>%
  lapply(function(ro){
    if(ro$route_change){
      ro %>%
        mutate(route = paste(rev(unlist(strsplit(route,'-'))),collapse = '-'),
               route_change = F)
    } else{
      ro
    }
  }) %>%
  do.call(rbind,.)


allpts<-data.frame(id=as.vector(t(cbind(rr$start,rr$end))),
           stringsAsFactors = F) %>%
  st_set_geometry(rr %>%
                    st_geometry() %>%
                    st_cast('POINT'))
  

allpts<-allpts[sapply(unique(allpts$id),function(id){
  which(id==allpts$id)[1]
}),]


chk<-rr %>%
  group_by(route) %>%
  group_split() %>%
  lapply(function(ro){
    pt1<-allpts %>%
      filter(id == ro$start)
    
    pt2<-allpts %>%
      filter(id == ro$end)
    
    res<-st_linestring(matrix(c(pt1$geometry[[1]][1],pt1$geometry[[1]][2],
                           pt2$geometry[[1]][1],pt2$geometry[[1]][2]),ncol=2,byrow=T)) %>%
      st_geometry() %>%
      st_set_crs(2326) %>%
      st_equals(ro ,sparse =F) %>%
      as.vector()    
    res
  }) %>%
  unlist()


which(!chk) # should be empty

rr[111,]
rr %>%
  filter(start == "50076" | end == "50076" | start == "50079" | end == "50079")

rds %>%
  filter(start == "50076" | end == "50076" | start == "50079" | end == "50079")

apts %>%
  filter(id %in% c("50076","50079"))

apts %>%
  filter(id =="50079")

# 
# 
# rr[127,]
# rr %>%
#   filter(start == "50078" | end == "50078" | start == "762" | end == "762")
# allpts %>% 
#   filter(id %in% c("50078","762")) %>%
#   st_touches(rr[127,])

rr[92,] #4649p-46497 and 46495-4649p
rr %>%
  filter(start == "4649" | end == "4649")


rr[95,] #793-877p and 877p-46498
#rr[158,]
#rr[159,] 
rr %>%
  filter(start == "877" | end == "877")


rr[147,] #877-793p and 793p-7918
#rr[148,]
rr %>%
  filter(start == "793" | end == "793") 


rr[200,] #route 910-931 typo on "910", 816792.7 => 816792.6? and 910-931p
#rr[203,]
#rr[204,]
rr %>%
  filter(start == "931" | end == "931" | start == "910" | end == "910" |
           start == "50109" | end == "50109") # %>% mapview()


rr[111,] #route 50078-50079 typo on "50079", 836349 => 836349.2?
rr %>%
  filter(start == "50079" | end == "50079") 


24312.18 + 8860.33 + 9183.40 + 8483.40 - 35852.18
