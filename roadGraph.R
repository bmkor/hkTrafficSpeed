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

### Cleansing: we assume the route A-B gives start = A and end = B. Let's check
which(sapply(1:dim(rds)[1],function(i){
  !all(unlist(strsplit(rds[i,]$route,"-")) == c(rds[i,]$start,rds[i,]$end))
})) 
# if all good, the above should be empty.
# row 93 got problems

rds[93,] #7881 or 7891?
rds[which(rds$start == "7881"),] # seems typo in the start of route 7891-789
rds[93,]$start <- "7891"

################################################################################################
### Cleansing: Assume the point coordinate information of route A-B is correct,              ###
###            the intersecting points found from the point coordinate should match with the ###
###            start/end points in A-B                                                       ###
################################################################################################

### break all pts
allpts<-rds %>%
  select(start,end) %>%
  st_drop_geometry() %>%
  apply(1,function(r){
    rbind(r[1],r[2])
  }) %>%
  as.vector() %>%
  data.frame(id=. ,stringsAsFactors = F, geom = rds$geom %>%
               st_cast('POINT'))  %>% 
  st_as_sf()

### take out those without any intersections or id appears once only
roots<-allpts %>%
  group_by(id) %>%
  tally() %>%
  filter(n == 1) %>%
  select(id, geometry)

st_equals(roots,sparse =F) # should be an identity matrix

### routes with roots are assumed to be correct (i.e. correct geomtry)
rds<-rds %>% 
  mutate(is_pivot = F, route_change = F, checked = F) %>%
  select(route:route_type,is_pivot, route_change, checked, geom) %>%
  mutate(is_pivot = ifelse((start %in% roots$id | end %in% roots$id), T, F))
  
### 46332-3692 

checkRoute<-function(routes){
  # routes<-r
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
      st_intersects(rds_n,sparse=F) %>%
      which(., arr.ind = T) %>%
      apply(1,function(r){
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
                print("a")
                print(ttmp$id[rc[1,1]])
                print(pp$id[rc[1,2]])
                print("b")
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
              tmp %>%
                mutate(is_pivot = T)
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

# checkRoute(r)

rr<-r %>%
  group_by(route) %>%
  group_split() %>%
  lapply(function(ro){
    if(ro$route_change){
      ro %>%
        mutate(route = paste(rev(unlist(strsplit(route,'-'))),collapse = '-'))
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

allpts %>% filter(id == "30069")
r %>% 
  filter(start == "30069" | end == "30069")
  # filter(start == "888301")

chk<-rr %>%
  group_by(route) %>%
  group_split() %>%
  lapply(function(ro){
    ro<-rr[3,]
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

which(!chk)
rr[1,]



