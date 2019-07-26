### data cleansing
### some suspected route end-point ordering issue and one typo

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

w<-rds %>%
  st_intersects(sparse=F)


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
  

i<-140
i<-1

rpts<-rds[which(w[i,]),] %>%
  st_intersection() %>%
  filter(st_geometry_type(.) %in% "POINT") %>%
  select(geometry)


tmp<-rds[which(w[i,]),] %>%
  st_intersection() %>%
  filter(st_geometry_type(.) %in% "POINT") %>%
  select(geometry)


ttmp<-rds[which(w[i,]),] %>%
  select(start,end) %>%
  st_drop_geometry() %>%
  apply(1,function(r){
    rbind(r[1],r[2])
  }) %>%
  as.vector() %>%
  data.frame(id=. ,stringsAsFactors = F, 
             rid = rep(1:(length(.)/2), each = 2),
             route=rep(rds[which(w[i,]),]$route,each=2),
             repeated_id = F,
             start_end = rep(c('start','end'),times = length(.)/2),
             geom = rds[which(w[i,]),]$geom %>%
               st_cast('POINT'))  %>%
  add_count(id) %>%
  mutate(repeated_id = ifelse(n>1, T, F)) %>%
  select(id:start_end,n,geometry) %>%
  st_as_sf() %>%
  add_column(matched = ttmp %>%
               st_equals(tmp,sparse = F) %>%
               cbind(ttmp$repeated_id) %>%
               apply(1,function(r){
                 !(any(r))
               }), .before = "n") %>%
  group_by(rid) %>%
  mutate(matched = ifelse(any(matched), T , F))


ttmp<-rds[which(w[i,]),] %>%
  select(start,end) %>%
  st_drop_geometry() %>%
  apply(1,function(r){
    rbind(r[1],r[2])
  }) %>%
  as.vector() %>%
  data.frame(id=. ,stringsAsFactors = F, 
             rid = rep(1:(length(.)/2), each = 2),
             route=rep(rds[which(w[i,]),]$route,each=2),
             repeated_id = F,
             start_end = rep(c('start','end'),times = length(.)/2),
             geom = rds[which(w[i,]),]$geom %>%
               st_cast('POINT'))  %>%
  add_count(id) %>%
  mutate(repeated_id = ifelse(n>1, T, F)) %>%
  select(id:start_end,n,geometry) %>%
  st_as_sf() 

ttmp <- ttmp %>%
  add_column(matched = ttmp %>%
               st_equals(tmp,sparse = F) %>%
               cbind(ttmp$repeated_id) %>%
               apply(1,function(r){
                 !(any(r))
               }), .before = "n") %>%
  group_by(rid) %>%
  mutate(matched = ifelse(any(matched), T , F))


ttmp %>%
  group_by(route) %>%
  group_split() %>%
  # split(ttmp$route) %>%
  lapply(function(df){
    if (all(df$matched)){
      df
    } else {
      print("dffff")
      df %>%
        mutate(id2 = id, route2 = route) %>%
        filter(!matched) %>%
        group_by(route, id) %>%
        group_map(~ chkFct(.x, (ttmp %>% filter(matched & repeated_id)))) 
      # %>%
      #     mutate(id=id2, route = route2) %>%
      #     select(id,route, rid:n,geometry)
      # %>%
      #   do.call(rbind,.) %>%
    }
  }) %>%
  lapply(function(df){
    df2<-df
    if(all(df$matched)){
      if (df$start_end[1] == "end"){
        df2<-df2[c(2,1),]        
      }
    } else {
      df2<-df2[c(2,1),]
    }
    
    data.frame(route = df2$route[1],
               route2 = paste(df2$id,collapse = "-"),stringsAsFactors = F) %>%
    mutate(geom = list(df2$geometry %>%
                         st_coordinates() %>%
                         st_linestring())) %>%
      st_sf() %>%
      st_set_crs(2326)
  }) %>%
  do.call(rbind,.)

chkFct<-function(x,y){
  fy<-y %>% filter(id == x$id2)
  x %>%
    mutate(id=id2, route=route2,
           matched = unlist(as.vector(st_equals(fy, sparse = F)))) %>%
    select(id,rid,route,repeated_id,start_end,matched,n,geometry)
}



'test' %>%
  data.frame(route=., stringsAsFactors = F) %>%
  mutate(geom = list(rr[[1]]$geometry %>%
                       st_coordinates() %>%
                       st_linestring())) %>%
  # by_row(..f = function(r){
  #   rr[[1]]$geometry %>%
  #     st_coordinates() %>%
  #     st_linestring()
  # }, .to = "geom") %>%
  st_sf()



  mutate(geom = st_linestring(rr[[1]]$geometry %>%
                                st_coordinates() %>%
                                as.matrix())) %>%
  

  
  st_linestring(as.matrix(rbind(as.numeric(r[3:4]),r[6:7])))


  mutate(id2 = id, route2 = route) %>%
  nest(-matched) %>%
  

ttmp %>%
  mutate(id2 = id, route2 = route) %>%
  filter(!matched) %>%
  group_by(route, id) %>%
  group_map(~ chkFct(.x, (ttmp %>% filter(matched & repeated_id)))) %>%
  do.call(rbind,.) %>%
  mutate(id=id2, route = route2) %>%
  select(id,route, rid:n,geometry)
  


tmpF <- function(d){
  print(d)
  d %>% group_by(id, add = T) %>%
    group_map(~ chkFct(.x, (ttmp %>% filter(matched & repeated_id)))) %>%
    do.call(rbind,.) %>%
    mutate(id=id2) %>%
    select(id,route:n,geometry)
}
  group_by(id, add = T) %>%
  group_map(~ chkFct(.x, (ttmp %>% filter(matched & repeated_id)))) %>%
  do.call(rbind,.) %>%
  mutate(id=id2) %>%
  select(id,route:n,geometry)
  ttmp
  
ttmp %>%
  filter(!matched) %>%
  pull(id)

ttmp %>% filter(matched & repeated_id & id == "722")


ww<-ttmp %>%
  filter(matched & repeated_id)

ww$id[c(F,F)]

ttmp[1,] %>%
  st_equals((ttmp %>% filter(matched & repeated_id)), sparse = F) %>%
  as.vector() %>%
  ww$id[.]





qq<-sapply(1:dim(w)[1],function(i){
    
    # ttmp %>%
    #   filter(!matched) %>%
    #   by_row(function(u){
    #     # print(r[6])
    #     w<-ttmp %>%
    #       filter(matched & repeated_id & id == u$id) %>%
    #       st_equals(u,sparse = F) %>%
    #       as.vector()
    #     print(w)
    #     # rtmp<-ttmp %>%
    #     #   filter(repeated_id & matched & id == r[1]) 
    #     # print(rtmp)
    #     
    #   })
    # 
    # ttmp %>%
    #   rowwise() %>%
    #   nest(-id) %>%
    #   # mutate(s = map_dbl(data, function(x){
    #   #   x
    #   # })) %>%
    #   unnest()
    # 
    # ttmp %>%
    #   group_by(rid) %>%
    #   group_split()
    # 
    # ttmp %>%
    #   filter(!matched) %>%
    #   group_by(id) %>%
    #   group_map()
    

  
  
  



    ww$id[www]
    
    
    matchfct <- function(x, y){
      
    }
    
    
    ?group_map
    ww 
    ww$data[[2]]
    
    ttmp %>%
      mutate(matched = ifelse(!matched  , F, T  ))
    
    ttmp %>%
      nest(-id)
    
      rowwise() %>%
      do({
        matched = ttmp %>%
          filter(matched & repeated_id & id == .$id) %>%
          st_equals(.$geometry, sparse = F) %>%
          as.vector()
     
      })
      
      
      
  

        
    ttmp %>%
      filter(repeated_id & matched & id == ttmp$id[1]) %>%
      st_equals(ttmp %>% filter(id == ttmp$id[1] & matched), sparse =F)
    
    ttmp
    ttmp %>%
      filter(repeated_id & matched & id == id) %>%
      select(rid) %>% st_drop_geometry()
    
  ttmp 
    filter(any(matched))
    filter(matched) %>%
    select(rid) %>%
    
    
  
  ??OR
  ww<-ttmp %>%
    st_equals(tmp,sparse = F) 
  ww & ttmp$repeated_id
  ttmp %>% 
    filter(repeated_id == T) %>%
    group_by(id) %>%
    add_column(result=st_equals(tmp,sparse = F))
        
     %>%
    as.vector()
    
  
  
  ?add_count
  ttmp %>%
    group_by(id) %>%
    mutate_if(n() > 1, repeated_id = T) %>%
    
  
  
  
  v<-ttmp %>%
    group_by(id) %>%
    filter(n() == 1) %>%
    st_as_sf()
  
  rv<-ttmp %>%
    group_by(id) %>%
    filter(n() > 1) %>%
    st_as_sf()
  
  v %>%
    st_equals(tmp, sparse = F) %>%
    which() %>%
    v[-(1:length(v$id)[.]),]
    sapply(function(r) {v[-r,]})
  
  
  
  
  
  
  
  tmp %>%
    st_geometry() %>%
    st_equals(pv, sparse = F)
  
  
  tmp %>%
    st_geometry() %>%
    st_equals(ttmp, sparse = F) %>%
    apply(1,function(r){
      all(ttmp$id[r][1] == ttmp$id[r])
    }) %>% all()
})

which(!qq)



x<-rds[which(w[1,]),][tmp$origins[[1]],]

matrix(st_equals(tmp[1,],  x$geom %>% st_cast('POINT'),sparse = F),
          ncol=2) 
  

rdd$origins

o$geometry %>%
  



?st_intersection
getTDRoads<-function(specURL="http://static.data.gov.hk/td/traffic-speed-map/en/tsm_dataspec.pdf"){
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
  
  head(roadMetaData)
  
  tmrdmd<-roadMetaData %>%
    filter(district == "TM")
  
  
  
  dd<-roadMetaData %>%
    select(id=start,x=start_E,y=start_N,id=end,x=end_E,y=end_N) 
  
  dd<-rbind(dd[,1:3],dd[,4:6]) %>%
    by_row(..f = function(r){
      st_point(as.numeric(r[2:3])) #%>%
    }, .to = "geom") %>%
    mutate(id = as.character(id)) %>%
    select(id, geom) %>%
    st_sf() %>%
    st_set_crs(2326)
  
  w<-dd %>%
    st_equals(sparse=F) %>%
    apply(1,function(r){
      dd$id[which(r)]
    }) %>%
    setNames(dd$id)
  
  n<-which(!sapply(1:length(w),function(i){
    all(names(w)[i] == w[[i]])
  }))
  
  p<-sapply(n,function(i){
    unique(w[[i]][which(names(w)[i] != w[[i]])])
  }) %>%
    setNames(names(w)[n])
  
  #824251.1 832940.8
  library(igraph)
  m<-matrix(0,ncol=length(unique(names(p))),
         nrow=length(unique(names(p))))
  colnames(m) <- unique(names(p))
  rownames(m) <- unique(names(p))
  lapply(1:nrow(m),function(i){
    m[i,which(p[rownames(m)[i]] == colnames(m))] <<- 1
  })
  m[10,which(p[rownames(m)[10]] == colnames(m))] 
  
  g<-graph_from_adjacency_matrix(m,mode="undirected",diag=F)
  plot(g, vertex.size = 0.1, 
       vertex.label.cex = 0.5,
       edge.width = 1.5)
  
  comp<- components(g)
  
  pp<-lapply(lapply(which(comp$csize > 0),function(i){
    names(comp$membership)[which(comp$membership == i)]
  }),function(r){
    dd %>%
      filter(id %in% r)
  })
  
  
  road_sf<-roadMetaData %>%
    by_row(..f = function(r){
      st_linestring(as.matrix(rbind(as.numeric(r[3:4]),r[6:7])))
    }, .to = "geom") %>%
    mutate(start = as.character(start), end = as.character(end)) %>%
    select(route,start,end,district,route_type, geom) %>%
    st_sf() %>%
    st_set_crs(2326)
  
  roadMetaData
  i<-30
  road_sf %>%
    filter(start %in% pp[[i]]$id | end %in% pp[[i]]$id)
  road_sf %>%
    filter(start %in% pp[[i]]$id | end %in% pp[[i]]$id) %>%
    mapview() 
  pp[[i]]$id

  
  
  V(g)[which(degree(g) > 1)]$name
  
  induced.subgraph(g,vids=V(g)[which(degree(g) > 1)])
  
  sapply(p,function(pp){
    pp %in% names(p)
  })
  
  
  dd[dd$id == "7891",]
  
  
  w[n]
  dd %>%
    filter(id %in% n)
  w$`7881`
  which(names(w) == "7881")
  w[93]
  w['7881']
  n
  w[n]
  
  unique(n)
  
  dd[dd$id == "781",]
  
  w[which(names(w) == "787")]
  
  startp<-roadMetaData %>%
    select(id = start, x = start_E, y = start_N) %>%
    by_row(..f = function(r){
      st_point(as.numeric(r[2:3])) #%>%
    }, .to = "geom") %>%
    mutate(id = as.character(id)) %>%
    select(id, geom) %>%
    st_sf() %>%
    st_set_crs(2326)
  
  endp<-roadMetaData %>%
    select(id = end, x = end_E, y = end_N) %>%
    by_row(..f = function(r){
      st_point(as.numeric(r[2:3])) #%>%
    }, .to = "geom") %>%
    mutate(id = as.character(id)) %>%
    select(id, geom) %>%
    st_sf() %>%
    st_set_crs(2326)
  
  endp %>% 
    filter(!id %in% startp$id) %>%
    rbind(startp) %>%
    mapview()
  

  stp<-startp %>%
    filter(id %in% endp$id) %>%
    arrange(id)

  startp %>%
    filter(id %in% endp$id) %>%
    duplicated(id)
  startp[duplicated(startp$id),]$id
  duplicated(startp$id)
  
  names(which(!sapply(startp$id[which(duplicated(startp$id))],function(i){
    startp %>% 
      filter(id == i) %>%
      st_equals(sparse = F) %>%
      all()
  })))
  
  startp %>% 
    filter(id == "877") %>%
    st_equals(sparse = F)
  
  startp %>% 
    filter(id == id)
  
  
  startp[duplicated(startp$id),] %>%
    st_equals(sparse=F)
  
  length(stp$id)
  edp<-endp %>%
    filter(id %in% startp$id) %>%
    arrange(id)
  
  stp<-startp[which(startp$id %in% endp$id),]
  edp<-endp[which(endp$id %in% startp$id),]
  length(unique(startp$id))
  length(startp$id)
  
  
  
  st_equals(stp$geom,edp$geom,sparse = F)  
  ?st_equals
  startp %>%
    filter(id %in% endp$id) %>%
    st_equals(geom, endp$geom)

  
  startp[1,2]  
  
  library(purrrlyr)
  roadMetaData %>%
     by_row(..f = function(r){
      st_point(c(r[3],r[4])) 
    })
    # rowwise() %>%
    # st_point(c(start_E, start_N))
  
  st_point(c(roadMetaData$start_E[1], roadMetaData$start_N[1]))
  ?mutate
  roadMetaData %>%
    select(end,end_E, end_N)
  
  # if(require("sp",character.only = T, quietly = T)){
  #   roads<-apply(roadMetaData,1,function(r){
  #     m<-matrix(as.numeric(c(r[3],r[4],
  #                            r[6],r[7])), nrow = 2, byrow =T)
  #     m<-data.frame(m)
  #     sp::coordinates(m) <- ~X1+X2
  #     sp::SpatialLines(list(sp::Lines(list(sp::Line(m)), r[1])),sp::CRS("+init=epsg:2326")) ## the projection is HK1980 Grid
  #   })
  #   roads<-do.call(rbind,roads)
  #   roads<-sp::SpatialLinesDataFrame(roads,roadMetaData[,c("route","district","route_type")])
  #   roads<-sp::spTransform(roads,sp::CRS("+init=epsg:4326")) ## change the projection to 4326 for plotting
  #   roads
  # }else{
  #   NA
  # }
}

roads<-getTDRoads() ## get roads
