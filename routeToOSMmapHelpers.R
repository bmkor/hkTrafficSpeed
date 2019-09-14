createRoadGraph<-function(roads){
  roads %>%
    st_touches(.,.) %>%
    as.matrix() %>%
    provideDimnames(base=list(as.vector(roads$route),as.vector(roads$route))) %>%
    graph_from_adjacency_matrix(.,mode="undirected",diag = F,
                                add.rownames = "route")  %>%
    set_vertex_attr("roadnames",value=roads$routename) %>%
    set_vertex_attr("geometry",value=roads$geometry)
}

getShortestPathFromGraph<-function(startR,endR,g){
  shortest_paths(g,V(g)[V(g)$name == startR],
                 V(g)[V(g)$name == endR])$vpath
}

extractMotorNetworkWithinBBOX<-function(bbox){
  net<-osmdata::opq (bbox) %>%
    osmdata::add_osm_feature (key = "highway") %>%
    osmdata::osmdata_sf(quiet = T) %>%
    osmdata::osm_poly2line() 
  if (nrow (net$osm_lines) == 0){
    stop ("Street network unable to be downloaded")  
  }
  net$osm_lines %>%
    weight_streetnet(wt_profile = "motorcar") %>%
    by_row(..f = function(r){
      st_linestring(matrix(rbind(c(r$from_lon,r$from_lat),c(r$to_lon,r$to_lat)),nrow=2)) 
    },.to="geometry") %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    st_transform(2326)  
}

getStreetnet<-function(kkbox, kr){
  dodgr_streetnet(matrix(kbbox,nrow=2,dimnames = list(c("x","y"),c("min","max"))),expand=0) %>%
    weight_streetnet(wt_profile = "motorcar") %>%
    mutate(name=unlist(sapply(way_id, function(id){
      n <- kr[which(kr$osm_id == id),]$name
      if(length(n) == 0){
        NA
      } else{
        n
      }
    }))) %>%
    by_row(..f = function(r){
      st_linestring(matrix(rbind(c(r$from_lon,r$from_lat),c(r$to_lon,r$to_lat)),nrow=2)) 
    },.to="geometry") %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    st_transform(2326)
} 
findHighwaysWithinBBox<-function(bbox){
  opq('China') %>%
    add_osm_feature(key='highway', 
                    bbox = bbox) %>%
    osmdata_sf() ## Take quite a while  
}
routeParition<-function(ppts,l,pstart=T,pend=T){
  m<-apply(st_distance(ppts,l),1,which.min) %>%
    matrix(ncol=2,byrow=T)
  r<-m %>%
    cbind(.,seq(1,nrow(ppts),by=2)) %>%
    apply(.,1,function(r){
      ll<- l[r[1]:r[2],]
      cc<- ll %>% 
        st_coordinates()
      hh<-st_nearest_points(ppts[r[3],],l[r[1],]) %>%
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
    })
  if(!pstart){
    ll <- l[m[1,1],]
    cc<-r[[1]] %>%
      st_coordinates() %>%
      st_zm
    cc[1,] <- (ll %>%
                 st_coordinates() %>%
                 st_zm)[1,]
    hh<-as.vector((ll %>%
                     st_coordinates() %>%
                     st_zm)[1,])
    r[[1]]<-r[[1]] %>%
      mutate(start_E = hh[1], start_N = hh[2]) %>%
      st_drop_geometry() %>%
      st_set_geometry(st_linestring(cc) %>% st_geometry()) %>%
      st_set_crs(2326)
  }
  if(!pend){
    ll <- l[m[nrow(m),1]:m[nrow(m),2],]
    cc<-r[[length(r)]] %>%
      st_coordinates() %>%
      st_zm
    cc[nrow(cc),]  <- (ll %>%
                         st_coordinates() %>%
                         st_zm)[(ll %>%
                                   st_coordinates() %>%
                                   nrow),]
    ee<-as.vector((ll %>%
                     st_coordinates() %>%
                     st_zm)[(ll %>%
                               st_coordinates() %>%
                               nrow),])
    r[[length(r)]]<-r[[length(r)]] %>%
      mutate(end_E = ee[1], end_N = ee[2]) %>%
      st_drop_geometry() %>%
      st_set_geometry(st_linestring(cc) %>% st_geometry()) %>%
      st_set_crs(2326)
  }
  r
}
findShortestRoute<-function(d,from,to){
  d %>%
    dodgr_paths(.,from=from,to=to,vertices=F) %>%
    list.flatten() %>%
    unlist() %>%
    d[.,]
}
findOSMPathsWithinBBOX<-function(d,bbox){
  d %>%
    st_intersects(.,bbox,sparse = F) %>%
    as.vector() %>%
    d[.,]
}

viewOSMPathsWithinBBOX<-function(d,bbox,color="red"){
  d %>%
    st_intersects(.,bbox,sparse = F) %>%
    as.vector() %>%
    d[.,] %>% mapview(color=color)
}

nearestSegment<-function(ppts,d,k=1){
  ppts %>%
    st_nn(.,d,k=k) %>%
    unlist() %>%
    d[.,]
}

findEndpts<-function(l){
  l %>%
    st_cast("POINT") %>%
    st_touches(.,l,sparse=F) %>%
    rowSums() %>%
    equals(.,1) %>%
    which() %>%
    (l %>%
       st_cast("POINT"))[.,]
}
findAllpts<-function(l){
  l %>%
    st_cast("POINT") 
}
findBBOXpolygon<-function(l,xmin=0,ymin=0,xmax=0,ymax=0){
  ((l %>%
       st_bbox()) + c(xmin,ymin,xmax,ymax)) %>%
     st_as_sfc
}
findBBOX<-function(l,xmin=0,ymin=0,xmax=0,ymax=0){
  ((l %>%
      st_bbox()) + c(xmin,ymin,xmax,ymax))
}
filterRoutes<-function(routes,l){
  l %>% 
    filter(route %in% routes)
}
viewPath<-function(l,dk){
  l %>%
    mapview() + findEndpts(l) + nearestSegment(findEndpts(l),dk)
}
routeCreateInOneShot<-function(l,dk,from,to,pstart=T,pend=T){
  findShortestRoute(dk,from=from,to=to) %>%
    routeParition(findAllpts(l),.,pstart = pstart, pend = pend)
}
viewOneShotTry<-function(l,dk,from,to,pstart = pstart, pend = pend){
  routeCreateInOneShot(l,dk,from=from,to=to,pstart = pstart, pend = pend) %>%
    mapview() + findAllpts(l)
}






