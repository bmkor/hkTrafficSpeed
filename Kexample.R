require(tidygraph)

kroads %>%
  filter(degree <= 2) %>% mapview(zcol="degree")

sg<-induced.subgraph(g,V(g)[V(g)$name %in% (kroads %>% filter(degree <= 2) %>% pull(route))])
sg<-as_tbl_graph(sg)

sg<-sg %>%
  mutate(group=group_components(type="strong")) %>%
  select(-geometry,geometry) 


ll<-lapply(1:(sg %>% 
  pull(group) %>%
  max),function(i){
    kroads %>%
      filter(route %in% (sg %>% 
               filter(group==i) %>%
               pull(route))    )
  }) 


######## try the first one (the hardest one) ########

seqR<-ll[[1]] %>%
  pull(routename) %>%
  strsplit(.,"--") %>%
  unlist() %>%
  unique %>%
  Filter(function(x){!equals(x,"NA")},.)


endpts<-ll[[1]] %>%
  st_cast("POINT") %>%
  st_touches(.,ll[[1]],sparse=F) %>%
  rowSums() %>%
  equals(.,1) %>%
  which() %>%
  (ll[[1]] %>%
     st_cast("POINT"))[.,]

endpts %>%
  mapview() + ll[[1]]

endpts<-endpts[2:1,]

nearestSegment(endpts,dk) %>% 
  mapview(color="red") + ll[[1]] + endpts

from <- "6718595514"
to <- "5922666071" ### failed to output our desired path

#### Try other end point first
ll[[1]] %>%
  st_cast("POINT") %>%
  mapview() + ll[[1]]

nearestSegment((ll[[1]] %>%
                  st_cast("POINT") %>%
                  filter(row_number() == 15)), dk) %>% mapview(color="red") + ll[[1]] + (ll[[1]] %>%
                                                                                           st_cast("POINT") %>%
                                                                                           filter(row_number() == 15) )
to <-"1029866898"
findShortestRoute(dk,from,to) %>% 
  mapview() + ll[[1]]

l1<-findShortestRoute(dk,from,to)


#### missing a turn on the west
from <- "1029866898"
to <- "5922666071" 
findShortestRoute(dk,from,to) %>% 
  mapview() %>% addExtent((ll[[1]] %>%
                                         st_cast("POINT") %>%
                                         filter(row_number() %in% c(31,2)) %>%
                                         st_bbox() %>% st_as_sfc))  #+ ll[[1]] + l1

#### try to find out such turn if exists

tdk<-ll[[1]] %>%
  st_cast("POINT") %>%
  filter(row_number() %in% c(31,2)) %>%
  st_bbox() %>%
  getStreetnet(.)


#### wow, such turn actually exists!
tdk %>%
  st_intersects(.,(ll[[1]] %>%
                     st_cast("POINT") %>%
                     filter(row_number() %in% c(31,2)) %>%
                     st_bbox() %>% st_as_sfc),sparse=F) %>%
  unlist() %>%
  as.vector() %>%
  which() %>%
  tdk[.,] %>% mapview(color="red") + ll[[1]]

#### extract it
from <- "5137163530"
to <- "339051678"

findShortestRoute(tdk,from,to) %>%
  mapview(color ="red") + ll[[1]]

#### save it up as l3
l3 <-findShortestRoute(tdk,from,to)

#### find the route in the middle
from <- "1029866898"
to <- "5137163530"

findShortestRoute(dk,from,to) %>% 
  mapview(color="red") + l3 + l1 + ll[[1]]

#### save it as l2
l2 <-findShortestRoute(dk,from,to)

rou<-routeParition((ll[[1]] %>% st_cast("POINT")), rbind(l1,l2,l3)) %>%
  mapview() + (ll[[1]] %>% st_cast("POINT"))

#### 16 routes path
rou

######## try the second one  ########

ll[[2]] %>%
  mapview() + findEndpts(ll[[2]]) + nearestSegment(findEndpts(ll[[2]]),dk) + 
  findAllpts(ll[[2]])[4,] + nearestSegment(findAllpts(ll[[2]])[4,],dk)

ll[[2]] %>%
  pull(routename) %>%
  strsplit(.,"--") %>%
  unlist() %>%
  as.vector() %>%
  Filter(function(x){!equals(x,"NA")},.) %>%
  unique()

id1<-"4346833956"
id2<-"2878204585"
id3<-"3178645499"
findShortestRoute(dk,from=id1,to=id2) %>%
  mapview() + ll[[2]] + findShortestRoute(dk,from=id2,to=id3)

findShortestRoute(dk,from=id2,to=id3) ### still no path, let's check if such path exists

ll[[2]] %>%
  mapview() + findBBOXpolygon(ll[[2]],xmax=150,ymax=50)
  
dk %>%
  st_intersects(.,findBBOXpolygon(ll[[2]],xmax=150,ymax=50),sparse=F) %>%
  as.vector() %>%
  dk[.,] %>% mapview(color="red") + ll[[2]] #### OH! a broken segement along Prince Edward Road East



dkk<-dodgr_streetnet(matrix((findBBOXpolygon(ll[[2]],xmax=150,ymax=50) %>%
                              st_transform(4326) %>%
                              st_bbox()),nrow=2,dimnames = list(c("x","y"),c("min","max"))),expand=0) %>%
  weight_streetnet(wt_profile = "motorcar") %>% 
  by_row(..f = function(r){
  st_linestring(matrix(rbind(c(r$from_lon,r$from_lat),c(r$to_lon,r$to_lat)),nrow=2)) },.to="geometry") %>%
  st_as_sf() %>%
  st_set_crs(4326) %>%
  st_transform(2326)

dkk %>%
  st_intersects(.,findBBOXpolygon(ll[[2]],xmax=150,ymax=50),sparse=F) %>%
  as.vector() %>%
  dkk[.,] %>% mapview(color="red") + ll[[2]] #### YES! a broken segement along Prince Edward Road East fixed

tmprou<-findShortestRoute(dkk,from=id1,to=id3) ### got the path

routeParition(findAllpts(ll[[2]]),tmprou) %>% 
  mapview() + findAllpts(ll[[2]])

tmprou<-routeParition(findAllpts(ll[[2]]),tmprou)

rou<-c(rou,tmprou)


######## try the third one  ########
ll[[3]] %>%
  mapview() + findEndpts(ll[[3]]) + nearestSegment(findEndpts(ll[[3]]),dk)

findShortestRoute(dk,from="474589537",to="317358529") %>%
  mapview() + ll[[3]] ### one shot!

findShortestRoute(dk,from="474589537",to="317358529") %>%
  routeParition(findAllpts(ll[[3]]),.) %>%
  mapview() + findAllpts(ll[[3]])

tmprou<-findShortestRoute(dk,from="474589537",to="317358529") %>%
  routeParition(findAllpts(ll[[3]]),.) ## valid

rou<-c(rou,tmprou)
######## try the forth  ########
viewPath(ll[[4]],dk)

viewOneShotTry(ll[[4]],dk,from = "274244459" ,to="582486959")  
tmprou<-routeCreateInOneShot(ll[[4]],dk,from = "274244459" ,to="582486959")
rou<-c(rou,tmprou)

######## try the fifth  ########
viewPath(ll[[5]],dk)

viewOneShotTry(ll[[5]],dk,from="5390102149",to="473886634")
tmprou<-routeCreateInOneShot(ll[[5]],dk,from="5390102149",to="473886634")
rou<-c(rou,tmprou)

######## try the sixth  ########
i<-6
viewPath(ll[[i]],dk)
from<-"675264494"
to<-"582596629"
viewOneShotTry(ll[[i]],dk,from=from,to=to) + ll[[i]]
tmprou<-routeCreateInOneShot(ll[[i]],dk,from=from,to=to)
rou<-c(rou,tmprou)

######## try the seventh  ########
i<-7
viewPath(ll[[i]],dk)
from<-"804106892"
to<-"6357916383"
viewOneShotTry(ll[[i]],dk,from=from,to=to) + ll[[i]]
tmprou<-routeCreateInOneShot(ll[[i]],dk,from=from,to=to)
rou<-c(rou,tmprou)

######## try the eighth  ########
i<-8
viewPath(ll[[i]],dk)
from<-"582593262"
to<-"1168380660"
viewOneShotTry(ll[[i]],dk,from=from,to=to) + ll[[i]] + kroads
tmprou<-routeCreateInOneShot(ll[[i]],dk,from=from,to=to)
rou<-c(rou,tmprou)

######## try the nintth  ########
i<-9
viewPath(ll[[i]],dk)
from<-"34971941"
to<-"5026755149"
viewOneShotTry(ll[[i]],dk,from=from,to=to) + ll[[i]] + kroads
tmprou<-routeCreateInOneShot(ll[[i]],dk,from=from,to=to)
rou<-c(rou,tmprou)

######## try the tenth  ########
i<-10
viewPath(ll[[i]],dk)
from<-"582593419"
to<-"" ### trick on the end point
findEndpts(ll[[i]])  %>%
  mapview() ## note the position on the 2nd point

findEndpts(ll[[i]])[2,] %>%
  nearestSegment(.,dk,k=3) %>% mapview() + findEndpts(ll[[i]])[2,] ### should be the one in N-S direction!
to<-"587637085"
viewOneShotTry(ll[[i]],dk,from=from,to=to) + ll[[i]]
tmprou<-routeCreateInOneShot(ll[[i]],dk,from=from,to=to)
rou<-c(rou,tmprou)

######## try the eleventh  ########
i<-11
viewPath(ll[[i]],dk)
from<-"506448281"
to<-"297910474"
viewOneShotTry(ll[[i]],dk,from=from,to=to) + ll[[i]]
tmprou<-routeCreateInOneShot(ll[[i]],dk,from=from,to=to)
rou<-c(rou,tmprou)

######## try the twelfth  ########
i<-12
viewPath(ll[[i]],dk) #### we don't know the direction along Lam Hing Street 
viewPath(ll[[i]],dk)  + findBBOXpolygon(ll[[i]],xmin=-20,xmax=50,ymin=-20, ymax=20) 

dk %>%
  st_intersects(.,findBBOXpolygon(ll[[i]],xmin=-20,xmax=50,ymin=-20, ymax=20),sparse = F) %>%
  as.vector() %>%
  dk[.,] %>% st_transform(2326) %>% mapview(color="red") + ll[[i]]

from<- "4066408596" #"4724086393"
to<-"1933640152"
viewOneShotTry(ll[[i]],dk,from=from,to=to) + ll[[i]] ### ? weird result

dk %>%
  dodgr_paths(.,from=from,to=to) ### wrong orientation

#### manually create...
from<- "4724086393"
to<-"1933640152"

viewOneShotTry(ll[[i]],dk,from=from,to=to) + ll[[i]] ### second part

dk %>%
  findShortestRoute(.,from="4066408596",to=from) %>%
  mapview() + ll[[i]] + (dk %>%
                           findShortestRoute(.,from=from,to=to))

tmp<-dk %>%
  findShortestRoute(.,from="4066408596",to=from) %>%
  rbind(.,(dk %>%
             findShortestRoute(.,from=from,to=to)))

tmp %>%
  mapview() + ll[[i]]

routeParition(findAllpts(ll[[i]]),tmp) %>%
  mapview() + findAllpts(ll[[i]])

tmprou<-routeParition(findAllpts(ll[[i]]),tmp)
rou<-c(rou,tmprou)

######## try the thirteenth  ########
i<-13
viewPath(ll[[i]],dk)
from<-"" ### illegitimate
to<-"5195347142"
nearestSegment(findEndpts(ll[[i]])[1,],dk,k=3) %>%
  mapview() + ll[[i]]
from<-"5086938880"
viewOneShotTry(ll[[i]],dk,from=from,to=to) + ll[[i]]
tmprou<-routeCreateInOneShot(ll[[i]],dk,from=from,to=to)
rou<-c(rou,tmprou)

######## try the forteenth  ########
i<-14
viewPath(ll[[i]],dk)
from<-"" ### illegitimate: too short
to<-"5195347142"
nearestSegment(findEndpts(ll[[i]]),dk)[1,] %>%
  mapview() + nearestSegment(findEndpts(ll[[i]]),dk)[2,] ## the segment already is the route
routeParition(findAllpts(ll[[i]]),nearestSegment(findEndpts(ll[[i]]),dk)[1,]) %>%
  mapview() + findAllpts(ll[[i]]) ## confirmed

tmprou<-routeParition(findAllpts(ll[[i]]),nearestSegment(findEndpts(ll[[i]]),dk)[1,])
rou<-c(rou,tmprou)

######## try the fifteenth  ########
i<-15
viewPath(ll[[i]],dk) ### too short again
nearestSegment(findEndpts(ll[[i]]),dk)[1,] %>%
  mapview() + nearestSegment(findEndpts(ll[[i]]),dk)[2,] ## the segment already is the route

routeParition(findAllpts(ll[[i]]),nearestSegment(findEndpts(ll[[i]]),dk)[1,]) %>%
  mapview() + findAllpts(ll[[i]]) ## confirmed

tmprou<-routeParition(findAllpts(ll[[i]]),nearestSegment(findEndpts(ll[[i]]),dk)[1,])
rou<-c(rou,tmprou)

######## try the last one  ########
i<-16
viewPath(ll[[i]],dk) ### which one is pointing to?
from<-"334650682"
nearestSegment(findEndpts(ll[[i]])[2,],dk, k=3) %>% mapview() + ll[[i]] + kroads ### ah, find it
to<-"1172166428"

viewOneShotTry(ll[[i]],dk,from=from,to=to) + ll[[i]]
tmprou<-routeCreateInOneShot(ll[[i]],dk,from=from,to=to)
rou<-c(rou,tmprou)

######## combine ########
kroutes<-rou %>%
  do.call(rbind,.)

kroads %>%
  filter(!route %in% kroutes$route) %>%
  mapview(color="red") + kroutes


####### try those with degree > 2 #######

kroads %>%
  filter(degree > 2) %>% mapview(zcol="degree")

sg<-induced.subgraph(g,V(g)[V(g)$name %in% (kroads %>% filter(degree > 2) %>% pull(route))])
sg<-as_tbl_graph(sg)

sg<-sg %>%
  mutate(group=group_components(type="strong")) %>%
  select(-geometry,geometry) 


ll2<-lapply(1:(sg %>% 
                pull(group) %>%
                max),function(i){
                  kroads %>%
                    filter(route %in% (sg %>% 
                                         filter(group==i) %>%
                                         pull(route))    )
                }) 

####### try 1 #######
i<-1

ll2[[i]] %>%
  mapview(color="red") + findBBOXpolygon(ll2[[i]]) + findAllpts(ll2[[i]]) + kroutes


dk %>%
  st_intersects(.,findBBOXpolygon(ll2[[i]],ymax=50),sparse = F) %>%
  as.vector() %>%
  dk[.,] %>% st_transform(2326) %>% mapview(color="red") + ll2[[i]] + findAllpts(ll2[[i]]) + kroutes

#### route<-"34214-34211"
#### from<-"998476250"
#### to<-"473886513

from<-"998476250"
to<-"473886513"

ll2[[i]] %>%
  filter(route == "34214-34211")
  
dodgr_paths(dk,from=from, to = to, vertices = F) %>%
  unlist() %>%
  dk[.,] %>% mapview() + ll2[[i]]


tmp<-dodgr_paths(dk,from=from, to = to, vertices = F) %>%
  unlist() %>%
  dk[.,]

tmp[1,] %>%
  mapview() + ll2[[i]] + kroutes[47,]
 
cc<-tmp %>%
  st_geometry() %>%
  st_coordinates() %>%
  st_zm

cc[1,]<-(kroutes[47,]%>%
  st_coordinates() %>%
  st_zm)[(kroutes[47,]%>%
            st_coordinates() %>% nrow),]

#### create "34214-34211"
  
ll2[[i]] %>%
  filter(route == "34214-34211") %>%
  select(route:route_type,degree) %>%
  mutate(start_E = cc[1,1], start_N = cc[1,2],
         start_N = cc[nrow(cc),1], end_N = cc[nrow(cc),2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(st_linestring(cc) %>% st_geometry()) %>%
  st_set_crs(2326) %>% mapview(color="red") + ll2[[i]] + kroutes


tmprou<-ll2[[i]] %>%
  filter(route == "34214-34211") %>%
  select(route,district,route_type,degree,start_E,start_N,end_E,end_N) %>%
  mutate(start_E = cc[1,1], start_N = cc[1,2],
         start_N = cc[nrow(cc),1], end_N = cc[nrow(cc),2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(st_linestring(cc) %>% st_geometry()) %>%
  st_set_crs(2326) %>%
  list("34214-34211" = .)

#### create c("3363-3369","3369-3370","3370-34211")
dk %>%
  st_intersects(.,findBBOXpolygon(ll2[[i]],ymax=50),sparse = F) %>%
  as.vector() %>%
  dk[.,] %>%
  mapview(color="red") + ll2[[i]] + tmprou$`34214-34211`

from<-"1933640127"
to<-"473886513"

dk %>% 
  findShortestRoute(.,from,to) %>% mapview()
    
viewOneShotTry(ll2[[i]] %>%
                 filter(route %in% c("3363-3369","3369-3370","3370-34211")),dk,
               from,to,pstart=T,pend=F) + (kroutes %>% mapview(color="red")) + tmprou$`34214-34211`

routeCreateInOneShot(ll2[[i]] %>%
                       filter(route %in% c("3363-3369","3369-3370","3370-34211")),dk,
                     from,to,pstart=T,pend=F) %>% mapview() + (kroutes %>% mapview(color="red")) + tmprou$`34214-34211`


tmprou$`3369-34211` <- routeCreateInOneShot(ll2[[i]] %>%
                                              filter(route %in% c("3363-3369","3369-3370","3370-34211")),dk,
                                            from,to,pstart=T,pend=F)
tmprou$`3369-34211`<-do.call(rbind,tmprou$`3369-34211`)

dk %>%
  st_intersects(.,findBBOXpolygon(ll2[[i]],ymax=50),sparse = F) %>%
  as.vector() %>%
  dk[.,] %>%
  mapview(color="red") + ll2[[i]] + tmprou$`34214-34211` + tmprou$`3369-34211` + findAllpts(ll2[[i]]) + kroutes

#### create "34211-33681"

from<-"473886513"
to<-"473886150"

viewOneShotTry(ll2[[i]] %>%
                 filter(route %in% c("34211-33681")),dk,
               from,to,pstart=F,pend=T) + (kroutes %>% mapview(color="red"))



tmprou$`34211-33681` <- do.call(rbind,routeCreateInOneShot(ll2[[i]] %>%
                                              filter(route %in% c("34211-33681")),dk,
                                            from,to,pstart=F,pend=T))


viewOSMPathsWithinBBOX(dk,findBBOXpolygon(ll2[[i]],ymax=50)) + 
  findAllpts(ll2[[i]] %>% filter(!route %in% do.call(rbind,tmprou)$route)) + 
  kroutes + (ll2[[i]] %>% filter(!route %in% do.call(rbind,tmprou)$route)) + do.call(rbind,tmprou)

##### create c("34212-33701","33701-3369","3369-33691")

from<-"473886313"
to<-"4724086393"

viewOneShotTry(ll2[[i]] %>%
                 filter(route %in% c("34212-33701","33701-3369","3369-33691")),dk,
               from,to,pstart=F,pend=T) + (kroutes %>% mapview(color="red"))

tmprou$`34212-33691` <- do.call(rbind,routeCreateInOneShot(ll2[[i]] %>%
                                                             filter(route %in% c("34212-33701","33701-3369","3369-33691")),dk,
                                                           from,to,pstart=F,pend=T))

##### create "34212-34213"
from<-"473886313"
to<-"473886811"
troute<-filterRoutes("34212-34213", ll2[[i]])
pstart<-F
pend<-T
  
viewOneShotTry(troute,dk,from,to,pstart=pstart,pend=pend) + (kroutes %>% mapview(color="red"))

tmprou$`34212-34213` <- routeCreateInOneShot(troute,dk,
                                               from,to,pstart=pstart,pend=pend) %>% extract2(1)

##### create "3690-3643"
from<-"5086938875"
to<-"473886072"
troute<-filterRoutes("3690-3643", ll2[[i]])
pstart<-T
pend<-F

viewOneShotTry(troute,dk,from,to,pstart=pstart,pend=pend) + (kroutes %>% mapview(color="red"))
tmprou$`3690-3643` <- routeCreateInOneShot(troute,dk,from,to,
                                           pstart=pstart,pend=pend) %>% extract2(1)


##### create "88810-34212"
from<-"5086938881"
to<-"473886313"
troute<-filterRoutes("88810-34212", ll2[[i]])
pstart<-T
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart=pstart,pend=pend) + (kroutes %>% mapview(color="red"))

tmprou$`88810-34212`<-routeCreateInOneShot(troute,
                                           dk,from,to,
                                           pstart=pstart,pend=pend) %>% extract2(1)

##### create "33681-3643"
from<-"473886149"
to<-"473886072"
troute<-filterRoutes("33681-3643", ll2[[i]])
pstart<-T
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart=pstart,pend=pend) + (kroutes %>% mapview(color="red"))

tmprou$`33681-3643` <- routeCreateInOneShot(troute,
                                            dk,from,to,
                                            pstart=pstart,pend=pend) %>% extract2(1)


##### create "3643-88813"
from<-"473886072"
to<-"3178708593"
troute<-filterRoutes("3643-88813", ll2[[i]])
pstart<-F
pend<-T

viewOneShotTry(troute,dk,from,to,
               pstart=pstart,pend=pend) + (kroutes %>% mapview(color="red"))

tmprou$`3643-88813` <- routeCreateInOneShot(troute,
                                            dk,from,to,
                                            pstart=pstart,pend=pend) %>% extract2(1)


# kroutesbk<-kroutes
kroutes<-rbind(kroutes,do.call(rbind,tmprou))


####### try 2 #######
i<-2
#tmprou<-list()
#tmproubk<-tmprou

#### take a look
viewOSMPathsWithinBBOX(dk,
                       findBBOXpolygon(ll2[[i]],xmin=-10,xmax=10,ymin=-10,ymax=10)) +
  ll2[[i]] + findAllpts(ll2[[i]]) + (kroutes %>% mapview(color="black"))

#### create "888303-3453"
from<-"582593415"
to<-"582593420"
troute<-filterRoutes("888303-3453",ll2[[i]])
pstart<-F
pend<-T

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red"))

tmprou$`888303-3453` <- routeCreateInOneShot(troute,
                                              dk,from,to,
                                              pstart=pstart,pend=pend) %>% extract2(1)

#### create "88827-888303"
from<-"582486959"
to<-"582593415"
troute<-filterRoutes("88827-888303",ll2[[i]])
pstart<-F
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red"))

tmprou$`88827-888303` <- routeCreateInOneShot(troute,
                                             dk,from,to,
                                             pstart=pstart,pend=pend) %>% extract2(1)

#### create "888302-888303"
from<-"582593329"
to<-"582593415"
troute<-filterRoutes("888302-888303",ll2[[i]])
pstart<-F
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red"))

tmprou$`888302-888303` <- routeCreateInOneShot(troute,
                                              dk,from,to,
                                              pstart=pstart,pend=pend) %>% extract2(1)


#### create "34554-888302" a difficult one
from<- "582593297" #"582593297" # "582593293"
to<- "2772111062" #"582593411 "
troute<-filterRoutes("34554-888302",ll2[[i]])
pstart<-F
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) +
  (dk %>% filter(from_id=="2772111062" & to_id=="582593329")) +
  (dk %>% filter(from_id == "582593296" & to_id == "582593297")) + 
  (dk %>% filter(from_id == "582593293" & to_id == "582593296"))
  

### "582593293" ->"582593296" ->  "582593297" -> "2772111062" -> "582593329"
coo<-(dk %>% filter(from_id == "582593293" & to_id == "582593296") %>% st_coordinates() %>% st_zm) %>%
  rbind(.,(dk %>% filter(from_id == "582593296" & to_id == "582593297")) %>% st_coordinates() %>% st_zm) %>%
  rbind(.,(routeCreateInOneShot(troute,
                               dk,from,to,
                               pstart=pstart,pend=pend) %>% extract2(1)) %>% st_coordinates() %>% st_zm) %>%
  rbind(.,(dk %>% filter(from_id=="2772111062" & to_id=="582593329"))%>% st_coordinates() %>% st_zm)
  
tmp<-routeCreateInOneShot(troute,
                          dk,from,to,
                          pstart=pstart,pend=pend) %>% extract2(1)

tmprou$`34554-888302` <-tmp %>%
  mutate(start_E = coo[1,1], start_N = coo[1,2],
         end_E = coo[nrow(coo),1], end_N = coo[nrow(coo),2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)
 

#### create "3480-888302" 
from<- "582596632"
to<- "582593410"

### break up again "582596632" -> "582593410" -> "582593329"
troute<-filterRoutes("3480-888302",ll2[[i]])
pstart<-F
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) +
  (dk %>% filter(from_id=="582593410" & to_id=="582593329"))

coo<-(routeCreateInOneShot(troute,
                           dk,from,to,pstart,pend) %>% extract2(1) %>% st_coordinates() %>% st_zm) %>%
  rbind(.,(dk %>% filter(from_id=="582593410" & to_id=="582593329")) %>% st_coordinates() %>% st_zm)

tmp<-routeCreateInOneShot(troute,
                          dk,from,to,
                          pstart=pstart,pend=pend) %>% extract2(1)

tmprou$`3480-888302` <-tmp %>%
  mutate(end_E = coo[nrow(coo),1], end_N = coo[nrow(coo),2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)


#### create "34551-34554"
from<-"6720228561"
to<-"582593293"
troute<-filterRoutes("34551-34554",ll2[[i]])
pstart<-T
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red"))

tmprou$`34551-34554`<-routeCreateInOneShot(troute,
                                           dk,from,to,
                                           pstart=pstart,pend=pend) %>% extract2(1)
#### create "34554-34501"
from<-"5026755152"
to<-"5119898372"

#### break up again: "582593293" ->  "5026755152" -> "5119898372"
troute<-filterRoutes("34554-34501",ll2[[i]])
pstart<-F
pend<-T

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) + do.call(rbind,tmprou) +
  (dk %>% filter(from_id == "582593293" & to_id == "5026755152"))


coo<-((dk %>% filter(from_id=="582593293" & to_id == "5026755152")) %>% st_coordinates() %>% st_zm)  %>%
  rbind(.,(routeCreateInOneShot(troute,
                                dk,from,to,pstart,pend) %>% extract2(1) %>% st_coordinates() %>% st_zm))

tmp<-routeCreateInOneShot(troute,
                          dk,from,to,
                          pstart=pstart,pend=pend) %>% extract2(1)

tmprou$`34554-34501` <-tmp %>%
  mutate(start_E = coo[1,1], start_N = coo[1,2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)


#### create "3480-88818"
from<-"582596632"
to<-"311730996"
troute<-filterRoutes("3480-88818",ll2[[i]])
pstart<-F
pend<-T

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) + do.call(rbind,tmprou)

tmprou$`3480-88818`<-routeCreateInOneShot(troute,
                          dk,from,to,
                          pstart=pstart,pend=pend) %>% extract2(1)

#### create "88827-34552"
from<-"582486959"
to<-"582593263"
troute<-filterRoutes("88827-34552",ll2[[i]])
pstart<-F
pend<-T
viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) + do.call(rbind,tmprou)

tmprou$`88827-34552`<-routeCreateInOneShot(troute,
                                          dk,from,to,
                                          pstart=pstart,pend=pend) %>% extract2(1)

#### create "88812-88827"
from<-"582486958"
to<-"582486959"
troute<-filterRoutes("88812-88827",ll2[[i]])
pstart<-T
pend<-F
viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) + do.call(rbind,tmprou)

tmprou$`88812-88827`<-routeCreateInOneShot(troute,
                                           dk,from,to,
                                           pstart=pstart,pend=pend) %>% extract2(1)

#### create "4643-3480"
from<-"1189559486"
to<-"582596632"
troute<-filterRoutes("4643-3480",ll2[[i]])
pstart<-T
pend<-F
viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) + do.call(rbind,tmprou)

tmprou$`4643-3480`<-routeCreateInOneShot(troute,
                                           dk,from,to,
                                           pstart=pstart,pend=pend) %>% extract2(1)

#kroutes<-rbind(kroutes,do.call(rbind,tmprou))


####### try 3 #######
i<-3
# tmprou<-list()
# tmproubk<-tmprou

#### take a look
viewOSMPathsWithinBBOX(dk,
                       findBBOXpolygon(ll2[[i]],xmin=-10,xmax=10,ymin=-10,ymax=10)) +
  ll2[[i]] + findAllpts(ll2[[i]]) + (kroutes %>% mapview(color="black"))

#### create "888221-3010"
from<-"334650748"
to<-"334649353"
troute<-filterRoutes("888221-3010",ll2[[i]])
pstart<-T
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) 

tmprou$`888221-3010` <- routeCreateInOneShot(troute,
                                             dk,from,to,
                                             pstart=pstart,pend=pend) %>% extract2(1)

#### create "3491-3010"
from<-"5104193274"
to<-"334649353"
troute<-filterRoutes("3491-3010",ll2[[i]])
pstart<-T
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) 
  
tmprou$`3491-3010` <- routeCreateInOneShot(troute,
                                               dk,from,to,
                                               pstart=pstart,pend=pend) %>% extract2(1)

#### create "3010-888301"
from<-"334649353"
to<-"334649360"  # "334649361"
#### break up again: "334649353" -> "334649360" -> "334649445" -> "2861666783" -> "2861666780" -> "334649361"
troute<-filterRoutes("3010-888301",ll2[[i]])
pstart<-F
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + 
  (kroutes %>% mapview(color="red"))  + 
  viewOneShotTry(troute,dk,from=to,to="334649445",
                 pstart,pend) +
  (dk %>% filter(from_id == "334649445" & to_id == "2861666783")) +
  (dk %>% filter(from_id == "2861666783" & to_id == "2861666780")) +
  (dk %>% filter(from_id == "2861666780" & to_id == "334649361"))

coo <- (routeCreateInOneShot(troute,
                            dk,from,to,
                            pstart=pstart,pend=pend) %>% extract2(1) %>% st_coordinates() %>% st_zm ) %>%
  rbind(.,(routeCreateInOneShot(troute,dk,from=to,to="334649445",
                          pstart,pend) %>% extract2(1) %>% st_coordinates() %>% st_zm)) %>%
  rbind(.,((dk %>% filter(from_id == "334649445" & to_id == "2861666783")) %>% st_coordinates() %>% st_zm)) %>%
  rbind(.,((dk %>% filter(from_id == "2861666783" & to_id == "2861666780")) %>% st_coordinates() %>% st_zm)) %>%
  rbind(.,((dk %>% filter(from_id == "2861666780" & to_id == "334649361")) %>% st_coordinates() %>% st_zm))
           

tmp<-routeCreateInOneShot(troute,
                          dk,from,to,
                          pstart=pstart,pend=pend) %>% extract2(1)

tmprou$`3010-888301` <-tmp %>%
  mutate(end_E = coo[nrow(coo),1], end_N = coo[nrow(coo),2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)

 
#### create "888301-3692"
from<-"334649361"
to<-"1311402294"

### break up again: "334649361" -> "334649360" -> "1311402294" 
troute<-filterRoutes("888301-3692",ll2[[i]])
pstart<-F
pend<-T

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red"))  + tmprou$`888301-3692`

tmprou$`888301-3692` <- routeCreateInOneShot(troute,
                                             dk,from,to,
                                             pstart=pstart,pend=pend) %>% extract2(1)


#### create "30069-888301"
from<-"5922666071"
to<-"339051707" # "334649361"

#### break up again: "5922666071" -> "339051707" -> "2861666784" -> "2861666781" -> "334649361"
troute<-filterRoutes("30069-888301",ll2[[i]])
pstart<-T
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + 
  (kroutes %>% mapview(color="red"))  + 
  (dk %>% filter(from_id == to & to_id == "2861666784")) +
  (dk %>% filter(from_id == "2861666784" & to_id == "2861666781")) +
  (dk %>% filter(from_id == "2861666781" & to_id == "334649361"))

coo <- (routeCreateInOneShot(troute,
                             dk,from,to,
                             pstart=pstart,pend=pend) %>% extract2(1) %>% st_coordinates() %>% st_zm ) %>%
  rbind(.,((dk %>% filter(from_id == to & to_id == "2861666784")) %>% st_coordinates() %>% st_zm)) %>%
  rbind(.,((dk %>% filter(from_id == "2861666784" & to_id == "2861666781")) %>% st_coordinates() %>% st_zm)) %>%
  rbind(.,((dk %>% filter(from_id == "2861666781" & to_id == "334649361")) %>% st_coordinates() %>% st_zm))


tmp<-routeCreateInOneShot(troute,
                          dk,from,to,
                          pstart=pstart,pend=pend) %>% extract2(1)

tmprou$`30069-888301` <-tmp %>%
  mutate(end_E = coo[nrow(coo),1], end_N = coo[nrow(coo),2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)

#### create "88801-36821"
from<-"4501979659"
to<-"1168380691"
troute<-filterRoutes("88801-36821",ll2[[i]])
pstart<-T
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) 

tmprou$`88801-36821` <- routeCreateInOneShot(troute,
                                           dk,from,to,
                                           pstart=pstart,pend=pend) %>% extract2(1)

#### create "36821-3491"
from<-"1168380708" #"1168380691"
to<-"1168315991"

#### break again:"1168380691" -> "1168380708" ->  "1168315991"
troute<-filterRoutes("36821-3491",ll2[[i]])
pstart<-F
pend<-T

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) + 
  (dk %>% filter(from_id == "1168380691" & to_id == "1168380708"))

coo <- ((dk %>% filter(from_id == "1168380691" & to_id == "1168380708")) %>% st_coordinates() %>% st_zm) %>%
  rbind(.,(routeCreateInOneShot(troute,
                                dk,from,to,
                                pstart=pstart,pend=pend) %>% extract2(1) %>% st_coordinates() %>% st_zm )) 

tmp<-routeCreateInOneShot(troute,
                          dk,from,to,
                          pstart=pstart,pend=pend) %>% extract2(1)

tmprou$`36821-3491` <-tmp %>%
  mutate(start_E = coo[1,1], start_N = coo[1,2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)


#### create "36821-34982"
from<-"6702444347" # "1168380691"
to<-"332248875"

#### break again:"1168380691" -> "6702444347" ->  "332248875"
troute<-filterRoutes("36821-34982",ll2[[i]])
pstart<-F
pend<-T

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) + 
  (dk %>% filter(from_id == "1168380691" & to_id == "6702444347"))

coo <- ((dk %>% filter(from_id == "1168380691" & to_id == "6702444347")) %>% st_coordinates() %>% st_zm) %>%
  rbind(.,(routeCreateInOneShot(troute,
                                dk,from,to,
                                pstart=pstart,pend=pend) %>% extract2(1) %>% st_coordinates() %>% st_zm )) 

tmp<-routeCreateInOneShot(troute,
                          dk,from,to,
                          pstart=pstart,pend=pend) %>% extract2(1)

tmprou$`36821-34982` <-tmp %>%
  mutate(start_E = coo[1,1], start_N = coo[1,2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)

#kroutesbk<-kroutes
#kroutes<-rbind(kroutes,do.call(rbind,tmprou))

####### try 4 #######
i<-4
# tmprou<-list()
# tmproubk<-tmprou

#### take a look
viewOSMPathsWithinBBOX(dk,
                       findBBOXpolygon(ll2[[i]],xmin=-10,xmax=10,ymin=-10,ymax=10)) +
  ll2[[i]] + findAllpts(ll2[[i]]) + (kroutes %>% mapview(color="black"))

#### create "88807-35071"
from<-"4860301263"
to<-"4860320025"
troute<-filterRoutes("88807-35071",ll2[[i]])
pstart<-F
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) 

tmprou$`88807-35071` <- routeCreateInOneShot(troute,
                                             dk,from,to,
                                             pstart=pstart,pend=pend) %>% extract2(1)

#### create "35071-3505"
from<-"4860320025"
to<-"804106896"
troute<-filterRoutes("35071-3505",ll2[[i]])
pstart<-F
pend<-T

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) 

tmprou$`35071-3505` <- routeCreateInOneShot(troute,
                                             dk,from,to,
                                             pstart=pstart,pend=pend) %>% extract2(1)

#### create "35071-3488"
from<-"4860320025"
to<-"804106579"
troute<-filterRoutes("35071-3488",ll2[[i]])
pstart<-F
pend<-T

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) 

tmprou$`35071-3488` <- routeCreateInOneShot(troute,
                                            dk,from,to,
                                            pstart=pstart,pend=pend) %>% extract2(1)


#### create "88807-3506"
from<-"804107601" #"4860301263" 
to<-"675264552"

#### break again: seems route "88807-3506" should be MAJOR-ROUTE
#### "4860301263" -> "804107601" -> "675264552"
troute<-filterRoutes("88807-3506",ll2[[i]])
pstart<-F
pend<-T

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) + 
  (dk %>% filter(from_id == "4860301263"  & to_id == "804107601"))

coo <- ((dk %>% filter(from_id == "4860301263" & to_id == "804107601")) %>% st_coordinates() %>% st_zm) %>%
  rbind(.,(routeCreateInOneShot(troute,
                                dk,from,to,
                                pstart=pstart,pend=pend) %>% extract2(1) %>% st_coordinates() %>% st_zm )) 

tmp<-routeCreateInOneShot(troute,
                          dk,from,to,
                          pstart=pstart,pend=pend) %>% extract2(1)

tmprou$`88807-3506` <-tmp %>%
  mutate(start_E = coo[1,1], start_N = coo[1,2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)


#### create "88806-88807"
from<-"4860301264"
to<-"4860301263"
troute<-filterRoutes("88806-88807",ll2[[i]])
pstart<-T
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) 

tmprou$`88806-88807` <- routeCreateInOneShot(troute,
                                            dk,from,to,
                                            pstart=pstart,pend=pend) %>% extract2(1)


all(names(tmprou) %in% ll2[[i]]$route)
any(names(tmprou) %in% kroutes$route)

#kroutesbk<-kroutes
#kroutes<-rbind(kroutes,do.call(rbind,tmprou))

####### try 5 #######
i<-5

# tmproubk<-tmprou
# tmprou<-list()

#### take a look
viewOSMPathsWithinBBOX(dk,
                       findBBOXpolygon(ll2[[i]],xmin=-10,xmax=10,ymin=-10,ymax=10)) +
  ll2[[i]] + findAllpts(ll2[[i]]) + (kroutes %>% mapview(color="black"))

#### create "3500-3499"
from<-"276372857"
to<-"1168380668"
troute<-filterRoutes("3500-3499",ll2[[i]])
pstart<-T
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) 

tmprou$`3500-3499` <- routeCreateInOneShot(troute,
                                             dk,from,to,
                                             pstart=pstart,pend=pend) %>% extract2(1)

#### create "3498-3499"
from<-"1168380686"
to<-"1168380668"
troute<-filterRoutes("3498-3499",ll2[[i]])
pstart<-T
pend<-F

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) 

tmprou$`3498-3499` <- routeCreateInOneShot(troute,
                                           dk,from,to,
                                           pstart=pstart,pend=pend) %>% extract2(1)

#### create "3499-88822"
from<-"1168380668"
to<-"334650745"
troute<-filterRoutes("3499-88822",ll2[[i]])
pstart<-F
pend<-T

viewOneShotTry(troute,dk,from,to,
               pstart,pend) + (kroutes %>% mapview(color="red")) 

tmprou$`3499-88822` <- routeCreateInOneShot(troute,
                                           dk,from,to,
                                           pstart=pstart,pend=pend) %>% extract2(1)


all(names(tmprou) %in% ll2[[i]]$route)
any(names(tmprou) %in% kroutes$route)

#kroutesbk<-kroutes
#kroutes<-rbind(kroutes,do.call(rbind,tmprou))

do.call(rbind,tmprou) %>%
  mapview(color="red") + (kroutes %>%
                            filter(!route %in% do.call(rbind,tmprou)$route)) 
###############

kroutes %>%
  mapview(color="black") + kpts + kroads

#### error found in route "46332-46522"
coo<-kroutes %>%
  filter(route == "46332-46522") %>%
  st_coordinates() %>% st_zm

coo<-coo[sort.int(coo[,2],index.return = T, decreasing = T)$ix,]
coo<-coo[-c(1,3,nrow(coo)-2,nrow(coo)),]

kroutes %>%
  filter(route == "46332-46522") %>%
  mutate(start_E = coo[1,1],start_N = coo[1,2],
         end_E = coo[nrow(coo),1], end_N = coo[nrow(coo),2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326) %>% mapview(color="red") + kroutes

kroutes2<-kroutes %>%
  filter(route != "46332-46522") %>%
  rbind(.,(kroutes %>%
             filter(route == "46332-46522") %>%
             mutate(start_E = coo[1,1],start_N = coo[1,2],
                    end_E = coo[nrow(coo),1], end_N = coo[nrow(coo),2]) %>%
             st_drop_geometry() %>%
             st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
             st_set_crs(2326)))

kroutes2 %>%
  mapview(color="black") + kpts

sapply(1:nrow(kroutes2),function(i){
  cc<-kroutes2[i,] %>%
    st_coordinates() %>% 
    st_zm 
  
  !all(apply(cc[-c(1,nrow(cc)),],1,function(x){paste0(x,collapse = "#")}) %>%
         table(.) %>%
         as.vector()  == 2)  
})

#### check degree
kroutes2 %>%
  st_touches(.,kroutes2, sparse = F) %>%
  rowSums()

#### convert to graph


#### correct route "46332-46522"
kroutes <- kroutes2

#### save data
write_sf(kroutes,"sfData/kroutes.csv", layer_options = "GEOMETRY=AS_WKT",delete_dsn=TRUE)





