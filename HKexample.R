#### filter HK roads ####
hkroads<-roads %>%
  filter(district == "HK")

#### Look at the coverage
hkroads %>%
  mapview(color="red") + 
  findBBOX(hkroads, xmin = -300,ymax=10) + kroutes + findAllpts(hkroads)

#### extract OSM roads
hkr<-findHighwaysWithinBBox(findBBOXpolygon(hkroads, xmin = -300,ymax=10) %>%
                                       st_transform(4326) %>%
                                       st_bbox())$osm_lines %>%
  filter((highway %in% 
            c("motorway", "secondary", "primary", "junction", "trunk_link", "primary_link",
              "tertiary", "secondary_link", "trunk", "bridge", "track", "road", "motorway_link") | 
            junction == "roundabout")) %>%
  st_transform(2326)

hk<-findHighwaysWithinBBox(findBBOXpolygon(hkroads, xmin = -300,ymax=10) %>%
                             st_transform(4326) %>%
                             st_bbox())$osm_lines %>% st_transform(2326)

#### fill road names
hkroads<-hkroads %>%
  by_row(..f = function(r){
    r %>%
      st_cast("POINT") %>%
      st_nn(.,hkr) %>%
      unlist %>%
      hkr[.,] %>%
      select(name) %>%
      st_drop_geometry %>%
      unlist %>%
      paste(collapse = "--")    
  },.to = "routename", .collate = "cols") %>%
  select(-geometry, geometry) %>%
  st_sf()

#### visual check
hkroads %>%
  mapview()

#### get dodgr streetnet
hkk<-getStreetnet(findBBOXpolygon(hkroads, xmin = -300,ymax=10) %>%
               st_transform(4326) %>%
               st_bbox(), hkr)

#### create graph
hg<-createRoadGraph(hkroad)


#### visual check
hg %>% 
  plot(vertex.size=1,
       vertex.label.cex = 0.5,
       vertex.label.dist = 0.25,
       vertex.label.degree = pi/2,
       layout = layout_(g, as_tree()),
       asp=0)

#### let's check with degree
names(which(degree(hg) == 1)) #8 routes got degree 1

#### bind with kroads
hkroads<-hkroads %>%
  mutate(route=as.vector(route)) %>%
  by_row(..f=function(r){
    degree(hg)[r$route]
  },.to="degree",.collate="cols") %>%
  select(-geometry,geometry) %>%
  st_sf()

#### visual check
#### Typos in coordinates can be found, e.g. routes 50078-50079, 50079-50076

hkroads %>%
  mutate(deg = ifelse(degree == 1, "1", ">1")) %>%
  mapview(zcol="deg",burst=T) # 3-pair start/end 

hkk %>%
  mapview(color="red") + hkroads + findAllpts(hkroads)


### create routes "50179-50178"
from<-"1013426487"
to<-"5086900405" 
pstart<-F
pend<-F

tmpR<-hkroads %>%
  filter(route == "50179-50178")
viewOneShotTry(tmpR,hkk,from,to,pstart,pend)
tmprou$`50179-50178`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))

### create routes from "50178-50019" to "893-4652"
from<-"5086900405"
to<-"288176870"

pstart<-F
pend<-T

tmpR<-hkroads[sapply(c("50178-50019","50019-50018","50018-760","760-756","756-752",
                       "752-875","875-3402","3402-8979","8979-8978","8978-896","896-724","724-722","722-50059",
                       "50059-50058","50058-50045","50045-50049","50049-50048","50048-893","893-4652"),function(r){
                         which(hkroads$route == r)
                       }) %>% as.vector(),]

viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + findAllpts(tmpR) + tmprou$`50179-50178`
tmprou$`50178-4652`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))

### create routes "50099-787"
from<-"1013426166"  
to<-"4576848583"  #"587758984"
pstart<-T
pend<-F
#### needs break up: "1013426166" -> "4576848583" -> "587758984"
tmpR<-hkroads %>%
  filter(route == "50099-787")
#tmpR %>% mapview()
viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + 
  (hkk %>% filter(from_id == to  & to_id == "587758984"))

coo <- (routeCreateInOneShot(tmpR,
                             hkk,from,to,
                             pstart=pstart,pend=pend) %>% extract2(1) %>% st_coordinates() %>% st_zm ) %>%
  rbind(.,(hkk %>% filter(from_id == to  & to_id == "587758984")) %>% st_coordinates() %>% st_zm) 

tmp<-routeCreateInOneShot(tmpR,
                          hkk,from,to,
                          pstart=pstart,pend=pend) %>% extract2(1)

tmprou$`50099-787` <-tmp %>%
  mutate(end_E = coo[nrow(coo),1], end_N = coo[nrow(coo),2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)

### create routes from "4647-46478" to "50098-50099"
from<-"5087080145" 
to<-"2987721669"  
pstart<-T
pend<-T

tmpR<-hkroads %>%
  filter(route %in% 
           (getShortestPathFromGraph("4647-46478","50098-50099",hg) %>% 
              extract2(1) %>%
              names))
#tmpR %>% mapview()
viewOneShotTry(tmpR,hkk,from,to,pstart,pend) 
tmprou$`4647-50099`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))

### create route from "787-7882" to "7882-7891"
from<-"587758984" 
to<-"587759030"
pstart<-F
pend<-F

tmpR<-hkroads %>%
  filter(route %in% 
           (getShortestPathFromGraph("787-7882","7882-7891",hg) %>% 
              extract2(1) %>%
              names))
#tmpR %>% mapview()
viewOneShotTry(tmpR,hkk,from,to,pstart,pend)
tmprou$`787-7891`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))

tmprou$`787-7891`[1,] %>% 
  st_transform(4326) %>%
  st_coordinates()

### create routes "787-50179"
from<-"587758984"
to<-"1013426487"
pstart<-F
pend<-F

tmpR<-hkroads %>%
  filter(route == "787-50179")
#tmpR %>% mapview()
viewOneShotTry(tmpR,hkk,from,to,pstart,pend) 
tmprou$`787-50179`<-do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))


### create routes from "7891-789" to "910-931" Note "931" got different coordinates (compare to "931-4650")
from<-"587759030"
to<- "5086665653" #"2199258878" 
pstart<-F
pend<-F # "50109-910" 
### break "587759030" -> "5086665653" -> "2199258878"
tmpR<-hkroads[sapply(c("7891-789", "789-7908", "7908-7909", "7909-793",  "793-877","877-46498", "46498-4649", 
                      "4649-46499",  "46499-811", "811-818", "818-821", "821-50107","50107-50109","50109-910",
                      "910-931"),function(r){
                         which(hkroads$route == r)
                       }) %>% as.vector(),]

### typo on 910
coo<-tmpR[which(tmpR$end == 910),] %>% st_coordinates() %>% st_zm
coo[2,] <- (tmpR[which(tmpR$start == 910),] %>% st_coordinates() %>% st_zm)[1,]

tmpR[which(tmpR$end == 910),] <- tmpR[which(tmpR$end == 910),] %>%
  st_drop_geometry() %>% st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)

#tmpR %>% mapview()
viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + 
  findShortestRoute(hkk,from=to,to="2199258878")

tmp<-do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,
                                   pstart=pstart,pend=pend))

coo <- (tmp[nrow(tmp),] %>% st_coordinates() %>% st_zm ) %>%
  rbind(.,(findShortestRoute(hkk,from=to,to="2199258878")) %>% st_coordinates() %>% st_zm) 

tmp[nrow(tmp),]<-tmp[nrow(tmp),] %>%
  mutate(end_E = coo[nrow(coo),1], end_N = coo[nrow(coo),2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)

tmprou$`7891-931`<-tmp


#### create route "930-931"
from<- "5609888966" # "471196257"
to<-  "5086665660" #"2199258878" 
pstart<-F
pend<-F
#### break up: "471196257" -> "5609888966" -> "5086665660 -> "2199258878" 
tmpR<-hkroads %>% filter(route == "930-931")

#tmpR %>% mapview()
viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + 
  findShortestRoute(hkk,from="471196257", to =from) + 
  findShortestRoute(hkk,from=to, to ="2199258878" )

tmp<-routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend) %>% extract2(1)

coo<-findShortestRoute(hkk,from="471196257", to =from) %>% st_coordinates() %>% st_zm %>%
  rbind(., tmp %>% st_coordinates() %>% st_zm) %>%
  rbind(.,findShortestRoute(hkk,from=to, to ="2199258878" ) %>% st_coordinates() %>% st_zm)

tmprou$`930-931`<- tmp %>%
  mutate(start_E = coo[1,1],start_N = coo[1,2],
         end_E = coo[nrow(coo),1], end_N = coo[nrow(coo),2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)

### create routes from "931-4650" to "3651-4632"
from<-"2199258878"
to<-"5222118318"
pstart<-F
pend<-T

tmpR<-hkroads %>%
  filter(route %in% 
           (getShortestPathFromGraph("931-4650","3651-4632",hg) %>% 
              extract2(1) %>%
              names))
viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + findAllpts(tmpR) 
tmprou$`931-4632`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))

### create routes from "930-9101" to "786-7861"
from<-"6041004700" #"471196257"
to<-"1012751991"
pstart<-F
pend<-F

tmpR<-hkroads[sapply((getShortestPathFromGraph("930-9101", "786-7861",hg) %>% 
                        extract2(1) %>%
                        names),function(r){
                          which(hkroads$route == r)
                        }) %>% as.vector(),]

viewOneShotTry(tmpR,hkk,from,to,pstart=pstart,pend=pend) + findAllpts(tmpR) +do.call(rbind,tmprou) +
  
tmp <- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))

coo<-(findShortestRoute(hkk,"471196257",to=from) %>% st_coordinates() %>% st_zm) %>%
  rbind(.,(tmp[1,] %>% st_coordinates() %>% st_zm))

tmp[1,]<-tmp[1,] %>%
  mutate(start_E = coo[1,1],end_E=coo[1,2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)

tmprou$`930-7861`<- tmp

### create routes from "4648-46488" to "50079-50076"
from<-"2910511401"
to<-"1013426332" #tricky typo on the point 50079
pstart<-T
pend<-F

tmpR<-sapply(c("4648-46488","46488-762","762-50078",
               "50078-50079","50079-50076"),function(r){
                 which(hkroads$route == r)
               }) %>% as.vector() %>%
  hkroads[.,]

tmpR %>%
  filter(route == "50078-50079") %>%
  st_coordinates() %>% st_zm

tmpR %>%
  filter(route == "50079-50076") %>%
  st_coordinates() %>% st_zm

coo<-tmpR[which(tmpR$route == "50078-50079"),] %>%
  st_coordinates() %>% st_zm

coo[2,] <- (tmpR[which(tmpR$route == "50079-50076"),] %>%
  st_coordinates() %>% st_zm)[1,]

tmpR[which(tmpR$route == "50078-50079"),] <- tmpR[which(tmpR$route == "50078-50079"),] %>%
  st_drop_geometry() %>% st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)

viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + findAllpts(tmpR) 
tmprou$`4648-50076`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))

#### create route "50076-7881"
from<-"1013426332"
to<-"6233602195"
pstart<-F
pend<-F

tmpR<-hkroads %>%
  filter(route == "50076-7881")

viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + findAllpts(tmpR) 
tmprou$`50076-7881`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))


#### create route from "50076-7883" to "7883-7891"
from<-"1013426332"
to<-"587759030"
pstart<-F
pend<-F

tmpR<-hkroads[sapply(c("50076-7883","7883-7891"),function(r){
  which(hkroads$route == r)
}),]

viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + findAllpts(tmpR) 
tmprou$`50076-7891`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))

#### create route from "7861-7871" to "7871-7881"
from<-"1012751991"
to<-"6233602195"
pstart<-F
pend<-F

tmpR<-hkroads[sapply(c("7861-7871", "7871-7881"),function(r){
  which(hkroads$route == r)
}),]

viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + findAllpts(tmpR) 
tmprou$`7861-7881`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))


#### create route "7861-50179"
from<-"1012751991"
to<-"1013426487"
pstart<-F
pend<-F

tmpR<-hkroads %>% 
  filter(route == "7861-50179")

viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + findAllpts(tmpR) 
tmprou$`7861-50179`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))

#### create route from "787-788" to "868-870"
from<-"587758984"
to<-"4577535120"
pstart<-F
pend<-F

tmpR<-hkroads[sapply(getShortestPathFromGraph("787-788","868-870",hg) %>% 
  extract2(1) %>%
  names,function(r){
    which(hkroads$route == r)
  }),]


viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + findAllpts(tmpR) 
tmprou$`787-870`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))


#### create route "7881-870"
from<-"6233602200" # "6233602195" #"6233602201"
to<-"587640060" #"4577535120"
pstart<-F
pend<-F
#### breakup : "6233602195" -> "6233602201" -> "6233602200" -> "587640060" -> "4577535120"
tmpR<-hkroads %>% 
  filter(route == "7881-870")

viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + findAllpts(tmpR) +
  findShortestRoute(hkk,from="6233602195",to="6233602201") + 
  findShortestRoute(hkk,from="6233602201",to="6233602200") +
  findShortestRoute(hkk,to,"4577535120") + do.call(rbind,tmprou)
  
coo <- (findShortestRoute(hkk,from="6233602195",to="6233602201") %>% st_coordinates() %>% st_zm) %>%
  rbind(.,(findShortestRoute(hkk,from="6233602201",to="6233602200"))%>% st_coordinates() %>% st_zm) %>%
  rbind(.,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend) %>% extract2(1) %>% st_coordinates() %>% st_zm) %>%
  rbind(.,findShortestRoute(hkk,to,"4577535120") %>% st_coordinates() %>% st_zm)

tmprou$`7881-870` <- routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend) %>% extract2(1) %>%
  mutate(start_E = coo[1,1], start_N = coo[2,1],
         end_E = coo[nrow(coo),1], end_N = coo[nrow(coo),2]) %>%
  st_drop_geometry() %>%
  st_set_geometry(coo %>% st_linestring() %>% st_geometry()) %>%
  st_set_crs(2326)

#### create route from "870-4651" to "4651-4631"
from<-"4577535120"
to<-"4862617493"
pstart<-F
pend<-T

tmpR<-hkroads[sapply(c("870-4651", "4651-4631"),function(r){
  which(hkroads$route == r)
}),]

viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + findAllpts(tmpR) 
tmprou$`870-4631`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))

#### create route from "5011-50119" to "50119-930"
from<-"3005253122"
to<-"471196257"
pstart<-T
pend<-F

tmpR<-hkroads[sapply(c("5011-50119", "50119-930"),function(r){
  which(hkroads$route == r)
}),]

viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + findAllpts(tmpR) 
tmprou$`5011-50119`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))



#### create route "4652-4633"
from<-"288176867"
to<-"1311402667"
pstart<-T
pend<-T

tmpR<-hkroads %>% 
  filter(route == "4652-4633")

viewOneShotTry(tmpR,hkk,from,to,pstart,pend) + findAllpts(tmpR) 
tmprou$`4652-4633`<- do.call(rbind,routeCreateInOneShot(tmpR,hkk,from,to,pstart,pend))

do.call(rbind,tmprou)%>% 
  mapview(color = "red") + #(hkroads %>% filter(!route %in% do.call(rbind,tmprou)$route)) + 
  (kroutes %>% mapview(color="black"))

hkroads$route %in%do.call(rbind,tmprou)$route

hkroutes <- do.call(rbind,tmprou)


#### check degree

hkroutes[which(hkroutes %>%
  st_touches(.,hkroutes, sparse = F) %>%
  rowSums() == 1),] %>% 
  mapview(color="red") + hkroutes

### bugs found on routes: "50078-50079", "50109-910" ## typo on 50079 and 910
lapply(hkroutes[which(hkroutes %>% st_touches(.,hkroutes, sparse = F) %>% rowSums() == 1),]$route,
       function(r) { hkroutes[which(hkroutes$route == r),]}) %>% mapview() + (hkroutes %>% mapview(color="black"))

kroutes
hkroutes

write_sf(hkroutes,"sfData/hkroutes.csv", layer_options = "GEOMETRY=AS_WKT",delete_dsn=TRUE)
