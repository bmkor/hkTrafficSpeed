### download roads' specification, required packages pdftools, sp
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
  roadMetaData<-read.csv(tmp,sep=" ",stringsAsFactors = TRUE)
  rownames(roadMetaData) <- roadMetaData$route
  
  if(require("sp",character.only = T, quietly = T)){
    roads<-apply(roadMetaData,1,function(r){
      m<-matrix(as.numeric(c(r[3],r[4],
                             r[6],r[7])), nrow = 2, byrow =T)
      m<-data.frame(m)
      sp::coordinates(m) <- ~X1+X2
      sp::SpatialLines(list(sp::Lines(list(sp::Line(m)), r[1])),sp::CRS("+init=epsg:2326")) ## the projection is HK1980 Grid
    })
    roads<-do.call(rbind,roads)
    roads<-sp::SpatialLinesDataFrame(roads,roadMetaData[,c("route","district","route_type")])
    roads<-sp::spTransform(roads,sp::CRS("+init=epsg:4326")) ## change the projection to 4326 for plotting
    roads
  }else{
    NA
  }
}

roads<-getTDRoads() ## get roads

pal<- colorFactor(rainbow(length(levels(roads$district))),
                  levels = levels(roads$district)) ## for legend

require(leaflet)
map <- leaflet() %>%
  addTiles()

map %>%
  addPolylines(data=roads,
               color=~pal(district),
               popup=~route) %>%
  addLegend("topright", 
            pal = pal, 
            values = roads$district,
            title="District") ## plot roads on a map

