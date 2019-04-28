require(xml2)
require(dplyr)
require(httr)
require(fst)
require(jsonlite)
require(tictoc)
require(curl)

### Convert XML to Data frame  
toDF<-function(content){
  tmp<-read_xml(content)
  xml_ns_strip(tmp)
  data.frame(
    id=xml_text(xml_find_all(tmp,"//LINK_ID")),
    region=xml_text(xml_find_all(tmp,"//REGION")),
    level=xml_text(xml_find_all(tmp,"//ROAD_SATURATION_LEVEL")),
    speed=xml_text(xml_find_all(tmp,"//TRAFFIC_SPEED")),
    date=xml_text(xml_find_all(tmp,"//CAPTURE_DATE"))
  )  
}

### Helper to get the list of timestamps available of the traffic speed map
getDataList<-function(start,
                      end,
                      url="https://api.data.gov.hk/v1/historical-archive/list-file-versions",
                      spURL="http://resource.data.one.gov.hk/td/speedmap.xml"){
  datalistAPIURL<-url
  GET(datalistAPIURL,query = list(start=start,end=end,url=spURL))  
}

conDownloadHelper<-function(start,
                            end,
                            dir="Data/",
                            url="https://api.data.gov.hk/v1/historical-archive/get-file",
                            spURL="http://resource.data.one.gov.hk/td/speedmap.xml"){
  l<-getDataList(start=start,end=end)
  ts<-fromJSON(content(l,"text"))
  ts<-ts$timestamps
  pool<-new_pool(total_con = 720000, host_con = 100, multiplex = T)
  cb <- function(req,fname){
    if (req$status == 200){
      tryCatch({
        write.fst(toDF(rawToChar(req$content)),paste0(dir,fname,".fst"),100)  
      }, error = function(cond){
        return(req$url)
      })
    } else{
      req$url
    }
  }
  sapply(paste0(url,"?","url=",spURL,"&","time=",ts),function(u){
    fname=unlist(strsplit(u,"time="))[2]
    curl_fetch_multi(u,done=function(req){cb(req,fname)},
                     pool=pool )
  })
  multi_run(pool=pool)
}

# tic("download") ##better: 3924.539 sec elapsed ~ 1 hour #3545.704, # 2329.938, 2138.423
# conDownloadHelper(start="20190225",end="20190331")
# toc()