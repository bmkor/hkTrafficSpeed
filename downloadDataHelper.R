# require(xml2)
# require(dplyr)
require(httr)
# require(fst)

require(tictoc)
require(curl)
require(parallel)
require(jsonlite)


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
                      spURL="http://resource.data.one.gov.hk/td/speedmap.xml",
                      fileList=NULL,
                      file_cnt=NULL){
  datalistAPIURL<-url
  g<-httr::GET(datalistAPIURL,query = list(start=start,end=end,url=spURL))
  r <- if (is.null(fileList)) c() else fileList
  if (g$status_code == 200){
    tryCatch({
      p<-httr::content(g,"parsed")
      all_cnt<-if(is.null(file_cnt)) p$`version-count` else file_cnt 
      ts<-unlist(p$timestamps)
      d_cnt<-length(ts)
      r<-unique(c(r,ts))
      r_cnt = all_cnt - d_cnt 
      if(r_cnt > 0){
        nstart=unlist(strsplit(ts[length(ts)],"-"))[1]
        r<-getDataList(nstart,end, 
                       fileList = r, file_cnt = r_cnt)
      } else {
        r
      }
    }, error = function(c){
      print(error)      
    } 
    )  
  } else{
    print(g$status_code)
  }
}
r<-getDataList(start = start, end = end)

data_downloader<-function(start,
                          end,
                          dir="pData/",
                          url="https://api.data.gov.hk/v1/historical-archive/get-file",
                          spURL="http://resource.data.one.gov.hk/td/speedmap.xml"){
  ts<-getDataList(start=start,end=end)
  pool<-new_pool(total_con = 10000, host_con = 100, multiplex = T)
  
  cb <- function(req,fname){
    if (req$status == 200){
      tryCatch({
        write(rawToChar(req$content),file=paste0(dir,fname,".xml"))
        # write.fst(toDF(rawToChar(req$content)),paste0(dir,fname,".fst"),100)  
      }, error = function(cond){
        print("error entered")
        print(error)
        return(req$url)
      })
    } else{
      print("http status <> 200")
      req$url
    }
  }
  # try mclapply?
  sapply(paste0(url,"?","url=",spURL,"&","time=",ts),function(u){
    fname=unlist(strsplit(u,"time="))[2]
    curl_fetch_multi(u,done=function(req){cb(req,fname)},
                     pool=pool )
  })
  multi_run(pool=pool)
}

tic("download")
data_downloader(start="20190225",end="20190331") 
## download: 1354.441 sec elapsed for 25151 files (not using write.fst)
toc()

