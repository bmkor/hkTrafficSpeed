require(sparklyr)
require(dplyr)

# minikube start --memory 8192 --cpus 4
# kubectl create serviceaccount spark
# kubectl create clusterrolebinding spark-role --clusterrole=edit --serviceaccount=default:spark --namespace=default
# cd $SPARK_HOME
# before building the image, may take a look at https://github.com/rstudio/sparklyr/issues/1525#issuecomment-404964718
# 
# ./bin/docker-image-tool.sh -m -t sparklyr build

spark_disconnect_all()
showConnections()
closeAllConnections()

cip<-system2("kubectl",args="cluster-info",stdout = T)[1]
pattern<-"https://((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?):[0-9]+"
mip<-paste0("k8s://",regmatches(cip,regexpr(pattern,cip)))

k8s_config<-spark_config_kubernetes(master = mip,
                        image="spark-r:sparklyr", 
                        driver = "spark-pi-driver",
                        conf = list(
                          "spark.executor.instances" = 1,
                          "spark.executor.memory" = "2g",
                          "spark.executor.cores" = "2",
                          "spark.kubernetes.driver.volumes.persistentVolumeClaim.checkpointpvc.mount.path" = "/checkpoint",
                          # "spark.kubernetes.driver.volumes.persistentVolumeClaim.checkpointpvc.mount.readOnly" = "true",
                          "spark.kubernetes.driver.volumes.persistentVolumeClaim.checkpointpvc.options.claimName" = "traffic-data-claim",
                          "spark.kubernetes.executor.volumes.persistentVolumeClaim.checkpointpvc.mount.path" = "/checkpoint",
                          # "spark.kubernetes.executor.volumes.persistentVolumeClaim.checkpointpvc.mount.readOnly" = "true",
                          "spark.kubernetes.executor.volumes.persistentVolumeClaim.checkpointpvc.options.claimName" = "traffic-data-claim",                          
                          "spark.jars.packages"="com.databricks:spark-xml_2.11:0.5.0"              
                        ))

k8s_config$sparklyr.log.console = TRUE
sc<-spark_connect(master=k8s_config$spark.master,
                  config=k8s_config)



df<- sc %>%
  spark_session() %>%
  invoke("read") %>%
  invoke("format","xml") %>%
  invoke("option","rowTag","jtis_speedmap") %>%
  invoke("load","/checkpoint/*")

sdf<-sdf_register(df,"trafficSPD")
head(sdf)

