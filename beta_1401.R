memory.limit(size = 50000)

library(XML)
library(rjson)
library(jsonlite)
library(data.table)
library(simsalapar)
library(readxl)
library(cluster)
library(ggplot2)
library(splitstackshape)
library(stringr)
library(plyr)
library(dplyr)
library(httr)
library(lubridate)
library(libcurl)
library(h2o)

localH2o <- h2o.init(nthreads = -1) #doesn't work with Java 9 , try 8 or lower version
h2o.init(ip ='localhost', port =54321, nthreads = -1, max_mem_size =  '4g')

query <- "https://sdfhjjnpgc.execute-api.ap-southeast-2.amazonaws.com/prod/api/metafields/?"
getdata<-GET(url=query, add_headers(Authorization="Token 8e70597d2eec1c4cdb0a1f260a1756e3d24396d8")) %>% stop_for_status() %>% json_parse
data_api <- getdata$results

next_page <- getdata$'next'
while(!is.null(next_page)){
  more_data <- GET(next_page,add_headers(Authorization="Token 8e70597d2eec1c4cdb0a1f260a1756e3d24396d8")) %>% stop_for_status() %>% json_parse
  data_api <- rbind(data_api, more_data$results)
  next_page <- more_data$'next'
  Sys.sleep(2)
}

data2<- read.csv("E:/Machine Learning A-Z/metastate.csv/api_metastate.csv/pageviews.csv")
data3<- data2[!apply(data2 == "", 1, all),] # removes empty rows from data2
data4 <- data3[-(grep("[a-zA-Z]",data3$id)), ] #removes characters from id 
gc()

data_table_1 <- data.table(data_api, key= "pageviewid")
data_table_2 <- data.table(data4, key= "pageviewid")
system.time(dt.merged <- merge(data_table_1, data_table_2))
gc()

dt.merged$epc <- as.numeric(levels(dt.merged$epc))[dt.merged$epc]
gc()
dt.merged$epc[is.na(dt.merged$epc)] <- 0
gc()


# split the data 80% train/20% test
splitdata<- sample.int(n=nrow(dt.merged),size=floor(.8*nrow(dt.merged)),replace = F)
data_train <- dt.merged[splitdata, ]
data_test <- dt.merged[-splitdata, ]
gc()
