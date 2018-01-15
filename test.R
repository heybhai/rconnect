#calling essential libraries
library(XML)
library(rjson)
library(jsonlite)
library(data.table)
library(splitstackshape)
library(stringr)
library(plyr)
library(dplyr)

#reading the benefit file
data2 <-
  read.csv("E:/Machine Learning A-Z/metastate.csv/api_metastate.csv/pageviews.csv")

#checking for missing value
table(is.na(data2)) #not found
#EDA
dim(data2) #checking dimension of dataa
colnames(data2) #checking names of the columns
summary(data2) #taking summary of the data

#reading the file generated
data1 <-
  read.csv("E:/Machine Learning A-Z/metastate.csv/api_metastate.csv/api_metastate.csv")
#checking for missing value
table(is.na(data1)) #not found
#EDA
dim(data1) #checking dimension of data
colnames(data1) #checking names of the columns

#creating a merged data set based on pageviewid
mergedata <- merge(data1, data2, by = c("pageviewid"))
#EDA
dim(mergedata) #checking dimension of data
colnames(mergedata) #checking names of the columns

#Analysis

apply(mergedata, 2, function(x)
  length(unique(x))) #finding unique in each column

#computing sum of epc per pageview
mg1 <-
  mergedata %>% group_by(pageviewid) %>% summarise(sum(as.numeric(unlist(epc)), na.rm = TRUE))
mg1 <-
  arrange(mg1, desc(mg1$`sum(as.numeric(unlist(epc)), na.rm = TRUE)`))

#computing no of pageview per sessions
length(unique(mergedata$pageviewid)) / length(unique(mergedata$sessionid)) 

#comparing epc for desktop vs mobile/tabs
mg2 <-
  mergedata %>% group_by(is_desktop) %>% summarise(sum(as.numeric(unlist(epc)), na.rm = TRUE))
mg2 <- arrange(mg2,desc(mg2$`sum(as.numeric(unlist(epc)), na.rm = TRUE)`,na.rm=TRUE))


#computing sum of epc per campaign
mg3 <-
  mergedata %>% group_by(campaign) %>% summarise(sum(as.numeric(unlist(epc)), na.rm = TRUE))
mg3 <- arrange(mg3, desc(mg3$`sum(as.numeric(unlist(epc)), na.rm = TRUE)`))
#max were not associated to any campaign

#computing sum of epc per campaign
mg4 <-
  mergedata %>% group_by(deleted) %>% summarise(sum(as.numeric(unlist(epc)), na.rm = TRUE))
mg4 <- arrange(mg4, desc(mg4$`sum(as.numeric(unlist(epc)), na.rm = TRUE)`))

#computing avg no of different cards per pageview
length(unique(mergedata$id.x) / length(unique(mergedata$pageviewid)))

       