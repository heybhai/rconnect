library(XML)
library(rjson)
library(jsonlite)
library(data.table)
#reading the benefit file
data2<- read.csv("E:/Machine Learning A-Z/metastate.csv/api_metastate.csv/pageviews.csv")

#checking for missing value
table(is.na(data2)) #not found
#EDA
dim(data2) #checking dimension of dataa
colnames(data2)
summary(data2)

#reading the file generated 
data1<- read.csv("E:/Machine Learning A-Z/metastate.csv/api_metastate.csv/api_metastate.csv")
#checking for missing value
table(is.na(data1)) #not found
#EDA
dim(data1) #checking dimension of data
colnames(data1)



mergedata <- merge(data1,data2,by=c("pageviewid"))
#EDA
dim(mergedata) #checking dimension of data
colnames(mergedata)
head(mergedata,2)

apply(mergedata,2,function(x)length(unique(x)))

