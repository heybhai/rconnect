#install.packages('simsalapar')
library(simsalapar)
library(readxl)
newfile<- read_xlsx("E:/Machine Learning A-Z/metastate.csv/Offer name & ID.xlsx")
head(newfile)
htmlString <- mergedata$value.x
cleanfun <- function(htlmString){return(gsub("<.*?>", "", htmlString))}
t <- cleanfun(htmlString)


# library(RCurl)
# library(XML)
# input <- "http://www.creditcardcompare.com.au/"
# txt <- htmlToText(input)
# txt  
tt<- grepl("span",t)
sum(tt) #if 0 successful else gsub it
ttt<- gsub("^([ \t\n\r\f\v]+)|([ \t\n\r\f\v]+)$", "", t)
ttt
tttt<- gsub("\n","",t)
tttt

#l <- lazydata[!apply(lazydata == "", 1, all),]

#a <- htmlString[!(grepl(" ",htmlString))]

a <- mergedata[!(grepl(" ",merged))]

set.seed(1)
df <- as.data.frame(
  `names<-`(
    replicate(4, sample(LETTERS, 1e6, rep=T), simplify=F),
    letters[1:4]
  )
)

rows <- which(rowSums(as.matrix(mergedata$value.x) == "<span>") > 0)
df[head(rows), ]
rows

#rows <- which(rowSums(as.matrix(mergedata) == "U") > 0)


mergedata[grepl("pts", mergedata$value.x),] -> sick
mergedata[grepl("points", mergedata$value.x),] -> sick2

gsub(".*?([0-9]+).*", "\\1", mergedata[1:100,c("value.x")])
extract_numeric(mergedata[1:13,c("value.x")])
mergedata[which(mergedata[,as.numeric(mergedata$epc)]> 0),]
sum(as.numeric(mergedata$epc))
colnames(mergedata)
min(which(as.numeric(mergedata$epc) >0))

gc()
sapply(data2, class)
sapply(data1,class)
sapply(mergedata, class)

#unique epc
unepc <- unique(mergedata$epc)
unepcnum <- as.numeric(levels(unepc))[unepc] #factors to numeric coersion


unepc_old <- unique(data2$epc)
unepc_oldnum <- as.numeric(levels(unepc_old))[unepc_old]

cc<-unique(mergedata_copy$pageviewid)

mergedata_copy <- mergedata
mergedata_copy$epc <- as.numeric(levels(mergedata_copy$epc))[mergedata_copy$epc]
mergedata_copy$epc[is.na(mergedata_copy$epc)] <- 0
unepc_cpy <- unique(mergedata_copy$epc)
#mergedata_copy$epc_fac<- if(mergedata_copy$epc > 0){mergedata_copy$epc_fac =1} else {mergedata_copy$epc_fac =0}
mergedata_copy$epc_fac <-mergedata_copy$epc
mergedata_copy$epc_fac[mergedata_copy$epc_fac > 0] <- 1
mergedata_copy$epc_fac[mergedata_copy$epc_fac == 0] <- 0
mergedata_copy$epc_fac <- factor(mergedata_copy$epc_fac)
sapply(mergedata_copy, class)
waste<- subset(mergedata_copy, epc_fac == 0)
good<- subset(mergedata_copy, epc_fac != 0)

#finding epc per pageview for mergedata
length(unique(good$pageviewid))
tt<- ddply(good, .(pageviewid), nrow)
goodmerge <- merge(good,tt,by = c("pageviewid"))
goodmerge$realepc <- goodmerge$epc/goodmerge$V1
sum(goodmerge$realepc) #1861.9 (around 3% of original epc)

#test of adding offername and offerid as sent in 3rd file seperately

gdrepcsrt<-goodmerge %>% group_by(offer.y) %>% summarise(sum(realepc), na.rm = TRUE)
gdrepcsrt <- arrange(gdrepcsrt, desc(gdrepcsrt$`sum(realepc)`, na.rm = TRUE))

gdrepcsrt_mul<-goodmerge %>% group_by(offer.y,date) %>% summarise(sum(realepc), na.rm = TRUE)
#by revenue
gdrepcsrt_mul <- arrange(gdrepcsrt_mul, desc(gdrepcsrt_mul$`sum(realepc)`, na.rm = TRUE))
#by date
gdrepcsrt_mul <- arrange(gdrepcsrt_mul, desc(gdrepcsrt_mul$date, na.rm = TRUE))




#finding epc per pageview for spot data
length(unique(data2$pageviewid))
ttt<- ddply(mergedata_copy, .(pageviewid), nrow)
spotmerge <- merge(mergedata_copy,ttt,by = c("pageviewid"))
spotmerge$epc <- as.numeric(levels(spotmerge$epc))[spotmerge$epc]
spotmerge$epc[is.na(spotmerge$epc)] <- 0
spotmerge$realepc <- spotmerge$epc/spotmerge$V1
sum(spotmerge$realepc) # 69573.72

gdrepcsrt_mul<-goodmerge %>% group_by(offer.y,date) %>% summarise(sum(realepc), na.rm = TRUE)
#by revenue
gdrepcsrt_mul <- arrange(gdrepcsrt_mul, desc(gdrepcsrt_mul$`sum(realepc)`, na.rm = TRUE))
#by date
gdrepcsrt_mul <- arrange(gdrepcsrt_mul, desc(gdrepcsrt_mul$date, na.rm = TRUE))

dt1<- data.table(mergedata_copy, key= 'pageviewid')
dt2<- data.tab

# pgvepc<-mergedata_copy %>% group_by(pageviewid) %>% summarise(sum(epc), na.rm = TRUE)
-# pgvepc <- arrange(pgvepc, desc(pgvepc$`sum(epc)`, na.rm = TRUE))
# 
# 
# pgvepc_g<-good %>% group_by(pageviewid) %>% summarise(sum(epc))
# pgvepc_g <- arrange(pgvepc_g, desc(pgvepc_g$`sum(epc)`))
# 
# pgvepc_g_mul<-good %>% group_by(pageviewid,date) %>% summarise(sum(epc))
# pgvepc_g_mul <- arrange(pgvepc_g, desc(pgvepc_g$`sum(epc)`))
# 
# suspicious<- good[good$pageviewid ==  '435747556a694892b6db152eeae09c14',]


data_3001 <- read.csv("E:/Machine Learning A-Z/metastate.csv/api_metastate.csv/spot_010118-140118.csv")
summary(data_3001)
colnames(data_3001)
unique(data_3001$Visitor.Type..GA.)
