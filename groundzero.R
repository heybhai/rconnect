install.packages('simsalapar')
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
