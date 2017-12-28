#load libraries
 library(plyr)
 library("arules")
 library(readr)

#load data
#This file has two columns inidividual_merchant and inidividual_customer
 input <- read_csv("E:/Machine Learning A-Z/data.csv")

#Get the list of merchants/items
 merchant <- unique(input$individual_merchant)
 merchant <- merchant[order(merchant)]
 target_merchants <- merchant
 sno <- 1:length(target_merchants)
 merchant_ident <- cbind(target_merchants,sno)

#Create a reference mapper for all merchant
 colnames(merchant_ident) <- c("individual_merchant","sno")

# Create a correlation matrix for these merchants
 correlation_mat = matrix(0,length(merchant),length(target_merchants))
 correlation_mat = as.data.frame(correlation_mat)
 trans = read.transactions("E:/Machine Learning A-Z/data.csv", format = "single", sep = ",", cols =
                              c("inidividual_customer", "individual_merchant"))
 c <- crossTable(trans)
 rowitem <- rownames(c)
 columnitem <- colnames(c)
 correlation_mat <- c[order(as.numeric(rowitem)),order(as.numeric(columnitem))]
 for(i in 1:9822
     ) {
  correlation_mat[i,] <- correlation_mat[i,]/correlation_mat[i,i]
}
 colnames(correlation_mat) <- target_merchants
 rownames(correlation_mat) <- merchant

# Now let's start recommending for individual customer
 possible_slots <- 20
 avail <- 21
 merch_rec <- matrix(0, nrow = length(target_customers), ncol = avail)
 merch_rec[,1] <- unique(input3$Cust_map)
 correlation_mat <- as.matrix(correlation_mat)
 position <- 1
 for (i in 1:length(target_customers)) {
  been_thr <- input[position : (position + customer_merch_ct[i] - 1),'individual_merchant']
  merging <- as.data.frame(merchant_ident[merchant_ident[,'individual_merchant'] %in%          been_thr,])
  corel_subset <- correlation_mat[merging$sno,]
  will_go <- colSums(corel_subset)
  will_go_merch <- target_merchants[order(-will_go)]
  not_been_there <- will_go_merch[!will_go_merch %in% been_thr]
  will_go_propensity <- will_go[order(-will_go)][!will_go_merch %in% been_thr]
  merch_rec[i,2:avail] <- not_been_there[1:possible_slots]
  position <- position + customer_merch_ct[i]
}