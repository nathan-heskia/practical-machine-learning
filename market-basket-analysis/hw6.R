library(readxl)
library(rapportools)
library(arules)

online_retail <- read_excel("online-retail.xlsx")

online_retail$StockCode <- as.factor(online_retail$StockCode)
#online_retail$Country <- as.factor(online_retail$Country)

online_retail$InvoiceNo <- as.numeric(online_retail$InvoiceNo)
online_retail <- online_retail[which(online_retail$UnitPrice >= 0.01 & online_retail$Quantity > 0 & !is.na(online_retail$Description) & !(online_retail$StockCode %in% c('M', 'B', 'b', 'AMAZONFEE', 'POST', 'DOT'))),]

summary(online_retail)

online_retail <- online_retail[c(1,2,3)]

head(online_retail)

online_retail_baskets <- data.frame(invoice_no = unique(online_retail$InvoiceNo), itemset = '', stringsAsFactors = FALSE)

for (row in 1:nrow(online_retail)) {
  invoice_no <- online_retail[row,]$InvoiceNo
  stock_code  <- online_retail[row,]$StockCode
  
  invoiceitems <- online_retail_baskets[which(online_retail_baskets$invoice_no == invoice_no),]
  
  if(is.empty(invoiceitems$itemset)) {
    invoiceitems$itemset <- as.character(stock_code)
  } else {
    invoiceitems$itemset <- paste(as.character(invoiceitems$itemset), as.character(stock_code), sep = ",")
  }
  
  online_retail_baskets[which(online_retail_baskets$invoice_no == invoice_no),] <- invoiceitems
}

write.csv(online_retail, 'test.csv', row.names = FALSE)

baskets <- read.transactions('test.csv', format = 'single', header = TRUE, sep = ",", cols = c(1,3))

itemFrequencyPlot(baskets, support = 0.08)

itemFrequencyPlot(baskets, topN = 20)

image(sample(baskets, 1000))
  