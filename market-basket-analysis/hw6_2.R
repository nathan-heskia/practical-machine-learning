library(rapportools)
library(arules)


instacartorders <- read.transactions('order_products__prior.csv', format = 'single', header = TRUE, sep = ",", cols = c(1,2))

itemLabels(instacartorders) <- sapply(as.numeric(itemLabels(instacartorders)), function(x) { products[which(products$product_id == x),]$product_name; })

summary(instacartorders)

itemFrequencyPlot(instacartorders, topN = 25)

instacartrules <- apriori(instacartorders, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))

summary(instacartrules)

instacartrules <- apriori(instacartorders, parameter = list(support = 0.001, confidence = 0.20, minlen = 2))

summary(instacartrules)



products <- read.csv('products.csv', sep = ",", stringsAsFactors = FALSE)

departments <- read.csv('departments.csv', sep = ",", stringsAsFactors = FALSE)

departments

instacartorders <- read.csv('order_products__prior.csv', sep = ",", stringsAsFactors = FALSE)

merged <- merge(x = instacartorders, y = products, by = "product_id", all.x = TRUE)

merged <- merge(x = merged, y = departments, by = "department_id", all.x = TRUE)

merged$product_name <- paste(merged$product_name, merged$department, sep = " - ")

write.csv(merged,"order_product__prior_combined.csv", row.names = FALSE)

colnames(merged)

instacartorders <- read.transactions('order_product__prior_combined.csv', format = 'single', header = TRUE, sep = ",", cols = c(3,6))

summary(instacartorders)

itemFrequencyPlot(instacartorders, topN = 25)

canned_good_labels <- grep("canned goods$", itemLabels(instacartorders), value = TRUE)

alcohol_labels <- grep("alcohol$", itemLabels(instacartorders), value = TRUE)

produce_labels <- grep("produce$", itemLabels(instacartorders), value = TRUE)

not_produce_labels <- grep("[^produce]$", itemLabels(instacartorders), value = TRUE)

#itemsets <- apriori(instacartorders, parameter = list(support = 0, minlen = 1, target='frequent'))

rules <- apriori(instacartorders, parameter = list(support = 0.01, confidence = 0.01), appearance = list(lhs = canned_good_labels, rhs = setdiff(not_produce_labels,canned_good_labels)))

inspect(head(rules))

inspect(sort(rules, by = "lift")[1:20])

rules <- apriori(instacartorders, parameter = list(support = 0.001, confidence = 0.01), appearance = list(lhs = alcohol_labels))

inspect(head(rules))

inspect(sort(rules, by = "lift")[1:20])

rules <- apriori(instacartorders, parameter = list(support = 0.001, confidence = 0.05), appearance = list(none = produce_labels))

inspect(head(rules))

inspect(sort(rules, by = "lift")[1:20])

rules <- apriori(instacartorders, parameter = list(support = 0.001, confidence = 0.05), appearance = list(both = union(alcohol_labels, canned_good_labels)))

inspect(head(rules))

inspect(sort(rules, by = "lift")[1:20])

rules <- apriori(instacartorders, parameter = list(support = 0.00001, confidence = 0.00005, target = "rules", minlen=2), appearance = list(lhs=canned_good_labels, rhs=alcohol_labels))

inspect(head(rules))

inspect(sort(rules, by = "lift")[1:20])



inspect(subset(itemsets, subset = items %pin% "pantry" ) )

#canned_rules <- apriori(instacartorders[], parameter = list(support = 0.001, confidence = 0.20, minlen = 2))

# orders <- merged[,c("order_id", "product_name", "department")]
# orders$product_name <- as.factor(orders$product_name)
# orders$department <- as.factor(orders$department)
# transaction_ids <- as.factor(merged[,c("order_id")])
# 
# instacartorders <- as(orders, "transactions")
# 
# transactionInfo(instacartorders)[["transactionID"]] <- transaction_ids
# 
# inspect(head(instacartorders))
# 
# instacartrules <- apriori(instacartorders, parameter = list(support = 0.0005, confidence = 0.001), appearance = list(items = c("department=canned goods", "product_name=Organic Tomato Paste")))

new_products <- as.factor(paste(merged$product_name, merged$department, sep = " - "))

trans <- split(new_products, as.factor(merged[,c("order_id")]))

basket <- as(trans, "transactions")

# Given that everyone is buying alcohol and canned goods -> which items appear more frequently that I could advertise or run a promotion for in this time?

# orders <- read.csv('order_products__prior.csv', sep = ",", stringsAsFactors = FALSE)
# 
# 
# 
# total <- merge(x = orders, y = products, by = "product_id", all.x = TRUE)
# 
# total$product_name <- as.factor(total$product_name)
# total$order_id <- as.factor(total$order_id)
# total$department_id <- as.factor(total$department_id)
# 
# instacartorders <- as(total[,c("order_id", "product_name","department_id")], "transactions")
# 
# try_rules <- apriori(instacartorders, parameter = list(support = 0.1))
# 
# 
# transactions_split <- split(total$product_name, total$order_id)
# 
# instacartorders <- as(transactions_split, "transactions")
# 
# total <- merge(x = total, y = departments, by = "department_id", all.x = TRUE)
# 
# ### remove transaction IDs
# tid <- as.character(df[["Transaction_ID"]])
# df <- df[,-1]
# 
# ### make all columns factors
# for(i in 1:ncol(df)) df[[i]] <- as.factor(df[[i]])
# 
# trans <- as(df, "transactions")
# 
# ### set transactionIDs
# transactionInfo(trans)[["transactionID"]] <- tid
