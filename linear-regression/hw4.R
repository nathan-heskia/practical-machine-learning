library(caret)
library(corrplot)
library(Hmisc)

set.seed(315)

loanstats <-  read.csv(file = "LoanStats_securev1_2019Q4.csv", stringsAsFactors = FALSE, header=TRUE, skip = 1)

# Turn percent into number
loanstats$int_rate <- gsub('%', '', loanstats$int_rate)
loanstats$int_rate <- as.numeric(loanstats$int_rate)

# Turn percent into number
loanstats$revol_util <- gsub('%', '', loanstats$revol_util)
loanstats$revol_util <- as.numeric(loanstats$revol_util)

factor_columns <- c("home_ownership", "pymnt_plan", "purpose", "title", "grade", "term", "sub_grade")

loanstats[factor_columns] <- lapply(loanstats[factor_columns], factor)

loanstats <- loanstats[which(!is.na(loanstats$dti) & !is.na(loanstats$int_rate) & !is.na(loanstats$sub_grade) & !is.na(loanstats$revol_util)),]


cor(loanstats$dti, loanstats$int_rate)

# a = mean(y) - b * mean(x)

# b = cov(x,y)/var(x)

# using dti

b <- cov(loanstats$dti, loanstats$int_rate) / var(loanstats$dti)

a <- mean(loanstats$int_rate, use = "complete") - b * mean(loanstats$dti, use = "complete")

# y = a + bx

predictions <- a + b * loanstats$dti

# The standard error of the estimate is:
# sqrt(sum((loanstats$int_rate - loanstats$pred_int_rate)^2) / nrow(loanstats))
se_est <- RMSE(predictions, loanstats$int_rate)

# One variable, CARET

train_index <- createDataPartition(loanstats$int_rate, p = 0.75, list=FALSE)
loan_train <- loanstats[train_index,]
loan_test <- loanstats[-train_index,]

# setup cross validation and control parameters
control <- trainControl(method="repeatedcv", number=3, repeats = 3, verbose = TRUE, search = "grid")

# Training process 
# Fit / train a Linear Regression model to  dataset
fit.LR <- train(int_rate ~ dti , data=loan_train, method="lm", metric="RMSE",  preProc=c("center", "scale"), trControl=control, tuneLength = 10)

summary(fit.LR)

predictions <- predict(fit.LR, loan_test)

rmse <- RMSE(predictions, loan_test$int_rate)
rmse

error.rate = rmse / mean(loan_test$int_rate)
error.rate

# Search for better fit
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

loanstats$grade_numeric <- as.numeric(loanstats$grade)

correlation_r <- rcorr(as.matrix(loanstats[c("dti", "revol_util", "inq_last_6mths", "delinq_2yrs", "acc_open_past_24mths", "grade_numeric", "int_rate")]))

correlation_Matrix <- correlation_r$r
p_mat <- correlation_r$P

corsig<-corrplot(correlation_Matrix, method = "color", col = col(200),  
   type = "upper", order = "hclust", 
   addCoef.col = "black", # Add coefficient of correlation
   tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
   # Combine with significance level
   p.mat = p_mat, sig.level = 0.05, insig = "blank", 
   # hide correlation coefficient on the principal diagonal
   diag = FALSE,
   title = "Correlation Between Loan Application Attributes",
   mar=c(0,0,1,0)
)

loan_train <- loanstats[train_index,]
loan_test <- loanstats[-train_index,]

fit.LR <- train(int_rate ~ grade_numeric + revol_util, data=loan_test, method="lm", metric="RMSE",  preProc=c("center", "scale"), trControl=control, tuneLength = 10)

summary(fit.LR)

predictions <- predict(fit.LR, loan_test)

R2(predictions, loan_test$int_rate)
 