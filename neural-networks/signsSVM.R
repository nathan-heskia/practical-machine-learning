library(caret)
library(imager)
library(magick)
library(neuralnet)
library(dplyr)

set.seed(311)

# SVM method

signs <- read.csv('sign_mnist_train.csv', stringsAsFactors = FALSE)

signs$label <- as.factor(signs$label)

signs[,2:785] <- signs[,2:785] / 255.0

signsTrain <- sample_n(signs[which(signs$label == '0'),], 400)

for(r in c(1:26)) {
  signsTrain <- rbind(signsTrain, sample_n(signs[which(signs$label == as.character(r)),], 400))
}

svmModel <- train(
  label ~ ., data = signsTrain, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale")
)

saveRDS(svmModel, './svmLinearSigns400')

svmModel <- train(
  label ~ ., data = signs, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale")
)

saveRDS(svmModel, './svmLinearSigns')