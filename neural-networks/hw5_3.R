library(caret)
library(imager)
library(magick)
library(neuralnet)
library(dplyr)

set.seed(311)

signs <- read.csv('sign_mnist_train.csv', stringsAsFactors = FALSE)
signsTest <- read.csv('sign_mnist_test.csv', stringsAsFactors = FALSE)

signs$label <- as.factor(signs$label)
signsTest$label <- as.factor(signsTest$label)

# 
# signSamples <- sample_n(signs[which(signs$label == '0'),], 250)
# 
# for(r in c(1:26)) {
#   signSamples <- rbind(signSamples, sample_n(signs[which(signs$label == as.character(r)),], 250))
# }

signSamples <- data.frame(signs)

signSamplesEdges <- data.frame(signSamples)

for(i in c(1:nrow(signSamplesEdges))) {
  s <- as.cimg(as.numeric(signSamplesEdges[i,2:785]))
  img <- magick::image_read(s)
  imgEdges <- img %>% image_equalize() %>% image_canny()
  #imgEdges <- img %>% image_canny()
  x <- as.numeric(image_data(imgEdges, 'rgb'))
  signSamplesEdges[i,2:785] <- t(x[,,1])
}

signsTestEdges <- data.frame(signsTest)

for(i in c(1:nrow(signsTestEdges))) {
  s <- as.cimg(as.numeric(signsTestEdges[i,2:785]))
  img <- magick::image_read(s)
  imgEdges <- img %>% image_equalize() %>% image_canny()
  #imgEdges <- img %>% image_canny()
  x <- as.numeric(image_data(imgEdges, 'rgb'))
  signsTestEdges[i,2:785] <- t(x[,,1])
}

set.seed(104)
trainIndex <- createDataPartition(signSamplesEdges$label, p = 0.75, list=FALSE)
trainData <- signSamplesEdges[trainIndex,]
trainTest <- signSamplesEdges[-trainIndex,]

nn1 <- train(label ~ ., data = trainData, trControl= trainControl(method = "cv", number = 3), 
             method = "nnet", tuneGrid = expand.grid(.size = c(1), .decay = 5e-4), 
             preProc = c("center", "scale"), linout = 0)

nn5 <- train(label ~ ., data = trainData, trControl= trainControl(method = "cv", number = 3), 
             method = "nnet", tuneGrid = expand.grid(.size = c(5), .decay = 5e-4), 
             preProc = c("center", "scale"),linout = 0, MaxNWts = 4100)

predictions5 <- predict(nn5, trainTest[,-1])

confusionMatrix(predictions5, trainTest$label)

predictions5 <- predict(nn5, signsTestEdges[,-1])

confusionMatrix(predictions5, signsTestEdges$label)

nn10 <- train(label ~ ., data = trainData, trControl= trainControl(method = "cv", number = 3), 
              method = "nnet", tuneGrid = expand.grid(.size = c(10), .decay = 5e-4), 
              preProc = c("center", "scale"),linout = 0, MaxNWts = 8200)

nn14 <- train(label ~ ., data = trainData, trControl= trainControl(method = "cv", number = 3), 
              method = "nnet", tuneGrid = expand.grid(.size = c(14), .decay = 5e-4), 
              preProc = c("center", "scale"),linout = 0, MaxNWts = 14000)

predictions14 <- predict(nn14, trainTest[,-1])

confusionMatrix(predictions14, trainTest$label)

predictions14 <- predict(nn14, signsTestEdges[,-1])

confusionMatrix(predictions14, signsTestEdges$label)

nn1 <- train(label ~ ., data = signSamplesEdges, trControl= trainControl(method = "cv", number = 3), 
             method = "nnet", tuneGrid = expand.grid(.size = c(1), .decay = 5e-4), 
             preProc = c("center", "scale"), linout = 0)

nn5 <- train(label ~ ., data = signSamplesEdges, trControl= trainControl(method = "cv", number = 3), 
             method = "nnet", tuneGrid = expand.grid(.size = c(5), .decay = 5e-4), 
             preProc = c("center", "scale"),linout = 0, MaxNWts = 4100)

nn10 <- train(label ~ ., data = signSamplesEdges, trControl= trainControl(method = "cv", number = 3), 
              method = "nnet", tuneGrid = expand.grid(.size = c(10), .decay = 5e-4), 
              preProc = c("center", "scale"),linout = 0, MaxNWts = 8200)

nn17 <- train(label ~ ., data = signSamplesEdges, trControl= trainControl(method = "cv", number = 3), 
              method = "nnet", tuneGrid = expand.grid(.size = c(17), .decay = 5e-4), 
              preProc = c("center", "scale"),linout = 0, MaxNWts = 14000)

signsnn1 <- train(label ~ ., data = signs, trControl= trainControl(method = "cv", number = 3), 
             method = "nnet", tuneGrid = expand.grid(.size = c(1), .decay = 5e-4), 
             preProc = c("center", "scale"), linout = 0)

signsnn5 <- train(label ~ ., data = signs, trControl= trainControl(method = "cv", number = 3), 
                  method = "nnet", tuneGrid = expand.grid(.size = c(5), .decay = 5e-4), 
                  preProc = c("center", "scale"),linout = 0, MaxNWts = 4100)

signsnn10 <- train(label ~ ., data = signs, trControl= trainControl(method = "cv", number = 3), 
                  method = "nnet", tuneGrid = expand.grid(.size = c(10), .decay = 5e-4), 
                  preProc = c("center", "scale"),linout = 0, MaxNWts = 8400)

predictions <- predict(signsnn1, signsTest[,-1])

confusionMatrix(predictions, signsTest$label)

signsTestEdges <- data.frame(signsTest)

for(i in c(1:nrow(signsTestEdges))) {
  s <- as.cimg(as.numeric(signsTestEdges[i,2:785]))
  img <- magick::image_read(s)
  imgEdges <- img %>% image_canny()
  x <- as.numeric(image_data(imgEdges, 'rgb'))
  signsTestEdges[i,2:785] <- t(x[,,1])
}

i <- as.cimg(as.numeric(signsTestEdges[50,2:785]))

plot(i)

predictions <- predict(signsnn10, signsTestEdges[,-1])

confusionMatrix(predictions, signsTestEdges$label)





# SVM

svmModel <- train(
  label ~., data = signs, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale")
)

svmModel <- train(
  label ~., data = signs, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
  preProcess = c("center","scale")
)

plot(svmModel)

# More sampling

signs <- read.csv('sign_mnist_train.csv', stringsAsFactors = FALSE)
signsTest <- read.csv('sign_mnist_test.csv', stringsAsFactors = FALSE)

signs$label <- as.factor(signs$label)
signsTest$label <- as.factor(signsTest$label)

signs[,2:785] <- signs[,2:785] / 255.0
signsTest[,2:785] <- signsTest[,2:785] / 255.0

signsTrain <- sample_n(signs[which(signs$label == '0'),], 400)

for(r in c(1:26)) {
  signsTrain <- rbind(signsTrain, sample_n(signs[which(signs$label == as.character(r)),], 400))
}

nn5 <- train(label ~ ., data = signsTrain, trControl= trainControl(method = "cv", number = 3),
             method = "nnet", tuneGrid = expand.grid(.size = c(5), .decay = 5e-4),
             preProc = c("center", "scale"), linout = 0, MaxNWts = 4100)

nn15 <- train(label ~ ., data = signsTrain, trControl= trainControl(method = "cv", number = 5),
             method = "nnet", tuneGrid = expand.grid(.size = c(15), .decay = 5e-4),
             preProc = c("center", "scale"), linout = 0, MaxNWts = 13000)

nnMultiple10 <- train(label ~ ., data = signsTrain, trControl= trainControl(method = "cv", number = 5, verboseIter = TRUE),
                    method = "mlpML", tuneGrid = expand.grid(.layer1 = 10, .layer2 = 10, .layer3 = 10),
                    preProc = c("center", "scale"))

nnMultiple15 <- train(label ~ ., data = signsTrain, trControl= trainControl(method = "cv", number = 5, verboseIter = TRUE),
method = "mlpML", tuneGrid = expand.grid(.layer1 = 15, .layer2 = 15, .layer3 = 15),
preProc = c("center", "scale"))

nnMultiple18 <- train(label ~ ., data = signsTrain, trControl= trainControl(method = "cv", number = 5, verboseIter = TRUE),
method = "mlpML", tuneGrid = expand.grid(.layer1 = 18, .layer2 = 18, .layer3 = 18),
preProc = c("center", "scale"))

nnMultiple20 <- train(label ~ ., data = signsTrain, trControl= trainControl(method = "cv", number = 5, verboseIter = TRUE),
method = "mlpML", tuneGrid = expand.grid(.layer1 = 20, .layer2 = 20, .layer3 = 20),
preProc = c("center", "scale"))

nnMultiple30 <- train(label ~ ., data = signsTrain, trControl= trainControl(method = "cv", number = 5, verboseIter = TRUE),
method = "mlpML", tuneGrid = expand.grid(.layer1 = 30, .layer2 = 30, .layer3 = 30),
preProc = c("center", "scale"))

nnMultiple40 <- train(label ~ ., data = signsTrain, trControl= trainControl(method = "cv", number = 5, verboseIter = TRUE),
method = "mlpML", tuneGrid = expand.grid(.layer1 = 40, .layer2 = 40, .layer3 = 40),
preProc = c("center", "scale"))

nnMultiple54 <- train(label ~ ., data = signsTrain, trControl= trainControl(method = "cv", number = 5, verboseIter = TRUE),
method = "mlpML", tuneGrid = expand.grid(.layer1 = 54, .layer2 = 54, .layer3 = 54),
preProc = c("center", "scale"))


# SVM method

signsTrain <- sample_n(signs[which(signs$label == '0'),], 400)

for(r in c(1:26)) {
  signsTrain <- rbind(signsTrain, sample_n(signs[which(signs$label == as.character(r)),], 400))
}

svmModel <- train(
  label ~ ., data = signsTrain, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale")
)