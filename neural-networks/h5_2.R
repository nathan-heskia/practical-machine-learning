library(caret)
library(imager)
library(magick)
library(neuralnet)
library(dplyr)

# https://www.openml.org/d/554

digits <- read.csv('digit_train.csv', stringsAsFactors = FALSE)
digits$label <- as.factor(digits$label)

zerodigits <- sample_n(digits[which(digits$label == '0'),], 500)
onedigits <- sample_n(digits[which(digits$label == '1'),], 500)
twodigits <- sample_n(digits[which(digits$label == '2'),], 500)
threedigits <- sample_n(digits[which(digits$label == '3'),], 500)
fourdigits <- sample_n(digits[which(digits$label == '4'),], 500)
fivedigits <- sample_n(digits[which(digits$label == '5'),], 500)
sixdigits <- sample_n(digits[which(digits$label == '6'),], 500)
sevendigits <- sample_n(digits[which(digits$label == '7'),], 500)
eightdigits <- sample_n(digits[which(digits$label == '8'),], 500)
ninedigits <- sample_n(digits[which(digits$label == '9'),], 500)

digitsTrain <- rbind(zerodigits, onedigits, twodigits, threedigits, fourdigits, fivedigits, sixdigits, sevendigits, eightdigits, ninedigits)

# Example image

par(mfrow=c(2,2))
plot(magick::image_read(as.cimg(as.numeric(digitsTrain[1,2:785]))))
plot(magick::image_read(as.cimg(as.numeric(digitsTrain[2500,2:785]))))
plot(magick::image_read(as.cimg(as.numeric(digitsTrain[3000,2:785]))))
plot(magick::image_read(as.cimg(as.numeric(digitsTrain[5000,2:785]))))

# Train

nn1 <- train(label ~ ., data = digitsTrain, trControl= trainControl(method = "cv", number = 3), 
            method = "nnet", tuneGrid = expand.grid(.size = c(1), .decay = 5e-4), 
            preProc = c("center", "scale"), linout = 0)

nn5 <- train(label ~ ., data = digitsTrain, trControl= trainControl(method = "cv", number = 3), 
            method = "nnet", tuneGrid = expand.grid(.size = c(5), .decay = 5e-4), 
            preProc = c("center", "scale"),linout = 0, MaxNWts = 4000)

nn10 <- train(label ~ ., data = digitsTrain, trControl= trainControl(method = "cv", number = 3), 
            method = "nnet", tuneGrid = expand.grid(.size = c(10), .decay = 5e-4), 
            preProc = c("center", "scale"),linout = 0, MaxNWts = 8000)

nnMultiple <- train(label ~ ., data = digitsTrain, trControl= trainControl(method = "cv", number = 3), 
              method = "mlpML", tuneGrid = expand.grid(.layer1 = 10, .layer2 = 10, .layer3 = 10), 
              preProc = c("center", "scale"))

nnMultiple20 <- train(label ~ ., data = digitsTrain, trControl= trainControl(method = "cv", number = 3), 
                    method = "mlpML", tuneGrid = expand.grid(.layer1 = 10, .layer2 = 10, .layer3 = 10), 
                    preProc = c("center", "scale"))


svmModel <- train(
  label ~ ., data = digitsTrain, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale")
)

svmModelTuned <- train(
  label ~ ., data = digitsTrain, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
  preProcess = c("center","scale")
)

plot(svmModelTuned)

svmModelRadial <- train(
  label ~ ., data = digitsTrain, method = "svmRadial",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale")
)



