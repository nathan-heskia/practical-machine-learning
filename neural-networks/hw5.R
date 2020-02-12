library(caret)
library(neuralnet)

letters <- read.csv('letter-recognition.data', header = FALSE)

colnames(letters)[1] <- 'label'

letters$label <- as.factor(letters$label)

summary(letters$label)

set.seed(104)
trainIndex <- createDataPartition(letters$label, p = 0.75, list=FALSE)
trainData <- letters[trainIndex,]
trainTest <- letters[-trainIndex,]

nn <- train(label ~ ., data = trainData,
             trControl= trainControl(method = "cv", number = 3),
             method = "nnet",
             tuneGrid = expand.grid(.size = c(1), .decay = 0),
             preProc = c("center", "scale"),linout = 0)

nn2 <- train(label ~ ., data = trainData,
            trControl= trainControl(method = "cv", number = 3),
            method = "nnet",
            tuneGrid = expand.grid(.size = c(5), .decay = 0),
            preProc = c("center", "scale"),linout = 0)

predictions <- predict(nn2, trainTest[, -1])

confusionMatrix(predictions, trainTest$label)

nn3 <- train(label ~ ., data = trainData,
             trControl= trainControl(method = "cv", number = 3),
             method = "nnet",
             tuneGrid = expand.grid(.size = c(17), .decay = 5e-4),
             preProc = c("center", "scale"),linout = 0)

predictions <- predict(nn3, trainTest[, -1])

confusionMatrix(predictions, trainTest$label)

nn4 <- neuralnet(V1 ~ ., data = letters, hidden = 3, threshold = 0.01, err.fct = "ce", likelihood = TRUE)

