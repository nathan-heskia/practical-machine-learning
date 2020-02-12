library(caret)

set.seed(404)
zoo_animals <- read.csv(file = "zoo.data", stringsAsFactors = FALSE, skip = 1, header=FALSE)

colnames(zoo_animals) <- c("name",
  "hair",
  "feathers",
  "eggs",
  "milk",
  "airborne",
  "aquatic",
  "predator",
  "toothed",
  "backbone",
  "breathes",
  "venomous",
  "fins",
  "legs",
  "tail",
  "domestic",
  "catsize",
  "type") 

zoo_animals[-c(1, 18)] <- lapply(zoo_animals[-c(1, 18)], factor)


zoo_animals$type <- ifelse(zoo_animals$type == 1, "mammal", zoo_animals$type)
zoo_animals$type <- ifelse(zoo_animals$type == 2, "bird", zoo_animals$type)
zoo_animals$type <- ifelse(zoo_animals$type == 3, "reptile", zoo_animals$type)
zoo_animals$type <- ifelse(zoo_animals$type == 4, "fish", zoo_animals$type)
zoo_animals$type <- ifelse(zoo_animals$type == 5, "amphibian", zoo_animals$type)
zoo_animals$type <- ifelse(zoo_animals$type == 6, "bug", zoo_animals$type)
zoo_animals$type <- ifelse(zoo_animals$type == 7, "invertebrate", zoo_animals$type)

zoo_animals$type <- as.factor(zoo_animals$type)

summary(zoo_animals)

in_train <- createDataPartition(zoo_animals$type, p=0.75, list = FALSE)

# Create Training Data 
training_data <- zoo_animals[in_train,-c(1)] # Name not needed
test_data <- zoo_animals[-in_train, -c(1)]

# One R
oner_model <- train(type ~ ., data = training_data, method = "OneR")

summary(oner_model)

predictions <- predict(oner_model, test_data)

accuracy <- sum(predictions == (test_data$type))/length(test_data$type)

print(accuracy)

# RIPPER - Cross Validation
ctrl <- trainControl(method = "cv", number = 10)
jrip_model <- train(type ~ ., data = training_data, method = "JRip", trControl = ctrl, tuneLength = 15)

summary(jrip_model)

predictions <- predict(jrip_model, test_data)

accuracy <- sum(predictions == (test_data$type))/length(test_data$type)
print(accuracy)

# C5 - Cross Validation
c5_model <- train(type ~ ., data = training_data, method = "C5.0", trControl = ctrl, tuneLength = 15)

summary(c5_model)

predictions <- predict(c5_model, test_data)

accuracy <- sum(predictions == (test_data$type))/length(test_data$type)
print(accuracy)

# RIPPER - Bootstrap sampling
jrip_model <- train(type ~ ., data = training_data, method = "JRip")

summary(jrip_model)

predictions <- predict(jrip_model, test_data) 

accuracy <- sum(predictions == (test_data$type))/length(test_data$type)
print(accuracy)

# C5 - Cross Validation
c5_model <- train(type ~ ., data = training_data, method = "C5.0", tuneGrid = data.frame(model = "tree", trials = 20, winnow = FALSE))

summary(c5_model)

predictions <- predict(c5_model, test_data)

accuracy <- sum(predictions == (test_data$type))/length(test_data$type)
print(accuracy)

library(RWeka)

oner_model <- RWeka::OneR(type ~ ., data = training_data)

summary(oner_model)

