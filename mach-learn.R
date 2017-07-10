setwd("C:/Users/perre/Desktop/machine-learning")

require(caret)
require(rattle)
require(rpart.plot)

## Download and read data

if(!file.exists("./data")) {
        dir.create("./data")
        url1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        url2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        file_dest1 <- "./data/pml-training.csv"
        file_dest2 <- "./data/pml-testing.csv"
        
        download.file(url1, file_dest1)
        download.file(url2, file_dest2)
}

train_raw <- read.csv("./data/pml-training.csv")
test_raw <- read.csv("./data/pml-testing.csv")

## 1. Data preprocessing

# Remove id variables
TR <- train_raw[, -c(1:5)]

# Remove variables that are more than 90% missing values
nas <- sapply(TR, function(x) mean(is.na(x))) > 0.9

TR <- TR[, nas == 0]

# Remove variables with almost zero variation (very few unique values)

TR <- TR[, - nearZeroVar(TR)]

# Split the train dataset to 2 parts: 80% of it's observations will form the final
# training set and 20% will be used as validation to estimate the out-of-sample
# error

inTrain <- createDataPartition(TR$classe, p = 0.8, list = F)
TR <- TR[inTrain, ]
VLD <- TR[-inTrain, ]

## 2. Model 1 - Classification tree

# First we will try fitting a simple classification tree

set.seed(346)
mdl_tree <- train(classe ~ ., method = "rpart", data = TR)
fancyRpartPlot(mdl_tree$finalModel)
pred_tree <- predict(mdl_tree, VLD)

# View the confusion matrix to assess the model accuracy on the validation set

confusionMatrix(VLD$classe, pred_tree)

# With 58% accuracy, this simple classification tree is not good enough.

## 3. Model 2 - Random forest

# We will try fitting a random forest model

set.seed(347)
mdl_for <- train(classe ~ ., method = "rf", data = TR)
pred_for <- predict(mdl_for, VLD)
confusionMatrix(VLD$classe, pred_for)

# This model has 100% accuracy, which hints to overfitting.
# We will keep it as the final model and use it to predict the test values

pred_quiz <- predict(mdl_for, test_raw)
pred_quiz
