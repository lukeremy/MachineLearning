setwd("~/R/Coursera/MachineLearning/project")
library(randomForest); library(caret)

# source of data
# http://groupware.les.inf.puc-rio.br/har
# citation: Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.

# data csv file URLs
trainingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# only need to do this step once
# download.file(trainingURL,destfile="training_data.csv")
# download.file(testingURL,destfile="testing_data.csv")

# read data into R
train <- read.csv("training_data.csv",header=T,na.string=c("","#DIV/0!","NA"))
test <- read.csv("testing_data.csv",na.string=c("","#DIV/0!","NA"))

### clean data 

# first 7 columns contain non-relevant data, remove them
train <- train[,-(1:7)]
test <- test[,-(1:7)]

# remove all columns with > 5 NA values
nopes <- apply(train,2,function(x) { sum(is.na(x))})
train <- train[,nopes<5]
test <- test[,nopes<5]

# create "testing" & "training" datasets out of train dataset 
set.seed(30)
intrain <- createDataPartition(y=train$classe,p=.75,list=FALSE)
training <- train[intrain,]
testing <- train[-intrain,]

# apply randomForest() to training dataset, get model
modelFit <- randomForest(classe~.,data=training)
modelFit

# get confusion matrix from model, check accuracy
pred <- predict(modelFit,testing)
rfMatrix <- confusionMatrix(pred, testing$classe)
rfMatrix

pred_test <- predict(modelFit,test)
pred_test

# out of sample error rate = 1 - accuracy