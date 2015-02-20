

#library(e1071)
#library(rpart)

# read the csv files
setwd("~/machine")
train<-read.csv("pml-training.csv", na.strings = c("NA",""," "))
test<-read.csv("pml-testing.csv", na.strings = c("NA",""," "))

#clean the dataset. Eliminate all NAs columns
nacol.train<-which(colSums(is.na(train))>0)
nacol.test<-which(colSums(is.na(test))>0)
train<-train[,-c(nacol.train)]
test<-test[,-c(nacol.test)]

#eliminate first 7 columns they doesn't bring any important information
train<-train[,-c(1:7)]
test<-test[,-c(1:7)]

library(randomForest)
library(caret)
set.seed(12345)

#divide the training data in training and testings for cross-validation
inTrain<-createDataPartition(train$classe, p = 0.7, list=FALSE)

training<-train[inTrain,]
testing<-train[-inTrain,] 
#testing is the unused data to make the cross validation of the model fit

#fit the model

#model <- randomForest(classe ~ ., data = training, ntree=500, importance=TRUE)
model<- train(classe ~ .,data=training, method="rf",ntree=500, trControl=trainControl(method="cv",number=5))
model ##Cross-Validated (5 fold) 

##############pruebas
plot(model$finalModel, uniform=TRUE, main="Error vs Number of trees")


# Importance of the variables
#varImpPlot(model$finalModel, color="red")
plot(varImp(model), top=20)

#library(rattle)
#fancyRpartPlot(model$finalModel)
#fancyRpartPlot(model)

######################################

pred<- predict(model, testing)

#check the accuracy of the model fit
acc<-confusionMatrix(pred, testing$classe)
acc #accuracy high enough

#Out of sample error
ooserror <- sum(pred == testing$classe)/length(pred)
ooserror
outOfSampleError <- (1 - ooserror)
outOfSampleError*100

#To write the prediction files
answers<-predict(model, test)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(answers)





