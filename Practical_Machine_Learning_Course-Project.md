---
title: "Practical Machine Learning - Course Project"
author: "VICTOR CAMPOS"
date: '2022-09-07'
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 


#Data
According to the information, the URL to download this information is as follows:
The training data for this project are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

#Loading the libraries

```{r}
library(lubridate)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
```

#Downloading the data
```{r}
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "C:/Users/vicct/Downloads/pml-training.csv", mode = "wb")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "C:/Users/vicct/Downloads/pml-testing.csv", mode = "wb")
```

##Cleaning the data
The data information comes with (N/A)s & empty spaces, so the following commands will clean the data.

```{r Cleaning the data}
trainingcsv = read.csv("C:/Users/vicct/Downloads/pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testcsv = read.csv("C:/Users/vicct/Downloads/pml-training.csv", na.strings=c("NA","#DIV/0!",""))
trainingcsv <- trainingcsv[, colSums(is.na(trainingcsv)) == 0] 
testcsv <- testcsv[, colSums(is.na(testcsv)) == 0] 
```

##Removing columns with the X, timestamp & windows strigns. Those columns are unnecessary for the calculations. Using grepl to look for the unwised columns from te training and test data sets. 
```{r}
classe <- trainingcsv$classe

trainRemove <- grepl("^X|timestamp|window", names(trainingcsv))
trainingcsv <- trainingcsv[, !trainRemove]
trainingcsv_cleaned <- trainingcsv[, sapply(trainingcsv, is.numeric)]
trainingcsv_cleaned$classe <- classe
testRemove <- grepl("^X|timestamp|window", names(testcsv))
testcsv <- testcsv[, !testRemove]
testcsv_cleaned <- testcsv[, sapply(testcsv, is.numeric)]
```

#Partitioning the Data
##Using Caret to create a 70/30% split of the cleaned training data.
```{r}
training_partition <- createDataPartition(y = trainingcsv_cleaned$classe, p = 0.7, list = F)
trainingdata_partition <- trainingcsv_cleaned[training_partition,] #training data frame 
testingdata_partition <- trainingcsv_cleaned[-training_partition,] #test data frame 
```


#Examining the proportions of the data 
```{r}
prop.table(table(trainingdata_partition$classe))
prop.table(table(testingdata_partition$classe))
```
##Portions are equivalent for the training data and the testing data. 

##Building the Model
## Set up caret to perform 5-fold cross validation. Ramdom Forest is the method we will use to predict the activity recognition. This method will select the variables and correlate covariables. 

```{r}
train.control <- trainControl(method = "cv", 5)
model.rf <- train(classe ~ ., data = trainingdata_partition, method = "rf", trControl = train.control)
##Examining model.rf
model.rf
```

##Validating the performance using the testing data set. 
```{r}
predict_classe <- predict(model.rf, testingdata_partition)
#Checking the levels
table(testingdata_partition$classe) ##Predicted
table(predict_classe) ##Actual
confusionMatrix(as.factor(testingdata_partition$classe), predict_classe)
```
###Reviewing thw sensitivity and specifity of the information, then checking the Accuracy and the out of sample error of the results.  
```{r}
accuracy <- postResample(predict_classe, as.factor(testingdata_partition$classe))
accuracy
outofsampleerror <- 1 - as.numeric(confusionMatrix(as.factor(testingdata_partition$classe), predict_classe)$overall[1])
#outofsampleerror
outofsampleerror
```
### the out of sample error result is 0.005 and the accuracy of 0.9949~, this indicate a good result for the prediction. 

#Ploting the correlation of the training data. Correlation among the different variables. 
```{r}
plotdata <- cor(trainingdata_partition[,-length(names(trainingdata_partition))])
corrplot(plotdata, main = "Correlation Matrix Visualization", method="color")
```
#Tree model
##In this model it is possible to check the correct and incorrect barbell lifts executed by the participants. 

```{r}
tree_model <- rpart(classe ~ ., data=trainingdata_partition, method="class")
prp(tree_model, main = "Visualization Tree", type = 1 ) # fast plot
```

