---
title: "Practical machine learning"
author: "Sara"
date: '2022-04-25'
output:
  word_document: default
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Data Processing

```{r cars}
library(corrplot)
library(caret)
library(ggplot2)
library(lattice)
```

Read csv:

```{r}
url_train <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_quiz  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
```

```{r cars}
Rtrain <- read.csv(url(url_train), strip.white = TRUE, na.strings = c("NA",""))
Rtest  <- read.csv(url(url_quiz),  strip.white = TRUE, na.strings = c("NA",""))
```

## Exploratory Data Analysis

Removing the NA and empty values data:

```{r pressure, echo=FALSE}
Percentage_max_NA = 90
maxNACount <- nrow(Rtrain) / 100 * Percentage_max_NA
removeColumns <- which(colSums(is.na(Rtrain) | Rtrain=="") > maxNACount)
training.cleaned <- Rtrain[,-removeColumns]
testing.cleaned <- Rtest[,-removeColumns]
```

Reducing the columns to only 60 columns

```{r}
dim(training.cleaned)
```

```{r}
dim(testing.cleaned)
```

Investigating the data we can see that the seven first columns have a sequencial number (the first) and variations of the timestamp that we are not using for this analysis so we will eliminate those columns remaining 53

Investigating: The first 7 columns have a sequential number.

Eliminating this and the variations of the timestamp so 53 columns will be remaining.

```{r}
trainOK<-training.cleaned[,-c(1:6)]
testOK<-testing.cleaned[,-c(1:6)]
dim(trainOK);dim(testOK)
```

```{r}
exerCorrmatrix<-cor(trainOK[sapply(trainOK, is.numeric)])  
corrplot(exerCorrmatrix,order="original", method="circle", type="lower", tl.cex=0.45, tl.col="black", number.cex=0.25)
```

ultimate validation set, we will split the current training in a test and train set to work with

```{r}
set.seed(2022)
inTrain<-createDataPartition(trainOK$classe, p=3/4, list=FALSE)
Rtrain<-trainOK[inTrain,]
valid<-trainOK[-inTrain,] 
```

Analysing the principal components, we got that 25 components are necessary to capture .95 of the variance. But it demands alot of machine processing so, we decided by a .80 to capture 80% of the variance using 13 components

```{r}
PropPCA<-preProcess(Rtrain[,-54],method="pca", thresh=0.8)
PropPCA
```

## Preprocessing:

```{r}
#create the preProc object, excluding the response (classe)
preProc  <- preProcess(Rtrain[,-54], 
                       method = "pca",
                       pcaComp = 13, thresh=0.8) 
#Apply the processing to the train and test data, and add the response 
#to the dataframes
train_pca <- predict(preProc, Rtrain[,-54])
train_pca$classe <- Rtrain$classe
#train_pca has only 13 principal components plus classe
valid_pca <- predict(preProc, valid[,-54])
valid_pca$classe <- valid$classe
#valid_pca has only 13 principal components plus classe
```

## Model Examination:

Random Forest model:

```{r}
start <- proc.time()
fitControl<-trainControl(method="cv", number=5, allowParallel=TRUE)
fit_rf<-train(classe ~., data=train_pca, method="rf", trControl=fitControl)
print(fit_rf, digits=4) 
```

```{r}
proc.time() - start
```

```{r}
predict_rf<-predict(fit_rf,valid_pca)  
(conf_rf<-confusionMatrix(as.factor(valid_pca$classe), predict_rf))
```

```{r}
(accuracy_rf<-conf_rf$overall['Accuracy'])
```

Random forest method has an accuracy of 0.96

## Prediction on Testing Set

Applying the Random Forest to predict the outcome variable classe for the test set.

```{r}
test_pca <- predict(preProc, testOK[,-54])
test_pca$problem_id <- testOK$problem_id
(predict(fit_rf, test_pca))
```

these are the 20 predictions.
