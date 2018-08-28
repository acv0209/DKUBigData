require("class")   # same as library(class)
setwd("c:/MyR")

#----------------------------------------------------------------------------
## Example-1: Data for liver.csv
#----------------------------------------------------------------------------
ds = read.csv("liver.csv", header = TRUE)
head(ds)
# prepare train/test data
train <- rbind(ds[1:100,], ds[201:270,])
test <- rbind(ds[101:200,], ds[271:345,])
head(train)
head(test) 

# run classification test
result <- knn(train[,-1], test[,-1], cl=train[,1], k=1)
result

# performance evaluation 1
acc <- mean(result==test[,1])
acc

# more performance evaluation
library(gmodels)    # for CrossTable
tab <- CrossTable(x = test[,1], 
                  y = result,
                  prop.chisq=FALSE)
tab

acc2 <- (tab$t[1,1]+tab$t[2,2])/sum(tab$t)
sens <- tab$t[1,1]/(tab$t[1,1]+tab$t[1,2])
spec <- tab$t[2,2]/(tab$t[2,1]+tab$t[2,2])

acc2
sens
spec

(result <- data.frame(measure=c("accuracy", "sensitivity", "specificity"), value=c(acc2, sens, spec)))

#----------------------------------------------------------------------------
## Example-2: Classifying Cancer Samples - wisc_bc_data.csv
#----------------------------------------------------------------------------
## Step 2: Exploring and preparing the data 

# import the CSV file
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

# examine the structure of the wbcd data frame
str(wbcd)

# drop the id feature (no need)
wbcd <- wbcd[-1]

# table of diagnosis
table(wbcd$diagnosis)

# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 2)

# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

# confirm that normalization worked
summary(wbcd_n$area_mean)

# create training and test data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train

# create labels for training and test data
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

## Step 3: Training a model on the data ---- load the "class" library
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

## Step 4: Evaluating model performance ---- load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

## Step 5: Improving model performance ----
# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

# The instances where we had correctly classified 98 percent
# of examples previously, we classified only 95 percent correctly this time. Making
# matters worse, we did no better at classifying the dangerous false negatives.


# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
tab01 <- CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
acc01 <- (tab01$t[1,1]+tab01$t[2,2])/sum(tab01$t)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
tab05 <- CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
acc05 <- (tab05$t[1,1]+tab05$t[2,2])/sum(tab05$t)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
tab11 <- CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
acc11 <- (tab11$t[1,1]+tab11$t[2,2])/sum(tab11$t)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
tab15 <- CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
acc15 <- (tab15$t[1,1]+tab15$t[2,2])/sum(tab15$t)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
tab21 <- CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
acc21 <- (tab21$t[1,1]+tab21$t[2,2])/sum(tab21$t)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
tab27 <- CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
acc27 <- (tab27$t[1,1]+tab27$t[2,2])/sum(tab27$t)

Final <- cbind(acc01, acc05, acc11, acc15, acc21, acc27)
Final

#----------------------------------------------------------------------------
## Example-3: The Actual KNN Model for Data iris 
#----------------------------------------------------------------------------
data(iris)

# Return structure of iris
str(iris)

# Division of `Species`
table(iris$Species) 

# Percentual division of `Species`
round(prop.table(table(iris$Species)) * 100, digits = 2)

# Summary overview of `iris`
summary(iris) 

# Refined summary overview
summary(iris[c("Petal.Width", "Sepal.Width")])

# Build your own normalize() function
normalize <- function(x) {((x-min(x))/(max(x)-min(x)))}

# Normalize the `iris` data
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
iris_norm

# Summarize `iris_norm`
summary(iris_norm)

set.seed(1234)
# replace=TRUE is required to prevent current selection from biasing next selection
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.70, 0.30))

# view distribution of 1s and 2s
table(ind)

# Compose training set
iris.training <- iris[ind==1, 1:4]

# Inspect training set
head(iris.training)

# Compose test set
iris.test <- iris[ind==2, 1:4]

# Inspect test set
head(iris.test)

# Compose `iris` training labels
iris.trainLabels <- iris[ind==1,5]

# Inspect result
print(iris.trainLabels)

# Compose `iris` test labels
iris.testLabels <- iris[ind==2,5]

# Inspect result
print(iris.testLabels)

# Build the model
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)

# Inspect `iris_pred`
iris_pred

# Put `iris.testLabels` in a data frame
irisTestLabels <- data.frame(iris.testLabels)

# Merge `iris_pred` and `iris.testLabels` 
merge <- data.frame(iris_pred, iris.testLabels)

# Specify column names for `merge`
names(merge) <- c("Predicted Species", "Observed Species")

# Inspect `merge` 
merge

library(class)

# you can make a cross tabulation or a contingency table.
library(gmodels)
tab <- CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)
accuracy <- (tab$t[1,1]+tab$t[2,2]+tab$t[3,3])/sum(tab$t)
accuracy


### Another Method  ###
# Create index to split based on labels  
library(gmodels)

install.packages("caret")
library(caret)

# Create index to split based on labels  
index <- createDataPartition(iris$Species, p=0.70, list=FALSE)

# Subset training set with index
iris.training <- iris[index,]

# Subset test set with index
iris.test <- iris[-index,]

# Overview of algos supported by caret
names(getModelInfo())

install.packages("e1071")
library(e1071)

# Train a model
model_knn <- train(iris.training[, 1:4], iris.training[, 5], method='knn')

# Predict the labels of the test set
predictions<-predict(object=model_knn, iris.test[,1:4])

# Evaluate the predictions
table(predictions)

# Confusion matrix 
confusionMatrix(predictions,iris.test[,5])



