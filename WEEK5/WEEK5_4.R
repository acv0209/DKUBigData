setwd("c:/MyR")
install.packages("C50")
install.packages("printr")
library(C50)     # for C5.0  
library(gmodels) # for CrossTable

# Example-1
# load dataset
credit <- read.csv("credit.csv")
str(credit)

# Let's take a look at the table() output for a couple of loan features that seem likely
# to predict a default. 

# The checking and savings account balance may prove to be important predictors of
# loan default status. Note that since the loan data was obtained from Germany, the
# currency is recorded in Deutsche Marks (DM).

table(credit$checking_balance)
table(credit$savings_balance)

# Some of the loan's features are numeric, such as its duration and the amount of
# credit requested:

summary(credit$months_loan_duration)
summary(credit$amount)

# The default vector indicates whether the loan applicant was unable to meet the
# agreed payment terms and went into default. A total of 30 percent of the loans in
# this dataset went into default:

table(credit$default)

# The following commands use the sample() function to select 900 values at random
# out of the sequence of integers from 1 to 1000. Note that the set.seed() function
# uses the arbitrary value 12345. Omitting this seed will cause your training and testing
# split to differ from those shown in the remainder of this chapter:

# make train/test data 
set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]
credit_train <- credit_rand[1:900, ]
credit_test  <- credit_rand[901:1000, ]

str(credit_train)
str(credit_test)

# generate model (decision tree)
credit_model <- C5.0(credit_train[-17], credit_train$default) 
credit_model

# perform predict 
credit_pred <- predict(credit_model, credit_test)
credit_pred     # prediction result for test data
mean(credit_pred == credit_test$default) # print accuracy 

# Analysis of prediction result 
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# Step: Improving model performance
# Boosting the accuracy of decision trees
# boosted decision tree with 10 trials
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# Making some mistakes more costly than others
# create dimensions for a cost matrix
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

# build the matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

# apply the cost matrix to the tree
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                          costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
  
# Example-2: Identifying Poisonous Mushrooms ----
# Step 2: Exploring and preparing the data ---- 
# We begin by using read.csv(), to import the data for our analysis. Since all the 22
# features and the target class are nominal, in this case, we will set stringsAsFactors = TRUE
# and take advantage of the automatic factor conversion:

mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)

# examine the structure of the data frame
str(mushrooms)

# The output of the str(mushrooms) command notes that the data contain 8,124
# observations of 23 variables as the data dictionary had described. 

# If you think it is odd that a factor has only one level, you are correct. The data
# dictionary lists two levels for this feature: partial and universal. However, all the
# examples in our data are classifed as partial. It is likely that this data element was
# somehow coded incorrectly. In any case, since the veil type does not vary across
# samples, it does not provide any useful information for prediction. We will drop this
# variable from our analysis using the following command:

# drop the veil_type feature
mushrooms$veil_type <- NULL

# We should take a quick look at the distribution of the
# mushroom type class variable in our dataset:
# examine the class distribution
table(mushrooms$type)

# We are not trying to develop rules that cover unforeseen types of mushrooms ; 
# we are merely trying to fnd rules that accurately depict the complete set of 
# known mushroom types. Therefore, we can build and test the model on the same data.

## Step 3: Training a model on the data ----
install.packages("RWeka")
library(RWeka)

# train OneR() on the data
mushroom_1R <- OneR(type ~ ., data = mushrooms)

## Step 4: Evaluating model performance ----
mushroom_1R
summary(mushroom_1R)

## Step 5: Improving model performance ----
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip
summary(mushroom_JRip)

# Rule Learner Using C5.0 Decision Trees (not in text)
library(C50)
mushroom_c5rules <- C5.0(type ~ odor + gill_size, data = mushrooms, rules = TRUE)
summary(mushroom_c5rules)

# Example-3
# Data: iris
# Compare C5.0 with CART
library(C50)
library(printr)

data(iris)
head(iris)
str(iris)
table(iris$Species)

set.seed(9850)
g <- runif(nrow(iris))
irisr <- iris[order(g),]
str(irisr)
m1 <- C5.0(irisr[1:100, -5], irisr[1:100,5])
m1
summary(m1)
plot(m1)
p1 <- predict(m1, irisr[101:150,])
p1

table(irisr[101:150,5], c5.0_predicted=p1)

# Analysis of prediction result 
CrossTable(irisr[101:150,5], p1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
# -----------------------------------------------------------------------------------
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)     # for CART  
library(rpart.plot) # for CART Plot
data(iris)
head(iris)
str(iris)
table(iris$Species)
set.seed(9850)
g <- runif(nrow(iris))
irisr <- iris[order(g),]
str(irisr)
m3 <- rpart(Species ~ ., data = irisr[1:100,], method="class")
m3
summary(m3)
rpart.plot(m3)
rpart.plot(m3, type=3, extra=101, fallen.leaves=T)
p3 <- predict(m3, irisr[101:150,],type="class")

table(irisr[101:150,5], rpart_predicted=p3)

# Example-4 
# Naive Training/Testing
library(rpart)
data(iris)
rpart.model <- rpart(Species~., data=iris, method="class")
print(rpart.model)

# And now we test on the same dataset. From this, we obtain a confusion matrix.
rcart.prediction <- predict(rpart.model, newdata=iris, type="class")
confusion.matrix <- table(iris$Species, rcart.prediction)
print(confusion.matrix)

# The resulting error is as follows:
accuracy.percent <- 100*sum(diag(confusion.matrix))/sum(confusion.matrix)
print(paste("accuracy:",accuracy.percent,"%"))

# Using k-fold cross-validation to train and test the model
library(plyr)
library(rpart)
set.seed(123)
form <- "Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"
folds <- split(iris, cut(sample(1:nrow(iris)),10))
errs <- rep(NA, length(folds))

for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  tmp.model <- rpart(form , train, method = "class")
  tmp.predict <- predict(tmp.model, newdata = test, type = "class")
  conf.mat <- table(test$Species, tmp.predict)
  errs[i] <- 1-sum(diag(conf.mat))/sum(conf.mat)
}
print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))

# k-fold cross-validation with C5.0
library(C50)
library(plyr)
errs.c50 <- rep(NA, length(folds))
form <- "Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"
folds <- split(iris, cut(sample(1:nrow(iris)),10))
for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  tmp.model <- C5.0(as.formula(form), train)
  tmp.predict <- predict(tmp.model, newdata=test)
  conf.mat <- table(test$Species, tmp.predict)
  errs.c50[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
}
print(sprintf("average error using k-fold cross validation 
              and C5.0 decision tree algorithm: %.3f percent", 100*mean(errs.c50)))

# Example-5 
# There is a direct way to visualize a c5.0 decision tree?
library(C50)
data(churn)
myTree = C5.0(x = churnTrain[, -20], y = churnTrain$churn)
summary(myTree)
plot(myTree)

# You can also use partykit to just display subtrees. 
# For example if you want to just show the left branch below the root 
# (starting from node 2) and the right branch below the root 
# (starting from node 33) you could do:

library("partykit")
myTree2 <- C50:::as.party.C5.0(myTree)
plot(myTree2[2])
plot(myTree2[33])

titanic <- read.csv("titanic_clean.csv", header = TRUE, sep = ",")
str(titanic)
titanic <- titanic[-1310,-1]
str(titanic)

# Use ggplot() to plot the distribution of sexes within the classes of the ship.
library(ggplot2)
ggplot(titanic,aes(x=factor(pclass),fill=factor(sex)))+
  geom_bar(position="dodge")

# Use ggplot() to estimate your chances of survival from the distribution 
# of sexes within the classes of the ship.
ggplot(titanic,aes(x=factor(pclass),fill=factor(sex)))+geom_bar(position="dodge")+
  facet_grid(". ~ survived")

# Use ggplot() to estimate your chances of survival based on your age 
# from the distribution of sexes within the classes of the ship.
posn.j <- position_jitter(0.5, 0)
ggplot(titanic,aes(x=factor(pclass),y=age,col=factor(sex)))+
  geom_jitter(size=3,alpha=0.5,position=posn.j)+
  facet_grid(". ~ survived")

# Import training and testing sets.
train <- read.csv("Titanic.train.csv")
test<- read.csv("Titanic.test.csv")
str(train)
str(test)

# Survival rates in absolute numbers
table(train$Survived)

# Survival rates in proportions
prop.table(table(train$Survived))

# Two-way comparison: Sex and Survived
table(train$Sex, train$Survived)

# Two-way comparison: row-wise proportions
prop.table(table(train$Sex,train$Survived),1)

# Create the column child, and indicate whether child or no child
train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0

# Two-way comparison
prop.table(table(train$Child,train$Survived),1)

# Copy of test
test_one <- test

# Initialize a Survived column to 0
test_one$Survived <- 0

# Set Survived to 1 if Sex equals "female"
test_one$Survived[test_one$Sex=="female"] <- 1

# Explore Decision Trees.
# Load in the R package  
#install.packages('rpart')
require(rpart)

# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

# Visualize the decision tree using plot() and text()
#plot(my_tree_two)
#text(my_tree_two)

# Load in the packages to build a fancy plot
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + 
                       SibSp + Parch + Fare + Embarked, data = train, method = "class")

# Time to plot your fancy tree
fancyRpartPlot(my_tree_two)

# Make predictions on the test set
my_prediction <- predict(my_tree_two, test, type = "class")

# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Use nrow() on my_solution
nrow(my_solution)
ncol(my_solution)

# Finish the write.csv() call
#write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
     data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))

# Visualize my_tree_three
fancyRpartPlot(my_tree_three)

# Create train_two
train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1

# Finish the command
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, 
                      data = train_two, method = "class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_four)

# New train and test sets.
# You have access to a new train and test set named train_new and test_new. 
# These data sets contain a new column with the name Title (referring to Miss, Mr, etc.). 
# Title is another example of feature engineering: 
# it's a new variable that possibly improves the model.
# Finish the command
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, 
                      data = train_new, method = "class")

# Visualize my_tree_five
fancyRpartPlot(my_tree_five)

# You must prepare train_new and test_new
# Make prediction
my_prediction <- predict(my_tree_five, test_new, type = "class")

# Make results ready for submission
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

install.packages("randomForest")
library(randomForest)
data(iris)
set.seed(71)
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.8, 0.2))
iris.rf <- randomForest(Species ~ ., data=iris[ind == 1,])
iris.pred <- predict(iris.rf, iris[ind == 2,])

table(observed = iris[ind==2, "Species"], predicted = iris.pred)

acc <- mean(iris.pred==iris[ind == 2,5])
cat("accuracy:", acc, "\n")

# A very basic introduction to Random Forests using R
# Data: iris in R.
# Let us split our data set then, shall we?
# Set random seed to make results reproducible:

set.seed(17)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(iris)/2)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(iris), size = data_set_size)
# Assign the data to the correct sets
training <- iris[indexes,]
validation1 <- iris[-indexes,]

#import the package
library(randomForest)
# Perform training:
rf_classifier = randomForest(Species ~ ., data=training, ntree=100, mtry=2, importance=TRUE)
rf_classifier

# Remember the importance parameter? 
# Let us take a look at the importance that our classifier has assigned to each variable:
  
varImpPlot(rf_classifier)

# Looks like we have a classifier that was properly trained and is 
# producing somewhat good predictions for our training set. 
# Shall we evaluate what happens when we try to use this classifier 
# to predict classes for our  validation1 set?

# Validation set assessment #1: looking at confusion matrix
prediction_for_table <- predict(rf_classifier,validation1[,-5])
table(observed=validation1[,5],predicted=prediction_for_table)


# The confusion matrix is a good way of looking at how good our 
# classifier is performing when presented with new data.
# Another way of assessing the performance of our classifier is to generate 
# a ROC curve and compute the area under the curve:

# Validation set assessment #2: ROC curves and AUC
# Needs to import ROCR package for ROC curve plotting:

install.packages("ROCR")
library(ROCR)
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rf_classifier,validation1[,-5],type="prob")
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
classes <- levels(validation1$Species)
# For each class
for (i in 1:3)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(validation1[,5]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}