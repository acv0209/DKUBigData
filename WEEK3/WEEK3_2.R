getwd()
setwd("c:/MyR")
ds = read.csv("6_correlation_and_regression.csv")
attach(ds)
head(ds)
plot(waistline,BMI)
cor(waistline,BMI)
result = lm(BMI~waistline)
summary(result)

par(mfrow=c(2,2))
plot(result)

fitted(result)[1:10]  #prediction
predict(result, newdata=data.frame(waistline=90))  #prediction
detach(ds)

# Another Example
# We will use the cars dataset that comes with R by default. 
# cars is a standard built-in dataset, that makes it convenient to demonstrate 
# linear regression in a simple and easy to understand fashion. 
# You can access this dataset simply by typing in cars in your R console. 
# You will find that it consists of 50 observations(rows) and 2 variables (columns) ??? dist and speed. 
# Lets print out the first six observations here.

head(cars)  # display the first 6 observations

# Scatter Plot
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot

# BoxPlot: Check for outliers
# box plot for 'speed'
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  

# box plot for 'distance'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  

# Density plot: Check if the response variable is close to normality
# density plot for 'speed'

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  
polygon(density(cars$speed), col="red")

# density plot for 'dist'
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  
polygon(density(cars$dist), col="red")

# Correlation: calculate correlation between speed and distance 
cor(cars$speed, cars$dist)  

# # Build Linear Model
# The function used for building linear models is lm(). 
# The lm() function takes in two main arguments, namely: 1. Formula 2. Data. 
# The data is typically a data.frame and the formula is a object of class formula. 
# But the most common convention is to write out the formula directly in place of the argument as written below.

# build linear regression model on full data
linearMod <- lm(dist ~ speed, data=cars)  
print(linearMod)

# Linear Regression Diagnostics
summary(linearMod)  # model summary

# The p Value: Checking for statistical significance
modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
t_value 
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
p_value 
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
f
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
model_p

# AIC and BIC
# The Akaike information criterion - AIC (Akaike, 1974) and 
# the Bayesian information criterion - BIC (Schwarz, 1978) are measures 
# of the goodness of fit of an estimated statistical model 
# and can also be used for model selection. 
# Both criteria depend on the maximized value of the likelihood function L for the estimated model.
AIC(linearMod)  # AIC 
BIC(linearMod)  # BIC 

# Predicting Linear Models

# Step 1: Create the training (development) and test (validation) data samples from original data.
# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# Step 2: Develop the model on the training data and use it to predict the distance on test data
# Build the model on training data -
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

# Step 3: Review diagnostic measures.
summary (lmMod)  # model summary

# Step 4: Calculate prediction accuracy and error rates
# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
min_max_accuracy
mape

# k- Fold Cross validation
# Suppose, the model predicts satisfactorily on the 20% split (test data), 
# is that enough to believe that your model will perform equally well all the time? 
# It is important to rigorously test the model??s performance as much as possible. 
# One way is to ensure that the model equation you have will perform well, 
# when it is ??built?? on a different subset of training data and predicted on the remaining data.

install.packages("DAAG")
library(DAAG)

# performs the CV
cvResults <- suppressWarnings(CVlm(data=cars, form.lm=dist ~ speed, m=5, dots=FALSE, seed=29, 
    legend.pos="topleft",  printit=FALSE, 
    main="Small symbols are predicted values while bigger ones are actuals."))  
# => 251.2783 mean squared error
attr(cvResults, 'ms') 