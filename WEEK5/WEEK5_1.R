###    Overfitting in Machine Learning: What It Is and How to Prevent It    ###
###    Machine Learning Explained: Overfitting
###    Exercises-1

require(data.table)
library(rpart)
require(ggplot2)

set.seed(456)

## Reading data
overfitting_data=data.table(airquality)
str(overfitting_data)
overfitting_data

ggplot(overfitting_data,aes(Wind,Ozone))+geom_point()+ggtitle("Ozone vs wind speed")
data_test=na.omit(overfitting_data[,.(Wind,Ozone)])
train_sample=sample(1:nrow(data_test),size = 0.7*nrow(data_test))

## creation of polynomial models
degree_of_poly=1:20
degree_to_plot=c(1,3,5,10,20)
polynomial_model=list()
df_result=NULL
for (degree in degree_of_poly)
{
  fm=as.formula(paste0("Ozone~poly(Wind,",degree,",raw=T)"))
  polynomial_model=c(polynomial_model,list(lm(fm,data_test[train_sample])))
  Polynomial_degree=paste0(degree)
  data_fitted=tail(polynomial_model,1)[[1]]$fitted.values
  new_df=data.table(Wind=data_test[train_sample,Wind],Ozone_real=data_test[train_sample,Ozone],Ozone_fitted=tail(polynomial_model,1)[[1]]$fitted.values,degree=as.factor(degree))
  if (is.null(df_result))
    df_result=new_df
  else
    df_result=rbind(df_result,new_df)
}
gg=ggplot(df_result[degree%in%degree_to_plot],aes(x=Wind))+geom_point(aes(y=Ozone_real))+geom_line(aes(color=degree,y=Ozone_fitted))
gg+ggtitle('Ozone vs wind for several polynomial regressions')+ylab('Ozone')

## Computing SE
SE_train_list=c()
SE_test_list=c()

for (poly_mod in polynomial_model)
{
  print(summary(poly_mod))
  SE_train_list=c(SE_train_list,sqrt(mean(poly_mod$residuals^2)))
  SE_test=sqrt(mean((data_test[-train_sample]-predict(poly_mod,data_test[-train_sample,]))^2))
  SE_test_list=c(SE_test_list,SE_test)
}

# The red line shows the evolution of the error on the testing set and the black line on the training set. 
# As soon as more than 9 or 10 degrees are used the MSE seem to start growing and explodes 
# when there are even more degrees.

data_plot=data.table(SE_test_list,SE_train_list,degree_of_poly)
ggplot(data_plot[degree_of_poly<=8])+geom_line(aes(x=degree_of_poly,y=SE_test_list),color='red')+
  geom_line(aes(x=degree_of_poly,y=SE_train_list))+ylab('MSE')+xlab('Degrees of polynomial')

###############################################################################################################
###    Exercises-2
### Regression Model Validation: Cross-Validation Essentials in R

library(tidyverse)
library(caret)
# Load the data
data("swiss")
# Inspect the data
sample_n(swiss, 3)
str(swiss)

fit.con <- lm(swiss$Fertility~1, data=swiss)
fit <- lm(swiss$Fertility~., data=swiss) 

## Forward Selection
fit.forward <- step(fit.con,scope=list(lower=fit.con,upper=fit), direction = "forward")
fit.forward

## Backward Elimination 
fit.backward <- step(fit,scope=list(lower=fit.con,upper=fit), direction = "backward")
fit.backward

## Stepwise 
fit.both <- step(fit.con,scope=list(lower=fit.con,upper=fit), direction = "both")
fit.both

### Method1: The Validation set Approach
# Split the data into training and test set

set.seed(123)
training.samples <- swiss$Fertility %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- swiss[training.samples, ]
test.data <- swiss[-training.samples, ]

# Build the model: Compare two models
Fmodel <- lm(Fertility ~., data = train.data)
Smodel <- lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture, data = train.data)

# Make predictions and compute the R2, RMSE and MAE
Fpredictions <- Fmodel %>% predict(test.data)
Spredictions <- Smodel %>% predict(test.data)
data.frame( R2_Full = R2(Fpredictions, test.data$Fertility),
            RMSE_Full = RMSE(Fpredictions, test.data$Fertility),
            MAE_Full = MAE(Fpredictions, test.data$Fertility),
            R2_Stepwise = R2(Spredictions, test.data$Fertility),
            RMSE_Stepwise = RMSE(Spredictions, test.data$Fertility),
            MAE_Stepwise = MAE(Spredictions, test.data$Fertility))


### Method2: Leave one out cross validation - LOOCV
# Define training control
train.control <- trainControl(method = "LOOCV")

# Train the model
Fmodel <- train(Fertility ~., data = swiss, method = "lm",
                trControl = train.control)
Smodel <- train(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture, data = swiss, method = "lm",
                trControl = train.control)

# Summarize the results
print(Fmodel)
print(Smodel)


### Method3: K-fold cross-validation
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)

# Train the model
Smodel <- train(Fertility ~., data = swiss, method = "lm",
                trControl = train.control)
Fmodel <- train(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture, data = swiss, method = "lm",
                trControl = train.control)

# Summarize the results
print(Fmodel)
print(Smodel)


### Method4: Repeated K-fold cross-validation
# The process of splitting the data into k-folds can be repeated a number of times, 
# this is called repeated k-fold cross validation.
# The final model error is taken as the mean error from the number of repeats.
# The following example uses 10-fold cross validation with 3 repeats:

# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
Fmodel <- train(Fertility ~., data = swiss, method = "lm",
                trControl = train.control)
Smodel <- train(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture, data = swiss, method = "lm",
                trControl = train.control)

# Summarize the results
print(Fmodel)
print(Smodel)