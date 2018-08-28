getwd()
setwd("c:/MyR")

# Example for multiple independent or predictor variables. 
# First, load the data that we are going to use. It is a data set that comes with R.
# Regression for Murder using Population, Income, Illiteracy and Frost

state<-data.frame(state.x77)
data(state)
head(state)

states <- as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit <- lm(Murder~.,data = states) 
fit

plot(fit)

# Check: multicollinearity
fit <- lm(Murder~.,data = state)

install.packages("car")
library("car")

car::vif(fit) 
(car::vif(fit)) > 10

## Forward Selection
fit.con <- lm(Murder~1,data=state)
fit.forward <- step(fit.con,scope=list(lower=fit.con,upper=fit), direction = "forward")
fit.forward
summary(fit.forward)

## Backward Elimination 
fit.backward <- step(fit,scope=list(lower=fit.con,upper=fit), direction = "backward")
fit.backward
summary(fit.backward)

## Stepwise 
fit.both <- step(fit.con,scope=list(lower=fit.con,upper=fit), direction = "both")
fit.both
summary(fit.both)

# Calculate the value of criterion
AIC(fit.forward)
AIC(fit.backward)
AIC(fit.both)
BIC(fit.forward)
BIC(fit.backward)
BIC(fit.both)

# Point Estimation
pre_murder <- predict(fit.both, newdata = state) 
pre_murder <- as.data.frame(pre_murder)
pre_murder 

# Interval Estimation
pre_murder <- predict(fit.both, newdata = state, interval = "confidence") 
pre_murder <- as.data.frame(pre_murder)
pre_murder

# Comparison of two values
ttmp <- cbind(pre_murder,state$Murder) 
ttmp

# Variable Selection: All Possible Regression

install.packages("olsrr")
library("olsrr")

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
summary(model)
ols_step_all_possible(model)

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
k <- ols_step_all_possible(model)
plot(k)

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_step_best_subset(model)

model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
k <- ols_step_best_subset(model)
plot(k)

# stepwise forward regression
model <- lm(y ~ ., data = surgical)
ols_step_forward_p(model)
k_f <- ols_step_forward_p(model)
plot(k_f) 

# stepwise backward regression
model <- lm(y ~ ., data = surgical)
ols_step_backward_p(model)
k_b <- ols_step_backward_p(model)
plot(k_b) 

# stepwise regression
model <- lm(y ~ ., data = surgical)
ols_step_both_p(model)
k_s <- ols_step_both_p(model)
plot(k_s) 

#----------------------------------------------------------------------------
# stepwise aic forward regression
model <- lm(y ~ ., data = surgical)
ols_step_forward_aic(model)

kAIC_f <- ols_step_forward_aic(model)
plot(kAIC_f) 

# stepwise aic regression
model <- lm(y ~ ., data = surgical)
ols_step_both_aic(model)
kAIC_s <- ols_step_both_aic(model)
plot(kAIC_s) 