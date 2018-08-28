getwd()
setwd("c:/MyR")
# ANOVA
# let us start with a simple data set distributed with the base packages
# called InsectSprays. This data set shows the effectiveness of six different insecticides.
attach(InsectSprays)
data(InsectSprays)
str(InsectSprays)

# 1. Descriptive statistics
# a. Mean, variance, number of elements in each cell
# b. Visualise the data ??? boxplot; look at distribution, look for outliers
# We??ll use the tapply() function which is a helpful shortcut in processing data, basically
# allowing you to specify a response variable, a factor (or factors) and a function that should be
# applied to each subset of the response variable defined by each level of the factor. I.e. Instead
# of doing: 
mean(count[spray=="A"]) 
mean(count[spray=="B"]) 
mean(count[spray=="C"]) 
mean(count[spray=="D"]) 
mean(count[spray=="E"]) 
mean(count[spray=="F"]) 

# Let??s look at the means:
tapply(count, spray, mean) 

# The variances:
tapply(count, spray, var)

# And sample sizes
tapply(count, spray, length)

# And a boxplot:
boxplot(count ~ spray)

# How does the data look?
# Default order is alphabetical. R needs, for example, the control condition to be 1st for
# treatment contrasts to be easily interpreted.
# If they??re not automatically in the correct order ??? i.e. if they were ordered variables, but
# came out alphabetically (e.g. "Very.short","Short","Long","Very.long" or ??A??, ??B??,
#                          ??Control??), re-order the variables for ordered IV:
# To change to, for example, F < B < C < D < E < A, use:

Photoperiod<-ordered(spray,levels=c("F","B","C","D","E","A"))
tapply(count,Photoperiod,mean)

# If you want to check that a variable is a factor 
# (especially for variables with numbers as factor levels). 
# We use the is.factor directive to find this out
is.factor(spray)

# 2. Run 1-way ANOVA
# a. Oneway.test ( )
# Use, for example:
oneway.test(count~spray)


# Default is equal variances (i.e. homogeneity of variance) not assumed ??? i.e. Welch??s
# correction applied (and this explains why the denom df 
# (which is normally k*(n-1)) is not a whole number in the output)
# To change this, set "var.equal=" option to TRUE


# Oneway.test( ) corrects for non-homogeneity, but doesn??t give much information ??? i.e.
# just F, p-value and dfs for numerator and denominator ??? no MS etc.
# b. Run an ANOVA using aov( )

# Use this function and store output and use extraction functions to extract what you need.
aov.out = aov(count ~ spray, data=InsectSprays)
summary(aov.out)

# 3. Post Hoc tests
# ??? Tukey HSD(Honestly Significant Difference) is default in R
TukeyHSD(aov.out)

# 4. Contrasts
# NB: ANOVA and linear regression are the same thing ??? more on that tomorrow. For the
# moment, the main point to note is that you can look at the results from aov() in terms of the
# linear regression that was carried out, i.e. you can see the parameters that were estimated.
 
summary.lm(aov.out)

# Implicitly this can be understood as a set of (non-orthogonal) contrasts of the first group
# against each of the other groups. R uses these so-called ??Treatment?? contrasts as the default,
# but you can request alternative contrasts (see later) 

# 5. Test assumptions
# a. Homogeneity of variance
bartlett.test(count ~ spray, data=InsectSprays)

# b. Model checking plots
# the aov command prepares the data for these plots
plot(aov.out)

# 6. Non-parametric alternative to ANOVA:
# Kruskal-Wallis rank sum test
kruskal.test(count ~ spray, data=InsectSprays)

##################################################################################################
# Two-way (between-groups) ANOVA in R
# ANOVA with R: analysis of the diet dataset
# The data set contains information on 76 people who undertook one of three diets 
# (referred to as diet A, B and C). 
# There is background information such as age, gender, and height. 
# The aim of the study was to see which diet was best for losing weight.
# 
# Section 1: importation and descriptive analysis
# Lets starts by importing the data set diet with the function read.csv()
# defining a new column weight.loss, corresponding to the difference between the initial and final weights 
# (respectively the corresponding to the columns initial.weight and final.weight of the dataset)
# displaying weight loss per diet type (column diet.type) by means of a boxplot.

dietR = read.csv("c:/MyR/diet.csv",row.names=1)
attach(dietR) 
dietR$weightlost<-pre.weight-weight6weeks 
attach(dietR)
boxplot(weightlost~Diet,data=dietR,col="light gray", ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="blue")

# Steps in R To carry out a two way ANOVA with an interaction, 
# use aov(dependent~as.factor(independent1)*as.factor(indepndent2),data= filename) 
# and give the ANOVA model a name e.g. anova2.  
# The as.factor() tells R that the two independents are categorical. 
anova2<-aov(weightlost~ as.factor(gender)*as.factor(Diet),data=dietR)
anova2

# Ask for the residuals (difference between each individual and their diet/gender combination mean) 
# and give them a name (res).
res<-anova2$residuals 

# Produce a histogram of the residuals. 
hist(res,main="Histogram of residuals",xlab="Residuals") 

# The Levene's test for equality of variances is in the additional ??car?? package.   
library(car)
leveneTest(weightlost~ as.factor(gender)*as.factor(Diet),data=dietR) 

# The p value is 0.8563 which is greater than 0.05, so equal variances can be assumed.
# To view the ANOVA table use the summary() command summary(anova2) 
summary(anova2) 

# The results of the two-way ANOVA and post hoc tests are reported in the same way 
# as one way ANOVA for the main effects and the interaction e.g. 
# there was a statistically significant interaction between the effects of Diet and 
# Gender on weight loss                   
# [F(2, 70)=3.153, p = 0.049].  
# Since the interaction effect is significant (p = 0.049), 
# the ??Diet?? effect cannot be generalised for both males and females together. 

# The easiest way to interpret the interaction is to use a means or interaction plot 
# which shows the means for each combination of diet and gender 
# (see the Interactions resource for more details).  
# Before producing an interaction plot, tell R the labels for gender. 

gender<-factor(dietR$gender,c(0,1), labels=c('Female','Male')) 
# To produce this interaction plot use: interaction.
interaction.plot(Diet,gender,weightlost,type="b",col=c(2:3),leg.bty="o ",
                 leg.bg="beige",lwd=2,pch=c(18,24) ,xlab="Diet",ylab="Weight lost",main="Interaction plot") 

# Post hoc tests 
TukeyHSD(anova2) 

#############################################################################################################
# Factorial ANOVA: Main Effects, Interaction Effects, and Interaction Plots
# The packages used in this example include:
# * psych
# * car
# * multcompView
# * lsmeans
# * FSA
# * ggplot2
# * phia

if(!require(car)){install.packages("car")}
if(!require(psych)){install.packages("psych")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(phia)){install.packages("phia")}

Input =("
Diet    Country  Weight_change
        A       USA      0.120
        A       USA      0.125
        A       USA      0.112
        A       UK       0.052
        A       UK       0.055
        A       UK       0.044
        B       USA      0.096
        B       USA      0.100
        B       USA      0.089
        B       UK       0.025
        B       UK       0.029
        B       UK       0.019
        C       USA      0.149
        C       USA      0.150
        C       USA      0.142
        C       UK       0.077
        C       UK       0.080
        C       UK       0.066
        ")

Data = read.table(textConnection(Input),header=TRUE)


### Order levels of the factor; otherwise R will alphabetize them
Data$Country = factor(Data$Country,
                      levels=unique(Data$Country))


###  Check the data frame
library(psych)
headTail(Data)
str(Data)
summary(Data)

### Remove unnecessary objects
rm(Input)

interaction.plot(x.factor     = Data$Country,
                 trace.factor = Data$Diet, 
                 response     = Data$Weight_change, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

# Specify the linear model and conduct an analysis of variance
# The ANOVA table indicates that the main effects are significant, but that the interaction effect is not.
model = lm(Weight_change ~ Country + Diet + Country:Diet, data = Data)
library(car)
Anova(model, type = "II")

# Extended example with additional country
# The hypothetical data have been amended to include a third country, New Zealand.
Input =("
Diet    Country  Weight_change
        A       USA      0.120
        A       USA      0.125
        A       USA      0.112
        A       UK       0.052
        A       UK       0.055
        A       UK       0.044
        A       NZ       0.080
        A       NZ       0.090
        A       NZ       0.075
        B       USA      0.096
        B       USA      0.100
        B       USA      0.089
        B       UK       0.025
        B       UK       0.029
        B       UK       0.019
        B       NZ       0.055
        B       NZ       0.065
        B       NZ       0.050
        C       USA      0.149
        C       USA      0.150
        C       USA      0.142
        C       UK       0.077
        C       UK       0.080
        C       UK       0.066
        C       NZ       0.055
        C       NZ       0.065
        C       NZ       0.050 
        C       NZ       0.054
        ")

Data = read.table(textConnection(Input),header=TRUE)

### Order levels of the factor; otherwise R will alphabetize them
Data$Country = factor(Data$Country,levels=unique(Data$Country))

###  Check the data frame
library(psych)
headTail(Data)
str(Data)
summary(Data)

### Remove unnecessary objects
rm(Input)

# Simple interaction plot
interaction.plot(x.factor     = Data$Country,
                 trace.factor = Data$Diet, 
                 response     = Data$Weight_change, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

# Specify the linear model and conduct an analysis of variance
# The ANOVA table indicates that the interaction effect is significant, as are both main effects.

model = lm(Weight_change ~ Country + Diet + Country:Diet, data = Data)
library(car)
Anova(model, type = "II")


# Interaction plot with error bars using ggplot2
### Create a data frame called Sum with means and standard deviations 

library(FSA) 
Sum = Summarize(Weight_change ~ Country + Diet, data=Data, digits=3)

### Add standard error of the mean to the Sum data frame 
Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)
Sum

### Order levels of the factor; otherwise R will alphabetize them 
Sum$Country = factor(Sum$Country, levels=unique(Sum$Country))


### Produce interaction plot 
library(ggplot2)

pd = position_dodge(.2)
ggplot(Sum, aes(x = Country, y = mean, color = Diet)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2, size=0.7, position=pd) +   
  geom_point(shape=15, size=4, position=pd) + theme_bw() +   
  theme(axis.title = element_text(face = "bold")) + 
  scale_color_manual(values= c("black", "red", "green"))+ylab("Mean weight change") 
  
### You may see an error, "ymax not defined"
###  In this case, it does not appear to affect anything

# Interaction plot with the phia package
# The phia package can be used to create interaction plots quickly.  
# By default the error bars indicate standard error of the means.  
# For additional options, see ?interactionMeans.


library(phia)
IM = interactionMeans(model)
IM

### Produce interaction plot
plot(IM)

### Return the graphics device to its default 1-plot-per-window state
par(mfrow=c(1,1))