getwd()
setwd("c:/MyR")
cu<-read.csv("thakali_cu_ec50s.csv")
names(cu)
mean(cu$ec50.cu)

# Summary statistics 
median(cu$ec50.cu)
sd(cu$ec50.cu)
var(cu$ec50.cu)
summary(cu)

# Example 1: Simple histogram
Temperature <- airquality$Temp
hist(Temperature)

# Example 2: Histogram with added parameters
# histogram with added parameters
hist(Temperature,
     main="Maximum daily temperature at La Guardia Airport",
     xlab="Temperature in degrees Fahrenheit",
     xlim=c(50,100), col="darkmagenta", freq=FALSE)

hist(Temperature,
     main="Maximum daily temperature at La Guardia Airport",
     xlab="Temperature in degrees Fahrenheit",
     xlim=c(40,110), ylim=c(0.00,0.07), col="skyblue", freq=FALSE)

# Return Value of hist()
h <- hist(Temperature)
h

# Example 3: Use Histogram return values for labels using text()
h <- hist(Temperature,ylim=c(0,40))
text(h$mids, h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Example 4: Histogram with different breaks
hist(Temperature, breaks=4, main="With breaks=4")
hist(Temperature, breaks=20, main="With breaks=20")

# Example 5: Histogram with non-uniform width
hist(Temperature, main="Maximum daily temperature at La Guardia Airport",
     xlab="Temperature in degrees Fahrenheit", xlim=c(50,100), col="skyblue",
     border="brown", breaks=c(55,60,70,75,80,85,90,95,100))

# LakeHuron data set, which has data on the water level of the lake over almost 100 years.
huron<-LakeHuron
hist(huron)
hist(huron, breaks=20, col="lightblue", main="Lake Huron level", xlab="Level (m)")

# If you have a large data set, and are interested in the distribution of the data, try plotting the output
# from the density function, which returns estimates of the probability density.
plot(density(huron),col='blue')

# For data sets with a small number of observations, the stripchart function is a good choice
cu<-read.csv("thakali_cu_ec50s.csv")
cu
stripchart(cu$ec50.cu)

# For data sets with much overlap, try method='jitter' or method='stack'. The stripchart
# function can also plot a response for multiple levels of a factor. For example, let's take a look at
# some data on soil respiration in Alaskan ecosystems.

gas<-read.csv("gas_emis.csv")
summary(gas)
stripchart(CO2 ~ tmt, data=gas, method='jitter', jitter=0.3)

# The boxplot function is an alternative.
boxplot(CO2 ~ tmt, data=gas)

# Beanplots may be a more informative alternative to boxplots.
install.packages("beanplot")
library(beanplot)
beanplot(CO2 ~ tmt, data=gas)
beanplot(CH4 ~ ecosystem, data=gas)

# More Strip Chart
# Example 1: Strip chart of daily air quality
str(airquality)
stripchart(airquality$Ozone)

# Example 2: Strip chart of airquality using jitter
stripchart(airquality$Ozone,
           main="Mean ozone in parts per billion at Roosevelt Island",
           xlab="Parts Per Billion",
           ylab="Ozone",
           method="jitter",
           col="purple",
           pch=1)

# dataset airquality which has ¡°Daily air quality measurements in New York, May to September 1973.¡±
boxplot(airquality$Ozone)
boxplot(airquality$Ozone,
        main = "Mean ozone in parts per billion at Roosevelt Island",
        xlab = "Parts Per Billion", ylab = "Ozone", 
        col = "orange", border = "brown", horizontal = TRUE, notch = TRUE)
b <- boxplot(airquality$Ozone)
b

# prepare the data
ozone <- airquality$Ozone
temp <- airquality$Temp

# gererate normal distribution with same mean and sd
ozone_norm <- rnorm(200,mean=mean(ozone, na.rm=TRUE), sd=sd(ozone, na.rm=TRUE))
temp_norm <- rnorm(200,mean=mean(temp, na.rm=TRUE), sd=sd(temp, na.rm=TRUE))
boxplot(ozone, ozone_norm, temp, temp_norm,
        main = "Multiple boxplots for comparision",
        at = c(1,2,4,5), names = c("ozone", "normal", "temp", "normal"),
        las = 2, col = c("orange","red"), border = "brown",
        horizontal = TRUE, notch = TRUE)
boxplot(Temp~Month, data=airquality, main="Different boxplots for each month", 
        xlab="Month Number", ylab="Degree Fahrenheit", col="orange", border="brown")

# One way to assess the normality of the distribution of a given variable is with a quantile-quantile plot. 
# This plot shows data values vs. quantiles based on a normal distribution. 
qqnorm(cu$ec50.cu)
qqline(cu$ec50.cu)

# There seems to be some deviation from normality here. 
# A common distribution for toxicity data is log-normal???let¡¯s see if this distribution works any better.
cu$l.ec50.cu<-log(cu$ec50.cu)
qqnorm(cu$l.ec50.cu)
qqline(cu$l.ec50.cu)


# While R does not have a cumulative probability function in its base packages, 
# it available in at least one package on CRAN, and it is easy to write your own function. 
# We can make a simple cumulative probability plot with two lines of code.
x<-sort( cu$ec50.cu)
plot(qnorm(ppoints(x)),x,log='y', xaxt="n",xlab="Cumulative probability (%)",ylab="Log Cu EC50")
axis(1,qnorm(c(0.1,0.25,0.5,0.75,0.9)),labels=c(10,25,50,75,90), las=1,tcl=0.3,mgp=c(0,0.2,0))

# R will return the quantiles of a given data set with the quantile function. Note that there are nine
# different algorithms available for doing this???you can find descriptions in the help file for
# quantile.
quantile(cu$l.ec50.cu)
quantile(cu$l.ec50, 0.05)

# Bar plots can be created in R using the barplot() function.
max.temp <- c(22, 27, 26, 24, 23, 26, 28)
barplot(max.temp)

# We can also plot bars horizontally by providing the argument horiz = TRUE.
# barchart with added parameters
barplot(max.temp,
        main = "Maximum Temperatures in a Week",
        xlab = "Degree Celsius",
        ylab = "Day",
        names.arg = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
        col = "green",
        horiz = TRUE)

# Plotting Categorical Data
age <- c(17,18,18,17,18,19,18,16,18,18)
table(age)
barplot(table(age),
  main="Age Count of 10 Students", xlab="Age", ylab="Count", border="red", col="blue", density=10)

# How to plot higher dimensional tables?
# This data set provides information on the fate of passengers on the fatal 
# maiden voyage of the ocean liner ¡®Titanic¡¯, summarized according to economic status (class), 
# sex, age and survival.-R documentation.
Titanic

# We can see that this data has 4 dimensions, class, sex, age and survival. 
# Suppose we wanted to bar plot the count of males and females.
# In this case we can use the margin.table() function. 
# This function sums up the table entries according to the given index.
margin.table(Titanic,1)  # count according to class
margin.table(Titanic,4)  # count according to survival
margin.table(Titanic)  # gives total count if index is not provided

# Now that we have our data in the required format, we can plot, survival for example, as 
barplot(margin.table(Titanic,4)) 
# plot male vs female count as 
barplot(margin.table(Titanic,2))

# Simple Pie Chart
slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls, main="Pie Chart of Countries")

# Pie Chart with Percentages
slices <- c(10, 12, 4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")

# 3D Exploded Pie Chart
install.packages("plotrix")
library(plotrix)
slices <- c(10, 12, 4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of Countries ")

# Pie Chart from data frame with Appended Sample Sizes
mytable <- table(iris$Species)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
    main="Pie Chart of Species\n (with sample sizes)")

# Scatterplots
# Simple Scatterplot
attach(mtcars)
plot(wt, mpg, main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

# Add fit lines
abline(lm(mpg~wt), col="red") # regression line (y~x) 
lines(lowess(wt,mpg), col="blue") # lowess line (x,y)

# Enhanced Scatterplot of MPG vs. Weight 
# by Number of Car Cylinders 
install.packages("car")
library(car) 

# Basic Scatterplot Matrix
pairs(~mpg+disp+drat+wt,data=mtcars, 
      main="Simple Scatterplot Matrix")

# Scatterplot Matrices from the glus Package 
install.packages("gclus")
library(gclus)
dta <- mtcars[c(1,3,5,6)] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

# High Density Scatterplot with Binning
install.packages("hexbin")
library(hexbin)
x <- rnorm(1000)
y <- rnorm(1000)
bin<-hexbin(x, y, xbins=50) 
plot(bin, main="Hexagonal Binning")

# 3D Scatterplot
install.packages("scatterplot3d")
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt,disp,mpg, main="3D Scatterplot")

# 3D Scatterplot with Coloring and Vertical Drop Lines
library(scatterplot3d) 
attach(mtcars) 
scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE, type="h", main="3D Scatterplot")

# 3D Scatterplot with Coloring and Vertical Lines and Regression Plane 
library(scatterplot3d) 
attach(mtcars) 
s3d <-scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE, type="h", main="3D Scatterplot")
fit <- lm(mpg ~ wt+disp) 
s3d$plane3d(fit)

# Spinning 3d Scatterplot
install.packages("rgl")
library(rgl)
plot3d(wt, disp, mpg, col="red", size=3)

# You can perform a similar function with the scatter3d(x, y, z) in the Rcmdr package.
# Another Spinning 3d Scatterplot
library(Rcmdr)
attach(mtcars)
scatter3d(wt, disp, mpg)

# Produce a stem and leaf display using the stem command in R.
redpine = c(42, 23, 43, 34, 49, 56, 31, 47, 61, 54, 46, 34, 26)
redpine
length(redpine)
stem(redpine)

# Stem and leaf plot can be scaled to have more stems by changing the scale option.
stem(redpine, scale = 2)

hist(redpine, prob = TRUE, main = "Heights (cm) of red pine seedlings")
hist(redpine, breaks = 100, 
     main = "dot plot of red pine seedling heights (cm)")

summary(redpine)
sd(redpine)
IQR(redpine)
diff(range(redpine))
var(redpine)
sd(redpine)^2
c(SD=sd(redpine), IQR=IQR(redpine), Range=diff(range(redpine)))

watershed = c(0.3, 0.3, 0.4, 0.5, 0.5, 1.3, 1.6, 1.8, 2.6, 2.6, 2.6, 4.7, 9.1, 9.1, 12.9, 19.4, 33.7, 176.1)
stem(watershed)
logshed = log10(watershed)
stem(logshed)
stem(log10(watershed))

# Line Charts
# Overview
# Line charts are created with the function lines(x, y, type=) 
# where x and y are numeric vectors of (x,y) points to connect. type= can take the following values:
# 
# type	description
# p	points
# l	lines
# o	overplotted points and lines
# b, c	points (empty if "c") joined by lines
# s, S	stair steps
# h	histogram-like vertical lines
# n	does not produce any points or lines

x <- c(1:5); y <- x # create some data 
par(pch=22, col="red") # plotting symbol and color 
par(mfrow=c(2,4)) # all plots on one page 
opts = c("p","l","o","b","c","s","S","h") 
for(i in 1:length(opts)){heading = paste("type=",opts[i]) 
  plot(x, y, type="n", main=heading) 
  lines(x, y, type=opts[i])}

x <- c(1:5); y <- x # create some data
par(pch=22, col="blue") # plotting symbol and color 
par(mfrow=c(2,4)) # all plots on one page 
opts = c("p","l","o","b","c","s","S","h") 
for(i in 1:length(opts)){heading = paste("type=",opts[i]) 
  plot(x, y, main=heading) 
  lines(x, y, type=opts[i])}

# Create Line Chart
# convert factor to numeric for convenience 
Orange$Tree <- as.numeric(Orange$Tree) 
ntrees <- max(Orange$Tree)

# get the range for the x and y axis 
xrange <- range(Orange$age) 
yrange <- range(Orange$circumference) 

# set up the plot 
plot(xrange, yrange, type="n", xlab="Age (days)",
     ylab="Circumference (mm)" ) 
colors <- rainbow(ntrees) 
linetype <- c(1:ntrees) 
plotchar <- seq(18,18+ntrees,1)

# add lines 
for (i in 1:ntrees) { 
  tree <- subset(Orange, Tree==i) 
  lines(tree$age, tree$circumference, type="b", lwd=1.5,
        lty=linetype[i], col=colors[i], pch=plotchar[i]) 
} 

# add a title and subtitle 
title("Tree Growth", "example of line plot")

# add a legend 
legend(xrange[1], yrange[2], 1:ntrees, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Tree")