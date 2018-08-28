getwd()
setwd("c:/MyR")
# Principal Component Analysis Using R
data("heptathlon", package = "HSAUR")
heptathlon

# To begin it will help to score all the seven events in the same direction, 
# so that ¡®large¡¯ values are ¡®good¡¯. We will recode the running events to achieve this; 

heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles 
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m 
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m 

round(cor(heptathlon[,-score]), 2)

# A principal component analysis of the data can be applied using the prcomp function.
# The result is a list containing the coefficients defining each component
# (sometimes referred to as loadings), the principal component scores, etc.
# The required code is (omitting the score variable)

heptathlon_pca <- prcomp(heptathlon[, -score], scale = TRUE) 
print(heptathlon_pca)
score <- which(colnames(heptathlon) == "score") 
plot(heptathlon[,-score])

# The summary method can be used for further inspection of the details
summary(heptathlon_pca)

# scree plot
screeplot(heptathlon_pca, type="lines", pch=1, main="scree plot")

# The linear combination for the first principal component is 
a1 <- heptathlon_pca$rotation[,1] 
a1

# We see that the 200m and long jump competitions receive the highest 
# weight but the javelin result is less important. 
# For computing the first principal component, the data need to be rescaled appropriately. 
# The center and the scaling used by prcomp internally can be extracted from the heptathlon_pca via. 

center <- heptathlon_pca$center 
scale <- heptathlon_pca$scale 

# Now, we can apply the scale function to the data and multiply with the loadings matrix 
# in order to compute the first principal component score for each competitor 

hm <- as.matrix(heptathlon[,-score]) 
drop(scale(hm, center = center, scale = scale) %*% + heptathlon_pca$rotation[,1])

# more conveniently, by extracting the ???rst from all precomputed principal components
predict(heptathlon_pca)[,1]

# The first two components account for 81% of the variance. 
# A barplot of each component¡¯s variance (see Figure 13.2) shows how the first two components dominate.
cor(heptathlon$score, heptathlon_pca$x[,1])
cor(heptathlon$score, heptathlon_pca$x[,2])
cor(heptathlon$score, heptathlon_pca$x[,3])


plot(heptathlon_pca)
biplot(heptathlon_pca, col = c("gray", "black"))
plot(heptathlon$score, heptathlon_pca$x[,1])


# How much variance does each component explain? 
# summary(hepPCA) gives us information on how much variance each component covers.

summary(heptathlon_pca)

## Factor Analysis
## Example 2: The following data show life expectancy in years by country, age, and sex.
# The data come from Keyfitz and Flieger (1971) and relate to life expectancies
# in the 1960s. 

######################################################################################################
######################################################################################################
##
## Entering the raw data for the life expectancy example:
##
"life" <- 
  structure(.Data = list(c(63., 34., 38., 59., 56., 62., 50., 65., 56., 69., 65., 64., 56., 60., 61., 49., 59., 63., 59., 65., 65., 64.,
                           64., 67., 61., 68., 67., 65., 59., 58., 57.)
                         , c(51., 29., 30., 42., 38., 44., 39., 44., 46., 47., 48., 50., 44., 44., 45., 40., 42., 44., 44., 48., 48., 63.,
                             43., 45., 40., 46., 45., 46., 43., 44., 46.)
                         , c(30., 13., 17., 20., 18., 24., 20., 22., 24., 24., 26., 28., 25., 22., 22., 22., 22., 23., 24., 28., 26., 21.,
                             21., 23., 21., 23., 23., 24., 23., 24., 28.)
                         , c(13., 5., 7., 6., 7., 7., 7., 7., 11., 8., 9., 11., 10., 6., 8., 9., 6., 8., 8., 14., 9., 7., 6., 8., 10., 8.,
                             8., 9., 10., 9., 9.)
                         , c(67., 38., 38., 64., 62., 69., 55., 72., 63., 75., 68., 66., 61., 65., 65., 51., 61., 67., 63., 68., 67., 68.,
                             68., 74., 67., 75., 74., 71., 66., 62., 60.)
                         , c(54., 32., 34., 46., 46., 50., 43., 50., 54., 53., 50., 51., 48., 45., 49., 41., 43., 48., 46., 51., 49., 47.,
                             47., 51., 46., 52., 51., 51., 49., 47., 49.)
                         , c(34., 17., 20., 25., 25., 28., 23., 27., 33., 29., 27., 29., 27., 25., 27., 23., 22., 26., 25., 29., 27., 25.,
                             24., 28., 25., 29., 28., 28., 27., 25., 28.)
                         , c(15., 6., 7., 8., 10., 14., 8., 9., 19., 10., 10., 11., 12., 9., 10., 8., 7., 9., 8., 13., 10., 9., 8., 10., 11.,
                             10., 10., 10., 12., 10., 11.)
  )
  , class = "data.frame"
  , names = c("m0", "m25", "m50", "m75", "w0", "w25", "w50", "w75")
  , row.names = c("Algeria", "Cameroon", "Madagascar", "Mauritius", "Reunion", "Seychelles", "South Africa(C)", "South Africa(W)",
                  "Tunisia", "Canada", "Costa Rica", "Dominican Rep", "El Salvador", "Greenland", "Grenada", "Guatemala",
                  "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Trinidad(62)", "Trinidad (67)", 
                  "United States (66)", "United States (NW66)", "United States (W66)", "United States (67)", "Argentina",
                  "Chile", "Columbia", "Ecuador")
  )
##
##

# A data set called life has been created.

factanal(life,factors=1, rotation="varimax")
# One factor is clearly not enough (tiny P-value).

factanal(life,factors=2, rotation="varimax")
# Two factors are clearly not enough (tiny P-value).

factanal(life,factors=3, rotation="varimax")
# Three factors may be enough (P-value = 0.458).

# How could we interpret the three factors?

# Saving the result as life.fa.3.v:

life.fa.3.v <- factanal(life,factors=3, rotation="varimax")

# The communalities:

1-life.fa.3.v$uniquenesses

# We see that m0, m50, w0, w25, w50 share very much of their variances with the other variables via the factors.
# m25 and m75 are more "unique".
# What does this mean?

# Estimating factor scores for the life expectancy data set:

life.fa.3.scores <- factanal(life,factors=3, rotation="varimax",scores="regression")$scores

#### Plotting the 3 factor scores for this data set using a 3-D plot:

# Creating a data frame with the original data and the scores:

life.df <- data.frame(life, life.fa.3.scores)
attach(life.df)

### Doing a 3-D plot:

library(lattice)  # loading the lattice package
cloud(Factor3 ~ Factor1 * Factor2, xlim=range(Factor1), ylim=range(Factor2), zlim=range(Factor3), 
      pch=row.names(life.df),
      scales = list(distance = rep(1, 3), arrows = FALSE))

# Looking back at the factor scores:

life.df[,c("Factor1","Factor2","Factor3")]

row.names(life.df)[order(Factor1)]
# Recall factor 1 basically measures life expectancy at birth.
# We see Madagascar and Cameroon have the smallest values for Factor 1.
# The U.S .and Canada have high values for Factor 1.

row.names(life.df)[order(Factor2)]
# Recall factor 2 basically measures life expectancy for older women.
# Cameroon also has a low score for this factor.

row.names(life.df)[order(Factor3)]
# Factor 3 reflects (mostly) life expectancy for older men.
# Cameroon has the lowest score for this factor.
# Algeria has the highest score for Factor 3.

### Doing separate 2-D scatterplots of the factor scores:

# Factor 2 vs. Factor 1:

plot(Factor1, Factor2, type='n', xlab='Factor 1 scores', ylab='Factor 2 scores')
text(Factor1, Factor2, labels = row.names(life.df), cex = 0.7 )

# Factor 3 vs. Factor 1:

plot(Factor1, Factor3, type='n', xlab='Factor 1 scores', ylab='Factor 3 scores')
text(Factor1, Factor3, labels = row.names(life.df), cex = 0.7 )

# Factor 3 vs. Factor 2:

plot(Factor2, Factor3, type='n', xlab='Factor 2 scores', ylab='Factor 3 scores')
text(Factor2, Factor3, labels = row.names(life.df), cex = 0.7 )

# -----------------------------------------------------------------------------------
sapply(1:3, function(f) factanal(life, factors = f, method ="mle")$PVAL)
factanal(life, factors = 3, method ="mle")

# The estimated factor scores are found as follows.
# (scores <- factanal(life, factors = 3, method = "mle", scores = "regression")$scores)
# We can use the scores to provide the plot of the data shown in Figure 5.1.
# Ordering along the first axis reflects life force at birth ranging from
# Cameroon and Madagascar to countries such as the USA. And on the third
# axis Algeria is prominent because it has high life expectancy amongst men
# at higher ages, with Cameroon at the lower end of the scale with a low life
# expectancy for men over 50.