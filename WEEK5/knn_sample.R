#___________________________________________________#
#         knn example with iris dataset             #
#___________________________________________________#

# load libs
library(class) # for kNN

# train/test sampling
set.seed(123)
sampleIndex = sample(1:150, 100)

train = iris[sampleIndex, ]
test = iris[-sampleIndex, ]

train.ds = train[, -5]
train.cl = as.factor(train[, 5])
test.ds = test[, -5]
test.cl = as.factor(test[, 5])

# classification / prediction
knn.pred = knn(train = train.ds, test = test.ds, cl = train.cl, k = 3)
print(knn.pred)
print(test.cl)

# evaluation : accuracy
accuracy = mean(knn.pred == test.cl)
print(accuracy)

# confusion matrix
p = rbind(test.cl, knn.pred)
xtabs( ~ test.cl + knn.pred, data = p)

# plotting
par(mfrow = c(1, 2))
plot(test.ds$Petal.Length, test.ds$Petal.Width, col = test.cl, main = "real species") # real
plot(test.ds$Petal.Length, test.ds$Petal.Width, col = knn.pred, main = "predicted species") # real



