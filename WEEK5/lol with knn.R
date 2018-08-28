# - load dataset, simple preprocessing and merge
rm(list = ls())
gold.dataset = read.csv("gold.csv")
gold.dataset = gold.dataset[gold.dataset$Type == "golddiff", ]

lol.dataset = read.csv("LeagueofLegends.csv")
lol.dataset.sub = lol.dataset[, c("bResult", "Address")]

target = merge(gold.dataset, lol.dataset.sub, by = "Address")
target = target[, -c(1,2)]

target$bResult = as.factor(target$bResult)
#write.csv(target, "lol_classification_exm.csv")
library(class)

#- sampling
trainIndex = sample(1:nrow(target), 5000)

train = target[trainIndex, ]
test = target[-trainIndex, ]

train.ds = train[, -ncol(train)]
train.cl = train[, ncol(train)]

test.ds = test[, -ncol(test)]
test.cl = test[, ncol(test)]

#- knn classification and prediction
knn.pred = knn(train = train.ds, test = test.ds, cl = train.cl, k = 3)

# evaluation
accuracy = mean(knn.pred == test.cl)
accuracy

# confusion matrix
predict.result = rbind(test.cl, knn.pred)
xtabs( ~ test.cl + knn.pred, predict.result)

library(caret)
cm = confusionMatrix(knn.pred, test.cl)
cm$table


