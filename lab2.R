df = read.csv("hepatitis.data", header=FALSE)
df
for (i in colnames(df)){
  df[[i]][df[[i]] == "?"] <- NA
  df[[i]] <- as.numeric(df[[i]])
  df[[i]][is.na(df[[i]])] <- as.integer(mean(df[[i]], na.rm=T))
}
df

Type <- interaction(as.factor(df$V9), as.factor(df$V10))
data <- data.frame(df, Type )
colnames(data) <- c(colnames(df), "Type")
str(data)
ind <- sample(2, nrow(df), replace=T)
train <- data[ind == 1, ]
test <- data[ind == 2, ]
str(train)
str(test)

barplot(test$V1, horiz=FALSE, col=c("blue", "green", "red", "yellow")[test$Type])
legend("topleft", pch=19, col=c("blue", "green", "red", "yellow"), c("1.1", "2.1", "1.2", "2.2"), cex=0.8,
       title="Type")

library(e1071)
library(caTools)
library(caret)

classifier_cl <- naiveBayes(Type ~., data=train)
prediction <- predict(classifier_cl, newdata=test)

con.mat <- table(test$Type, prediction)
confusionMatrix(con.mat)

par(mfrow=c(1,2))
barplot(test$V1, horiz=FALSE, col=c("blue", "green", "red", "yellow")[test$Type])
legend("topleft", pch=19, col=c("blue", "green", "red", "yellow"), c("1.1", "2.1", "1.2", "2.2"), cex=0.8,
       title="Type")
barplot(test$V1, horiz=FALSE, col=c("blue", "green", "red", "yellow")[prediction])
legend("topleft", pch=19, col=c("blue", "green", "red", "yellow"), c("1.1", "2.1", "1.2", "2.2"), cex=0.8,
       title="Type")

library(class)

classifier_knn <- knn(train, test, cl=train$Type, k=1)
con.mat.knn <- table(test$Type, classifier_knn)
confusionMatrix(con.mat.knn)

classifier_knn <- knn(train, test, cl=train$Type, k=3)
con.mat.knn <- table(test$Type, classifier_knn)
confusionMatrix(con.mat.knn)

classifier_knn <- knn(train, test, cl=train$Type, k=5)
con.mat.knn <- table(test$Type, classifier_knn)
confusionMatrix(con.mat.knn)

classifier_knn <- knn(train, test, cl=train$Type, k=7)
con.mat.knn <- table(test$Type, classifier_knn)
confusionMatrix(con.mat.knn)

classifier_knn <- knn(train, test, cl=train$Type, k=10)
con.mat.knn <- table(test$Type, classifier_knn)
confusionMatrix(con.mat.knn)

par(mfrow=c(1,2))
barplot(test$V1, horiz=FALSE, col=c("blue", "green", "red", "yellow")[test$Type])
legend("topleft", pch=19, col=c("blue", "green", "red", "yellow"), c("1.1", "2.1", "1.2", "2.2"), cex=0.8,
       title="Type")
barplot(test$V1, horiz=FALSE, col=c("blue", "green", "red", "yellow")[classifier_knn])
legend("topleft", pch=19, col=c("blue", "green", "red", "yellow"), c("1.1", "2.1", "1.2", "2.2"), cex=0.8,
       title="Type")

library(tidyverse)
library(nnet)

model <- nnet::multinom(Type ~., data=train)
classifier_log <-model %>% predict(test)
con.mat.log <- table(test$Type, classifier_log)
confusionMatrix(con.mat.log)

par(mfrow=c(1,2))
barplot(test$V1, horiz=FALSE, col=c("blue", "green", "red", "yellow")[test$Type])
legend("topleft", pch=19, col=c("blue", "green", "red", "yellow"), c("1.1", "2.1", "1.2", "2.2"), cex=0.8,
       title="Type")
barplot(test$V1, horiz=FALSE, col=c("blue", "green", "red", "yellow")[classifier_log])
legend("topleft", pch=19, col=c("blue", "green", "red", "yellow"), c("1.1", "2.1", "1.2", "2.2"), cex=0.8,
       title="Type")


svm_lin = svm(Type ~ ., data=train, kernel="linear")
clas.svm_lin = predict(svm_lin, test)
con.mat.svm_lin <- table(test$Type, clas.svm_lin)
confusionMatrix(con.mat.svm_lin)

svm_rad = svm(Type ~ ., data=train, kernel="radial")
clas.svm_rad = predict(svm_rad, test)
con.mat.svm_rad <- table(test$Type, clas.svm_rad)
confusionMatrix(con.mat.svm_rad)

svm_pol = svm(Type ~ ., data=train, kernel="polynomial")
clas.svm_pol = predict(svm_pol, test)
con.mat.svm_pol <- table(test$Type, clas.svm_pol)
confusionMatrix(con.mat.svm_pol)

svm_sig = svm(Type ~ ., data=train, kernel="sigmoid")
clas.svm_sig = predict(svm_sig, test)
con.mat.svm_sig <- table(test$Type, clas.svm_sig)
confusionMatrix(con.mat.svm_sig)

library(rpart)

dtree <- rpart(Type ~ ., data=train)
rpart.plot::rpart.plot(dtree)
clas.dtree <- predict(dtree, test, type = "class")
con.mat.dtree <- table(test$Type, clas.dtree)
confusionMatrix(con.mat.dtree)

library(randomForest)

rforest <- randomForest(x=train, y=train$Type)
clas.rforest = predict(rforest, newdata=test)
con.mat.rforest <- table(test$Type, clas.rforest)
confusionMatrix(con.mat.rforest)

library(maboost)

model_boost = maboost::maboost(Type ~ ., data=train)
clas.boost = predict(model_boost, test)
con.mat.boost <- table(test$Type, clas.boost)
confusionMatrix(con.mat.boost)



