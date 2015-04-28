library(randomForest)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
library(extraTrees)

#
set.seed(42)

# Random forest classifier on each cover type
accuracy <- rep(0, 8)
est <- data.frame(matrix(rep(0, (dim(train)[1] * 8)), ncol=8))
names(est)[8] = "Cover_Type"

accuracy_xval <- rep(0, 8)
est_xval <- data.frame(matrix(rep(0, (dim(xval)[1] * 8)), ncol=8))
names(est_xval)[8] = "Cover_Type"

pred <- data.frame(matrix(rep(0, (dim(test)[1] * 8)), ncol=8))
for (i in c(1:7)) {
  fit <- randomForest( train[,57 + i] ~ ., data = train[, c(2:11, 16:55, 57)], 
                       importance = TRUE, ntree = 100, maxnodes=100)
  varImpPlot(fit)
  est[, i] <- predict(fit, train)
  accuracy[i] <- sum(predict(fit, train) == train[, 57 + i]) / dim(train)[1]
  
  names(est_xval)[i] = paste("Cover_Type", i, sep="")
  est_xval[, i] <- predict(fit, xval)
  accuracy_xval[i] <- sum(est_xval[, i] == xval[, 57 + i]) / dim(xval)[1]
  
  pred[, i] <- predict(fit, test)
}

est_factor <- est
est <- apply(est, 2, as.numeric)
est_xval_factor <- est_xval
est_xval <-apply(est_xval, 2, as.numeric)
pred_factor <- pred
pred <- apply(pred, 2, as.numeric)

# Estimate those observations with an actual prediction
getIndex = function(x) match(1, x)

numType <- rowSums(est)
est[numType == 1, 8] <- apply(est[numType==1, 1:7], 1, getIndex)

numType_xval <- rowSums(est_xval)
est_xval[numType_xval==1, 8] <- apply(est_xval[numType_xval==1, 1:7], 1, getIndex)

numType_pred <- rowSums(pred)
pred[numType_pred==1, 8] <- apply(pred[numType_pred==1, 1:7], 1, getIndex)

# For undetermined observations, use classifier on all 7 types
fit <- randomForest( train$Cover_Type ~ ., data = train[, c(2:11, 16:57)], 
                     importance = TRUE, ntree = 100, maxnodes=200)
varImpPlot(fit)
est[numType != 1, 8] <- predict(fit, train[numType != 1,])
accuracy[8] <- sum(est[numType != 1, 8] == train$Cover_Type[numType != 1]) / sum(numType != 1)

est_xval[numType_xval != 1, 8] <- predict(fit, xval[numType_xval != 1,])
accuracy_xval[8] <- sum(est_xval[numType_xval != 1, 8] == xval$Cover_Type[numType_xval != 1]) / sum(numType_xval != 1)

total_accuracy <- sum(est[,8] == train$Cover_Type) / dim(train)[1]
total_accuracy_xval <- sum(est_xval[,8] == xval$Cover_Type) / dim(xval)[1]

pred[numType_pred != 1, 8] <- predict(fit, test[numType_pred != 1,])

# Compare to predicting directly using all 7 types
est7 <- predict(fit, train)
est7_xval <- predict(fit, xval)

est7_accuracy <- sum(est7 == train$Cover_Type) / dim(train)[1]
est7_xval_accuracy <- sum(est7_xval == xval$Cover_Type) / dim(xval)[1]

pred7 <- predict(fit, test)

