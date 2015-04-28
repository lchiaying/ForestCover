Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
library(extraTrees)

#
set.seed(42)

fit <- extraTrees(train[, -c(11)], train$Cover_Type, numRandomCuts=10, mtry = 8)

est <- predict(fit, train[, -c(11)])
accuracy <- sum(est == train$Cover_Type) / dim(train)[1]

est_xval <- predict(fit, xval[, -c(11)])
accuracy_xval <- sum(est_xval == xval$Cover_Type) / dim(xval)[1]

pred <- predict(fit, test)
