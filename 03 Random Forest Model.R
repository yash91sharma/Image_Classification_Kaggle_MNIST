##############################################################################
#Fit random forest model - without PCA
##############################################################################
library(randomForest)
#install.packages("ROCR")
library(ROCR)

#Multiple Iterations of the model

#final model fitting
rf <-randomForest(label ~ .,data=train, mtry=28, importance=TRUE,ntree=50,nodesize=10)
print(rf)
#importance(rf)
varImpPlot(rf)

#test the model on testing data
test1 <- test[,-1]
pred <- predict(rf, test1)
final_rf <- as.data.frame(cbind(test$label,pred))
colnames(final_rf)[1] <- "actual"
final_rf$flag <- ifelse(final_rf$actual == final_rf$pred,1,0)
paste("Out-sample test accuracy: ",sum(final_rf$flag)/nrow(final_rf))

#confusion matrix
cm = as.matrix(table(Actual = final_rf$actual, predicted = final_rf$pred))
cm

#f1 score, precision, recall
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)
diag = diag(cm)
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)

#Model misclassifies digits 0, 3, 5 and 8
#Neural Network Model overcomes this problem