
##############################################################################
#Fit neural network model
##############################################################################

library(nnet)
y_train <- class.ind(unlist(y_train))
print(x_train[1:5,1:5])
print(y_train[1:5,])

set.seed(12345)
nn_1 <- nnet(x_train_reduced,y_train,size=150,softmax=TRUE,maxit=180,MaxNWts = 11000)

prediction <- as.data.frame(predict(nn_1,test_reduced,type="class"))
final <- cbind(prediction,test[,1])
colnames(final) <- c("prediction","label")
plot(final)
final$flag <- ifelse(final$label==final$prediction,1,0)
paste("Out-sample test accuracy:",sum(final$flag)/nrow(final))

prediction <- as.data.frame(predict(nn_1,x_train_reduced,type="class"))
final <- cbind(prediction,train[,1])
colnames(final) <- c("prediction","label")
plot(final)
final$flag <- ifelse(final$label==final$prediction,1,0)
sum(final$flag)/nrow(final)

#confusion matrix
cm = as.matrix(table(Actual = final$label, predicted = final$pred))
cm

#f1 score, precision, recall
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)
diag = diag(cm)
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)

#install.packages("devtools")
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn_1)
