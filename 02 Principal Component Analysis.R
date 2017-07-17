

##############################################################################
#Data reduction - Principal Component Analysis
##############################################################################
x_train <- train[,-1]
y_train <- train[,1]
x_train <- x_train/255 #standardize the values (vary between 0 and 1)
x_cov <- cov(x_train) #computer covariance between columns of x_train
x_pca <- prcomp(x_cov) #calculate principal components
plot(x_pca) #plot variance of each principal components
str(x_pca)
summary(x_pca$sdev)
pca_variance <- cbind(c(1:784),as.data.frame(x_pca$sdev/sum(x_pca$sdev)))
colnames(pca_variance) <- c("pc","pc_var")
library(ggplot2)
ggplot(pca_variance, aes(x=pc, y=cumsum(pc_var)))+ 
  geom_line()+
  ggtitle("Total Variance Explained vs Number of Principal Components")+
  xlab("Number of Principal Components used")+
  ylab("Percentage Variance explained")+
  scale_y_continuous(labels = scales::percent)+
  geom_vline(xintercept = 50,color="orange")+
  annotate("text", x = 340, y = .80, label = "50 PCs explain 82.3% of total variance",color="orange")

sum(pca_variance$pc_var[1:50])
x_train_reduced <- as.matrix(x_train) %*% x_pca$rotation[,1:50]

test_reduced <- as.matrix(test[,-1]/255) %*%  x_pca$rotation[,1:50]

train_reduced <- cbind(test$label,as.data.frame(x_train_reduced))
colnames(train_reduced)[1] <- "label"

