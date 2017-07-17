##############################################################################
#Clear existing data sets and define working directory
##############################################################################
rm(list=ls())
getwd()
setwd("D:/UC work/Character Recognition")

##############################################################################
#Read the labelled dataset of 42000 observations
##############################################################################
library(readr)
df <- read_csv("train.csv")
df$label <- as.factor(df$label)
#str(df)

##############################################################################
#Define insample and outsample (5%)
##############################################################################
set.seed(12345)
random <- sample(1:nrow(df),nrow(df)*0.95)
test <- df[-random,]
train <- df[random,]

##############################################################################
#Data Exploration
##############################################################################

#check dimensions
dim(df)

#count complete cases
sum(complete.cases(df))

#check how may columns have no variation at all
a <- as.data.frame(apply(df!=0,2,sum))
a <- as.data.frame(a[-1,])
colnames(a)[1] <- "count"
max(a$count)
hist(a$count,main="Lot of columns have no variation in the data (all 0s)",
     xlab="Sum of all values of a column",ylab="Number of columns",
     xlim=c(0,30000),breaks=200)

#see the variation in dependent variable
library(ggplot2)
ggplot(df,aes(x=label))+
  geom_bar(stat="count")+
  ggtitle("Variation in Dependent Variable")+
  ylab("Frequency")+xlab("Digit (Dependent Variable)")
