#installing required packages
install.packages("plyr")
install.packages(("rpart"))
install.packages("rpart.plot")
install.packages("tidyverse")
install.packages("MLmetrics")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
# import libraries
library(tidyverse)
library(plyr)
library(rpart)
library(rpart.plot)
library(MLmetrics)
library(caret)
library(ggplot2)
library(ggcorrplot)
library(pROC)
library(dplyr)
# getting the data from csv file 
data <- read.csv("BreastCancer.csv")

print(data)
str(data)

data = select(data,-1)
str(data)

# preprocess data
# removing the nan values
data <- na.omit(data)
str(data)
# using pearson method and finding correlation  
corr_mat = cor(data[1:9],method = 'pearson')
View(corr_mat)

corr_mat[!upper.tri(corr_mat)] <- 0
# plot the correlation
ggcorrplot(corr_mat)
# removing the more correlated attributes
data <- data[, !apply(corr_mat,2,function(x) any(abs(x) > 0.95 ,na.rm = TRUE))]

View(data)

data$Class<- as.factor(data$Class)
str(data)
#for without library copy dataset
data_2<- data
str(data_2)

sample_split <- floor(.67*nrow(data))
sample_split

set.seed(1)
k = seq_len(nrow(data))

training <- sample(k,size=sample_split)
training

cancer_train <- data[training,]
cancer_train

cancer_test <- data[-training,]
cancer_test
 
cancer_test_data <- select(cancer_test,-10)
cancer_test_data

cancer_test_out <- cancer_test["Class"]
cancer_test_out

#rpart
#building model using id3 algorithm
tree_model <- rpart(Class~.,data=cancer_train,method="class",parms=(list(split='information')))
tree_model
#analyzing results and plotting tree
printcp(tree_model)
plotcp(tree_model)
summary(tree_model)
# ploting the decision Tree
rpart.plot(tree_model)

##
print(tree_model,cp=-1)
pred <- predict(tree_model,type="class")
View(pred)
str(pred)

#checking accuracy
predict.cls <- tree_model %>%
  predict(cancer_test,type="class")

#prediction accuracy
mean(predict.cls==cancer_test$Class)
head(predict.cls)

#calculating f1 score
F1_Score(y_pred=predict.cls,y_true=cancer_test$Class,positive="1")
#confusion matrix
res<-confusionMatrix(cancer_test$Class,predict.cls,positive = "1")

#precision
precision <- res$byClass['Pos Pred Value']
print(precision)
#recall
recall <- res$byClass['Sensitivity']
print(recall)

#k-fold cross validation
TPR <- c()
FPR <- c()
accuracy<-c()

#creating 10 folds
folds <- createFolds(data$Class,k=10)

crossvalidation = lapply(folds,function(x){
   training_fold = data[-x,]
   test_fold = data[x,]
   tree_model_kfold <- rpart(Class~.,data=training_fold,method='class',parms = list(split='information'))
   test_fold_data <- select(test_fold,-10)
   test_fold_out <- test_fold["Class"]
   
   y_pred <- tree_model_kfold %>%
     predict(test_fold_data,type="class")
   confusionmatrix = table(test_fold[,10],y_pred)
   acc = (confusionmatrix[1,1]+confusionmatrix[2,2])/(confusionmatrix[1,2]+confusionmatrix[2,1]+confusionmatrix[1,1]+confusionmatrix[2,2])
   accuracy <-c(accuracy,acc)
   y = confusionmatrix[1,1]/(confusionmatrix[1,1]+confusionmatrix[1,2])
   TPR <- c(TPR,y)
   x = confusionmatrix[2,1]/(confusionmatrix[2,2]+confusionmatrix[2,1])
   FPR <- c(FPR,x)
   result <- cbind(accuracy,TPR,FPR)
   return (result)
   })
b = crossvalidation
b
#average accuracy
accuracies <- c(b$Fold01[1],b$Fold02[1],b$Fold03[1],b$Fold04[1],b$Fold05[1],
                b$Fold06[1],b$Fold07[1],b$Fold08[1],b$Fold09[1],b$Fold10[1])

print(accuracies)
avg_accuracies <- mean(accuracies)
print(avg_accuracies)

#generating roc curve
TPR <- c(b$Fold01[2],b$Fold02[2],b$Fold03[2],b$Fold04[2],b$Fold5[2],
         b$Fold06[2],b$Fold07[2],b$Fold08[2],b$Fold09[2],b$Fold10[2])
FPR <- c(b$Fold01[3],b$Fold02[3],b$Fold03[3],b$Fold04[3],b$Fold5[3],
  b$Fold06[3],b$Fold07[3],b$Fold08[3],b$Fold09[3],b$Fold10[3])

print(c(x=FPR,y=TPR))

# on  x-axis FPR
# on y-axis TPR
plot(FPR,TPR,main="ROC curve",type ="l",ylab = "TPR",xlab = "FPR",xlim = c(0,1),ylim = c(0,1))
# roc curve zoom in
plot(FPR,TPR,main="ROC curve",type="l",ylab = "TPR",xlab = "FPR")

df <- cbind.data.frame(FPR,TPR)
df
library(lubridate)

q<- ggplot(df,aes(FPR,TPR))+geom_point(aes(col=1:9))
q
q+xlim(0,1)+ylim(0,1)

