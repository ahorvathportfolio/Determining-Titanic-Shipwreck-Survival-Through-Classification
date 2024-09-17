# Master's Report Classification Project - Andras Horvath
# Naive Bayes

setwd("Your directory to titanic data set folder")
data = read.csv("Cleaned Dataset.csv", header = T)
data = data[-1] # remove this pesky column that keeps track of the observations

library(e1071)
library(caTools)
library(caret)


# Fitting Naive Bayes Model 
# By default, this implementation of the naive Bayes classifier models each 
# quantitative feature using a Gaussian distribution. However, a kernel density 
# method can also be used to estimate the distributions.
# The output contains the estimated mean and standard deviation for each
# variable in each class.

#set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Survived ~ ., data = data)
classifier_cl

sum(data$Survived)/length(data$Survived) # = 0.4044944
# This is the prior probability for Y = 1 i.e. having survived.

# Predicting on test data (Since we do not know the "truth" about an individual's
# survival in the test set, I will actually use the training data set "Cleaned Dataset.csv")
y_pred <- predict(classifier_cl, newdata = data)

# Confusion Matrix
cm <- table(data = y_pred, reference = data$Survived) #https://www.rdocumentation.org/packages/caret/versions/3.45/topics/confusionMatrix
# prediction first, then true results.
cm
# table(y_pred,data$Survived) # gives the same output as cm

# Double check:
tester = 0
for(i in 1:length(data$Survived))
{
  if (data$Survived[i] == 1 & y_pred[i] ==0)
  {
    tester = tester + 1
  }
}
tester # 83. Perfect!

tester = 0
for(i in 1:length(data$Survived))
{
  if (data$Survived[i] == 1 & y_pred[i] ==1)
  {
    tester = tester + 1
  }
}
tester # 205 survived and classified as survived

tester = 0
for(i in 1:length(data$Survived))
{
  if (data$Survived[i] == 0 & y_pred[i] ==0)
  {
    tester = tester + 1
  }
}
tester # 355

tester = 0
for(i in 1:length(data$Survived))
{
  if (data$Survived[i] == 0 & y_pred[i] ==1)
  {
    tester = tester + 1
  }
}
tester # 69

# Model Evaluation
example <- confusionMatrix(cm)
example

# y_pred
#    0   1
#0 355  83
#1  69 205
# Classified just a little bit worse than logistic regression for p = 0.5 Threshold

example$overall[1]
# Accuracy : 0.7865
# 95% CI : (0.7546, 0.8161)

# Confusion Matrix Visual:
cm <- example
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1)))
  par(mar=c(2,2,2,2))
  plot(c(120, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='steelblue')
  text(195, 435, 'Did Not Survive', cex=1.2)
  rect(250, 430, 340, 370, col='gray')
  text(295, 435, 'Survived', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='gray')
  rect(250, 305, 340, 365, col='steelblue')
  text(140, 400, 'Did Not Survive', cex=1.2, srt=90)
  text(140, 335, 'Survived', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
}  
draw_confusion_matrix(cm)
example$table


# ROC Curve:
# Best video I found was the following:
# https://www.youtube.com/watch?v=qcvAqAH60Yw
library(pROC)
par(pty = "s") # set plot type to square if looks weird.
roc(data$Survived,as.numeric(y_pred), plot = TRUE, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve", col = "steelblue", print.auc = TRUE)
# Area under curve = 0.775

par(pty = "m") # Set back to maximum i.e. fill up the whole space provided.


# To use kernel density estimator you will need to download naivebayes package
# and specify usekernel = TRUE in naive_bayes function.
