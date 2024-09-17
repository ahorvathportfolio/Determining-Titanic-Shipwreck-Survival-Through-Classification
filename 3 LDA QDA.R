# Master's Report Classification Project - Andras Horvath
# LDA and QDA


################
# Titanic Data #
################
setwd("Your directory to titanic data set folder")
data = read.csv("Cleaned Dataset.csv", header = T)
data = data[-1] # remove this pesky column that keeps track of the observations
library(MASS)

# LDA Classifier
sum(data$Survived)/length(data$Survived) # = 0.4044944
# This is the prior probability for Y = 1 i.e. having survived.
lda.fit = lda(Survived ~ ., data = data)
lda.fit

# Predict on "test" data (it's really just our training data since that is all
# that we have)
lda.pred = predict(lda.fit,data)
lda.class = lda.pred$class # These are the predictions

# Confusion Matrix
cm = table(lda.class,data$Survived) # (364+204)/(364+204+84+60) =  0.7977528 accuracy
mean(lda.class == data$Survived) # 0.7977528 would give the same
cm

#lda.class   0   1
#        0 364  84
#        1  60 204

# Model Evaluation
library(caret)
example <- confusionMatrix(cm)
example

example$overall[1]
# Accuracy : 0.7978          is higher than Naive Bayes!
# 95% CI : (0.7663, 0.8267)

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
roc(data$Survived,as.numeric(lda.class), plot = TRUE, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve", col = "steelblue", print.auc = TRUE)
# Area under curve = 0.7834

par(pty = "m") # Set back to maximum i.e. fill up the whole space provided.

##################################################################################
# QDA Classifier
qda.fit <- qda(Survived ~ ., data = data)
qda.fit

# Prediction
qda.class <- predict(qda.fit, data)$class #  These are the predictions

# Confusion Matrix
cm = table(qda.class,data$Survived)
table(qda.class,data$Survived)
# qda.class   0   1
#         0 370  88
#         1  54 200
(370+200)/712 # 0.8005618 accuracy
mean(qda.class == data$Survived) # 0.8005618
cm

# Model Evaluation
library(caret)
example <- confusionMatrix(cm)
example

example$overall[1]
# Accuracy : 0.8006                    
# 95% CI : (0.7693, 0.8293)

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
roc(data$Survived,as.numeric(qda.class), plot = TRUE, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve", col = "steelblue", print.auc = TRUE)
# Area under curve = 0.7835

par(pty = "m") # Set back to maximum i.e. fill up the whole space provided.
