# Master's Report Classification Project - Andras Horvath
# Classification Trees:

################
# Titanic Data #
################
setwd("Your directory to titanic data set folder")
data = read.csv("Cleaned Dataset.csv", header = T)
data = data[-1] # remove this pesky column that keeps track of the observations
library(tree)
library(caret)

# I need to transform my survived variable into a factor
tree.titanic = tree(factor(Survived) ~ ., data = data)
summary(tree.titanic)
#Classification tree:
#  tree(formula = as.factor(Survived) ~ ., data = data)
#Variables actually used in tree construction:
#  [1] "sex"    "Pclass" "Age"    "SibSp"  "Fare"  
#Number of terminal nodes:  7 
#Residual mean deviance:  0.7995 = 563.6 / 705 
#Misclassification error rate: 0.1742 = 124 / 712

plot(tree.titanic)
text(tree.titanic, pretty = 0)

# However, I want to change the zeros and ones.

survived = c()
for(i in 1:length(data$Survived))
{
  if(data$Survived[i] == 1)
  {
    survived[i] = "Survived"
  } else
  {
    survived[i] = "Did not survive"
  }
}
survived

data_adjusted = cbind(data,survived)
counter = 0
for (i in 1:length(data$Survived))
{
  if (data_adjusted$Survived[i] == 1 & data_adjusted$survived[i] == "Survived")
  {
    counter = counter + 1
  }
  if (data_adjusted$Survived[i] == 0 & data_adjusted$survived[i] == "Did not survive")
  {
    counter = counter + 1
  }
}
counter # 712
length(data_adjusted$Survived) # 712
# Looks good to go!

data_adjusted = data_adjusted[,2:9]
colnames(data_adjusted) = c("Class","Age","SibSp","Parch","Fare","Sex","Embark", "survived")
# This is just to make sure all variables start with a capital letter and thus
# look better in any upcoming visuals.

tree.titanic2 = tree(factor(survived) ~ ., data = data_adjusted)
summary(tree.titanic2)
#Classification tree:
#  tree(formula = factor(survived) ~ ., data = data_adjusted)
#Variables actually used in tree construction:
# [1] "Sex"   "Class" "Age"   "SibSp" "Fare" 
#Number of terminal nodes:  7 
#Residual mean deviance:  0.7995 = 563.6 / 705 
#Misclassification error rate: 0.1742 = 124 / 712 

# It's the same as before! Perfect!
plot(tree.titanic2)
text(tree.titanic2, pretty = 0) # You will find this plot on page 35 of report

tree.pred = predict(tree.titanic2, data_adjusted,type = "class")
cm = table(tree.pred,data_adjusted$survived) # prediction then truth
cm

#tree.pred         Did not survive Survived
#  Did not survive             380       80
#  Survived                     44      208

example <- confusionMatrix(cm)
example

example$overall[1]
# 0.8258427 Accuracy

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


# Finding Optimal Tree:
# Does pruning the tree lead to improved results? Let's find out.

set.seed(7)
cv.titanic <- cv.tree(tree.titanic2, FUN = prune.misclass)
names(cv.titanic)
#"size" "dev" "k" "method"
cv.titanic

# Tree with 7 terminal nodes results in only 136 cross validation errors.
par(mfrow = c(1, 2))
plot(cv.titanic$size, cv.titanic$dev, type = "b")
plot(cv.titanic$k, cv.titanic$dev, type = "b")

# Plot for paper:
dev.off()
par(mfrow = c(1,1))
plot(cv.titanic$size, cv.titanic$dev, type = "b", col = "steelblue", xlab = "Number of Terminal Nodes", ylab = "Number of Cross Validation Errors", main = "Determining Optimal Tree")

# No pruning necessary

# ROC Curve
library(pROC)
par(pty = "s") # set plot type to square if looks weird.
roc(data$Survived,as.numeric(tree.pred), plot = TRUE, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve", col = "steelblue", print.auc = TRUE)
# Area under curve = 0.809
roc(data_adjusted$survived,as.numeric(tree.pred), plot = TRUE, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve", col = "steelblue", print.auc = TRUE)
# This gives the same thing

par(pty = "m") # Set back to maximum i.e. fill up the whole space provided.


#######################################################
# Additional Practice + Plot for Tree Intro in Report #
#######################################################

#install.packages("tree")
library(tree)
library(ISLR2)
attach(Carseats)

High = factor(ifelse(Sales <= 8, "No","Yes")) # Transform to binary variable
Carseats = data.frame(Carseats,High)

# Use all variables but sales to predict High
tree.carseats = tree(High ~ . - Sales, Carseats)
summary(tree.carseats)

# Use plot() to show the tree and text() to display node labels. pretty = 0 tells R
# to include category names rather than displaying a letter for each category.
plot(tree.carseats)
text(tree.carseats, pretty = 0)

tree.carseats

set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train, ] # 200  x 12
High.test <- High[-train] # 200 x 1
tree.carseats <- tree(High ~ . - Sales, Carseats,
                      subset = train)
tree.pred <- predict(tree.carseats, Carseats.test,
                     type = "class")
table(tree.pred, High.test)
(104+50) / 200

# Use cross validation and pruning to determine optimal tree size.
set.seed(7)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
#"size" "dev" "k" "method"
cv.carseats

# Tree with 9 terminal nodes results in only 74 cross validation errors.
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# Now prune to get the 9 node tree
par(mfrow = c(1,1))
prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0) # This plot can be found in my report on page 11

# If we have test data we can see how well this pruned tree performs on the 
# prediction.

tree.pred <- predict(prune.carseats, Carseats.test,
                     type = "class")
table(tree.pred, High.test)
(97 + 58) / 200
# Pruning has led to a more interpretable tree AND more test observations classified
# correctly.

# Increasing size of tree does not help:
prune.carseats <- prune.misclass(tree.carseats, best = 14)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test,
                     type = "class")
table(tree.pred, High.test)
(102 + 52) / 200

# Separate thing I want to check:
tree.carseats = tree(High ~ . - Sales, Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.pred = predict(tree.carseats, Carseats,type = "class")
table(tree.pred,Carseats$High)
# tree.pred  No Yes
#       No  213  13
#       Yes  23 151
(213 + 151) / (213+13+23+151) # 0 .91 accuracy as expected!