# Master's Report Classification Project - Andras Horvath
# Logistic Regression

# In this file the key takeaways and what actually made it into my report was:
# -- Showing the advantages of Logistic Regression over Linear Regression
#    (using another data set to emphasize this)
# -- Refining the Logit Model
# -- Creating Confusion Matrices to visualize the results
# You will find other bits of code including over fitting (by trying to find the absolute
# best probability cutoff) and correlation heat map code as I figured while not
# applicable to this project I found them an interesting brief exercise which may
# be useful to others for other projects where these ideas may be applicable/valid.

setwd("Your directory to titanic data set folder")
data = read.csv("Cleaned Dataset.csv", header = T)
data = data[-1] # remove this pesky column that keeps track of the observations

sum(data$Survived == 0) # 424 Did not survive
sum(data$Survived == 1) # 288 Survived
sum(data$Survived == 0 & data$sex == 0) # 360 males died
sum(data$Survived == 1 & data$sex == 0) # 93 males survived
sum(data$Survived == 0 & data$sex == 1) # 64 females died
sum(data$Survived == 1 & data$sex == 1) # 195 females survived
# 195/259 females survived = 0.75 survival rate
# 93/453 males survived = 0.21 survival rate
# Much smaller survival rate for men, but there were nearly twice as many men compared
# to females in the data set.

# If the data set did not have categorical variables the following Correlation
# Matrix Heatmaps would have been used over the Box Plots:
# Correlation Matrix Heatmap
install.packages("corrplot")
library(corrplot)
cor(data)
corrplot.mixed(cor(data), order = 'AOE')

corrplot(cor(data), tl.col = "black", method = "circle", bg = "White", tl.srt = 50, 
         title = "\n\n Correlation Plot Of Titanic Data",
         addCoef.col = "black", type = "lower")
# All very cool visuals I have used in other projects, but again, they cannot be 
# used since correlation not applicable for categorical variables.

##################
### Regression ###
##################
# I want to start with Linear Regression (and why it isn't a good idea). As you will
# see when we get to logistic regression for my data the clear advantages over linear
# regression are not very apparent visually. So I will use a different data set to
# drive this point home in my report.
lmod = lm(data$Survived ~ data$Age)
summary(lmod)
# Survived = 0.487313 - 0.002794*Age
plot(x = data$Age, y = data$Survived)
lines(data$Age,lmod$fitted.values)

# OR:
plot(data$Age,data$Survived, xlab = "Age", ylab = "Probability of Survival", main = "Estimated Probabilities of Survival Using Linear Regression", col = "steelblue", pch = 16, cex.lab=1.1, cex.axis=1.05, cex.main=1.25)
abline(lmod, col = "black")
logmod = glm(Survived ~ Age, family = binomial(link="logit"),data = data)
summary(logmod)
# Survived = -0.041169 - 0.011757*Age
plot(data$Age,data$Survived, xlab = "Age", ylab = "Probability of Survival", main = "Estimated Probabilities of Survival Using Linear Regression", col = "red", pch = 16, cex.lab=1.1, cex.axis=1.05, cex.main=1.25)


#################################################
#fit logistic regression model
model = glm(Survived ~ Age, family = binomial(link="logit"),data = data)

#define new data frame that contains predictor variable
newdata <- data.frame(Age=seq(min(data$Age), max(data$Age),len=500))

#use fitted model to predict values of Survived
newdata$Survived = predict(model, newdata, type="response")

#plot logistic regression curve
plot(Survived ~ Age, data=data, col="steelblue")
lines(Survived ~ Age, newdata, lwd=2)
####################################################
# Another way to plot this would be something such as the following:
library(ggplot2)


#plot logistic regression curve
ggplot(data, aes(x=Age, y=Survived)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))
##########################################################
# Our data is no good to show the downside of linear regression vs logistic regression.
# This is because I cannot generate a decent logistic curve.
# Use ISLR2 package Default data (Customer default records for a credit card company).
# I am going to reproduce the plots on page 133 of An Introduction  to 
# Statistical Learning by James et al 2021.

###############################
### Why Linear Reg Bad Idea ###
###############################

install.packages("ISLR2")
library(ISLR2)
df = Default
length(df$default) # 10000
lmod = lm(df$default~ df$balance)
Default_num = as.numeric((df$default))
Default_num
length(Default_num) # 10000
for (i in 1:length(Default_num))
{
  if (Default_num[i] == 1)
  {
    Default_num[i] = 0
  }
}
for (i in 1:length(Default_num))
{
  if (Default_num[i] == 2)
  {
    Default_num[i] = 1
  }
}

Default_num
Default_num = as.matrix(Default_num, nrows = 10000)
colnames(Default_num) = "Default_num"
df = cbind(df,Default_num)

lmod = lm(df$Default_num ~ df$balance)
par(mfrow=c(1,2))
plot(df$balance,df$Default_num, xlab = "Balance", ylab = "Probability of Default", main = "Estimated Probabilities of Default Using Linear Regression", col = "orange", pch = 16, cex.lab=1.1, cex.axis=1.05, cex.main=1.2, ylim = c(-0.1,1.1))
abline(h = 0, lty = 2)
abline(h = 1, lty = 2)
abline(lmod, col = "blue")

model = glm(Default_num ~ balance, family = binomial(link="logit"),data = df)

# Define new data frame that contains predictor variable
newdata <- data.frame(balance=seq(min(df$balance), max(df$balance),len=500))

# Use fitted model to predict values of Default
newdata$Default_num = predict(model, newdata, type="response")


# Plot logistic regression curve
plot(Default_num ~ balance, data=df, xlab = "Balance", ylab = "Probability of Default", main = "Estimated Probabilities of Default Using Logistic Regression", col = "orange", pch = 16, cex.lab=1.1, cex.axis=1.05, cex.main=1.2, ylim = c(-0.1,1.1))
abline(h = 0, lty = 2)
abline(h = 1, lty = 2)
lines(Default_num ~ balance, newdata, lwd=2, col = "blue")
par(mfrow = c(1,1)) # Set back to default of one plot filling up whole space
# This plot can be found on page 4 of my Masters Report.

##################################
### Logistic Regression Model: ###
##################################

logis_glm <- glm(Survived ~ ., family = binomial(link="logit"),data = data)
summary(logis_glm)

# Model:
# B'z = 2.730342 - 1.239166*Pclass - 0.043962*Age - 0.369697*SibSp - 0.058027*Parch + 0.001927*Fare + 2.616908*sex + 0.097510*embark
# Note that only the intercept, Pclass, Age, SibSp, and sex were significant predictors.
# I will address this later using backward elimination.
#################################################################################################################
# ROC Curve:
# Best video I found was the following:
# https://www.youtube.com/watch?v=qcvAqAH60Yw
library(pROC)
par(pty = "s") # set plot type to square if looks weird.
roc(data$Survived,logis_glm$fitted.values, plot = TRUE, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "Logistic Regression Full Model ROC Curve", col = "steelblue", print.auc = TRUE)
# Area under curve = 0.8585

par(pty = "m") # Set back to maximum i.e. fill up the whole space provided.

#################################################################################################################
# ROC Curve (Optional):
# https://www.digitalocean.com/community/tutorials/plot-roc-curve-r-programming
#error metrics -- Confusion Matrix
#err_metric=function(CM)
#{
#  TN =CM[1,1]
#  TP =CM[2,2]
#  FP =CM[1,2]
#  FN =CM[2,1]
#  precision =(TP)/(TP+FP)
#  recall_score =(FP)/(FP+TN)
#  f1_score=2*((precision*recall_score)/(precision+recall_score))
#  accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
#  False_positive_rate =(FP)/(FP+TN)
#  False_negative_rate =(FN)/(FN+TP)
#  print(paste("Precision value of the model: ",round(precision,2)))
#  print(paste("Accuracy of the model: ",round(accuracy_model,2)))
#  print(paste("Recall value of the model: ",round(recall_score,2)))
#  print(paste("False Positive rate of the model: ",round(False_positive_rate,2)))
#  print(paste("False Negative rate of the model: ",round(False_negative_rate,2)))
#  print(paste("f1 score of the model: ",round(f1_score,2)))
#}
# 59+365 = 424
# 59 / 424 = 0.139. The false positive rate is correct.

#logit_P = predict(logis_glm , newdata = data, type = 'response' ) # I need to determine if a testing set should be used.
#logit_P <- ifelse(logit_P > 0.5,1,0) # Probability check
#CM= table(data[,1] , logit_P)
#print(CM)
#err_metric(CM)

#ROC-curve using pROC library
#install.packages("pROC")
#library(pROC)
#roc_score=roc(data[,1], logit_P) #AUC score
#plot(roc_score ,main ="ROC curve -- Logistic Regression ", col = "steelblue")

#################################################################################################################
Linear_Classifier = c()
for (i in 1:length(data$Survived))
{
  Linear_Classifier[i] = 2.730342 - 1.239166*data$Pclass[i] - 0.043962*data$Age[i] - 0.369697*data$SibSp[i] - 0.058027*data$Parch[i] + 0.001927*data$Fare[i] + 2.616908*data$sex[i] + 0.097510*data$embark[i]
}
Linear_Classifier = as.data.frame(Linear_Classifier)
data_new = data.frame(c(data,Linear_Classifier))

#2.730342 - 1.239166*data$Pclass[1] - 0.043962*data$Age[1] - 0.369697*data$SibSp[1] - 0.058027*data$Parch[1] + 0.001927*data$Fare[1] + 2.616908*data$sex[1] + 0.097510*data$embark[1]
# -2.310046
#exp(-2.310046)/(1 + exp(-2.310046))
# 0.09029437
# Very small probability of surviving. Makes sense. First observation was a male.


# Before I forget, let me also get all of the probabilities:
# p(x) = exp(Bo + B1X + ...)/(1 + exp(Bo + B1X + ...))
p = c()
for (i in 1:length(data_new$Linear_Classifier))
{
  p[i] = exp(data_new$Linear_Classifier[i])/(1 + exp(data_new$Linear_Classifier[i]))
}
p = as.data.frame(p)
data_new = data.frame(c(data_new,p))
View(data_new)

####################
# confusion matrix #
####################
library(caret)

#Creates vectors having data points
Predicted = c()
for (i in 1:length(data_new$p))
{
  if (data_new$p[i] <= 0.5)
  {
    Predicted[i] = 0 # Did not survive
  }else
  {
    Predicted[i] = 1 # Survived
  }
}
Predicted
sum(Predicted==1) # 266 survivals. 288 actually survived in our dataset.
sum(Predicted==0) # 446 survivals. 424 actually survived in our dataset.

Actual = c()
for (i in 1:length(data_new$Survived))
{
  if(data_new$Survived[i] == 1)
  {
    Actual[i] = 1
  } else
  {
    Actual[i] = 0
  }
}
Actual
sum(Actual == data_new$Survived) # 712. So correct!

expected_value <- factor(Actual)
predicted_value <- factor(Predicted)

#Creating confusion matrix
example <- confusionMatrix(data=predicted_value, reference = expected_value)

#Display results 
example


#               Reference
# Prediction       0   1
#              0 365  81
#              1  59 207

tester = 0
for(i in 1:length(data$Survived))
{
  if (data$Survived[i] == 1 & Predicted[i] ==0)
  {
    tester = tester + 1
  }
}
tester #81 as expected.

example$overall[1]
# Accuracy : 0.8034         
# 95% CI : (0.7722, 0.832)

# Confusion Matrix Visual:
cm <- example
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1)))
  par(mar=c(2,2,2,2))
  plot(c(120, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('Full Model p(X) = 0.5 Threshold', cex.main=1.5)
  
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


# Let's see if we can improve this (Try to improve accuracy to see how playing with
# the cut off affects accuracy in the training data set).
# On actual Titanic roughly 1500/2200 died. Use 0.68 as new probability cut off.

Predicted = c()
for (i in 1:length(data_new$p))
{
  if (data_new$p[i] <= 0.68)
  {
    Predicted[i] = 0 # Did not survive
  }else
  {
    Predicted[i] = 1 # Survived
  }
}
Predicted
sum(Predicted==1) # 183 survivals. 288 actually survived in our dataset.
sum(Predicted==0) # 529 non survivals. 424 actually died in our dataset.

expected_value <- factor(Actual)
predicted_value <- factor(Predicted)

#Creating confusion matrix
example2 <- confusionMatrix(data=predicted_value, reference = expected_value)

#Display results 
example2

# Confusion Matrix and Statistics

#           Reference
#Prediction   0   1
#          0 405 124
#          1  19 164

#(164 + 405)/712 # Accuracy
#example2$overall[1]

# Accuracy : 0.7992         
# 95% CI : (0.7678, 0.828)


### Let's have a more structured approach
Logistic_Probabilities = as.data.frame(cbind(data_new$Survived, data_new$p))
colnames(Logistic_Probabilities) = c("Survived", "Probabilities")
# Now we can compare these and investigate.

##############################
### Finding optimal cutoff ###
##############################
# As a note, this is a very bad idea. I did this out of curiosity but it can be
# viewed as a form of over fitting to your training data. This will not work well
# on testing data sets.

Actual = c()
for (i in 1:length(data_new$Survived))
{
  if(data_new$Survived[i] == 1)
  {
    Actual[i] = 1
  } else
  {
    Actual[i] = 0
  }
}
Actual

accuracy = c()
for (j in seq(0.02, 0.97, by = 0.01)) # Only works between these probabilities for some reason.
{
  Predicted = c()
  for (i in 1:length(data_new$p))
  {
    if (data_new$p[i] <= j)
    {
      Predicted[i] = 0 # Did not survive
    }else
    {
      Predicted[i] = 1 # Survived
    }
  }
  
  expected_value <- factor(Actual)
  predicted_value <- factor(Predicted)
  example <- confusionMatrix(data=predicted_value, reference = expected_value)
  accuracy = append(accuracy,example$overall[1])
  accuracy
}
accuracy = as.vector(accuracy)
max(accuracy) # 0.8146067 which is observation 54 which would be p = 0.55 for cutoff
accuracy[50:60]
plot(accuracy, xlab = "Probability (%)", ylab = "Accuracy", main = "Optimal Probability for Logistic Regression", pch = 16, col = "blue",cex.lab=1.25, cex.axis=1.25, cex.main=1.5) # Here probability is Probability - 1%

# Let's just double check:

Predicted = c()
for (i in 1:length(data_new$p))
{
  if (data_new$p[i] <= 0.55)
  {
    Predicted[i] = 0 # Did not survive
  }else
  {
    Predicted[i] = 1 # Survived
  }
}
Predicted

expected_value <- factor(Actual)
predicted_value <- factor(Predicted)

#Creating confusion matrix
example <- confusionMatrix(data=predicted_value, reference = expected_value)

#Display results 
example
#0.8146
names(example)
example$table

# The following code comes from the following website (I have made a few modifications):
# https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package
#cm <- example
#draw_confusion_matrix <- function(cm) {
#  
#  layout(matrix(c(1,1)))
#  par(mar=c(2,2,2,2))
#  plot(c(120, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
#  title('CONFUSION MATRIX', cex.main=2)
#  
#  # create the matrix 
#  rect(150, 430, 240, 370, col='steelblue')
#  text(195, 435, 'Did Not Survive', cex=1.2)
#  rect(250, 430, 340, 370, col='gray')
#  text(295, 435, 'Survived', cex=1.2)
#  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
#  text(245, 450, 'Actual', cex=1.3, font=2)
#  rect(150, 305, 240, 365, col='gray')
#  rect(250, 305, 340, 365, col='steelblue')
#  text(140, 400, 'Did Not Survive', cex=1.2, srt=90)
#  text(140, 335, 'Survived', cex=1.2, srt=90)
#  
#  # add in the cm results 
#  res <- as.numeric(cm$table)
#  text(195, 400, res[1], cex=1.6, font=2, col='white')
#  text(195, 335, res[2], cex=1.6, font=2, col='white')
#  text(295, 400, res[3], cex=1.6, font=2, col='white')
#  text(295, 335, res[4], cex=1.6, font=2, col='white')

# add in the specifics 
#plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
#text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
#text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
#text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
#text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
#text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
#text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
#text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
#text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
#text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
#text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)

# add in the accuracy information 
#text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
#text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
#text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
#text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
#}  
#draw_confusion_matrix(cm)
#example$table

####
# Here is the original code:
# calculate the confusion matrix
#cm <- confusionMatrix(data = test_set$pred, reference = test_set$obs)
#draw_confusion_matrix <- function(cm) {

#  layout(matrix(c(1,1,2)))
#  par(mar=c(2,2,2,2))
#  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
#  title('CONFUSION MATRIX', cex.main=2)

# create the matrix 
#  rect(150, 430, 240, 370, col='#3F97D0')
#  text(195, 435, 'Class1', cex=1.2)
#  rect(250, 430, 340, 370, col='#F7AD50')
#  text(295, 435, 'Class2', cex=1.2)
#  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
#  text(245, 450, 'Actual', cex=1.3, font=2)
#  rect(150, 305, 240, 365, col='#F7AD50')
#  rect(250, 305, 340, 365, col='#3F97D0')
#  text(140, 400, 'Class1', cex=1.2, srt=90)
#  text(140, 335, 'Class2', cex=1.2, srt=90)
#  
# add in the cm results 
#  res <- as.numeric(cm$table)
#  text(195, 400, res[1], cex=1.6, font=2, col='white')
#  text(195, 335, res[2], cex=1.6, font=2, col='white')
#  text(295, 400, res[3], cex=1.6, font=2, col='white')
#  text(295, 335, res[4], cex=1.6, font=2, col='white')

# add in the specifics 
#  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
#  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
#  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
#  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
#  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
#  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
#  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
#  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
#  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
#  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
#  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)

# add in the accuracy information 
#  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
#  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
#  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
#  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
#}  
#draw_confusion_matrix(cm)
################################################################################


###########################
### Refined Logit Model ###
###########################
# That was for the full model. Best accuracy is 0.8146 for p = 0.55 cutoff. But again,
# this procedure is over fitting to the training data which is why it was not seriously
# considered for my report. The best procedure is to stick to a few predetermined 
# probability cutoffs which made sense from the beginning. Now, we saw from the full
# model that there may be good reason to refine our original logit model. I will do
# that here and finally test some of the logical probability cutoffs.

# Backward Elimination:
logis_glm <- glm(Survived ~ .-Parch, family = binomial(link="logit"),data = data)
summary(logis_glm)

logis_glm <- glm(Survived ~ .-Parch-embark, family = binomial(link="logit"),data = data)
summary(logis_glm)

logis_glm <- glm(Survived ~ .-Parch-embark-Fare, family = binomial(link="logit"),data = data)
summary(logis_glm)

logis_glm2 <- glm(Survived ~ Pclass+Age+SibSp+sex, family = binomial(link="logit"),data = data)
summary(logis_glm2) # Has smaller AIC. Preferred.
# First remove embark
# Then Parch
# Then Fare
# Now all significant and we have a smaller AIC.

# Model:
# B'z = 2.97607 - 1.31392*Pclass - 0.04459*Age - 0.37465*SibSp + 2.61477*sex
# This is the model which we will test at different predetermined probability cutoffs
# to investigate.
Linear_Classifier = c()
for (i in 1:length(data$Survived))
{
  Linear_Classifier[i] = 2.97607 - 1.31392*data$Pclass[i] - 0.04459*data$Age[i] - 0.37465*data$SibSp[i] + 2.61477*data$sex[i]
}
Linear_Classifier = as.data.frame(Linear_Classifier)
data_new = data.frame(c(data,Linear_Classifier))


p = c()
for (i in 1:length(data_new$Linear_Classifier))
{
  p[i] = exp(data_new$Linear_Classifier[i])/(1 + exp(data_new$Linear_Classifier[i]))
}
p = as.data.frame(p)
data_new = data.frame(c(data_new,p))
View(data_new)
hist(data_new$p) # Somewhat skewed right


####################
# confusion matrix # For 0.5 cutoff (default)
####################
library(caret)

#Creates vectors having data points
Predicted = c()
for (i in 1:length(data_new$p))
{
  if (data_new$p[i] <= 0.5)
  {
    Predicted[i] = 0 # Did not survive
  }else
  {
    Predicted[i] = 1 # Survived
  }
}
Predicted
sum(Predicted==1) # 268 survivals. 288 actually survived in our dataset.
sum(Predicted==0) # 444 non survivals. 424 actually survived in our dataset.

Actual = c()
for (i in 1:length(data_new$Survived))
{
  if(data_new$Survived[i] == 1)
  {
    Actual[i] = 1
  } else
  {
    Actual[i] = 0
  }
}
Actual
sum(Actual == data_new$Survived) # So correct!


expected_value <- factor(Actual)
predicted_value <- factor(Predicted)

#Creating confusion matrix
example <- confusionMatrix(data=predicted_value, reference = expected_value)

#Display results 
example


#         Reference
#Prediction   0   1
#         0 366  78
#         1  58 210

example$overall[1]
# Accuracy : 0.809         
# 95% CI : (0.7782, 0.8372)

# Confusion Matrix Visual:
cm <- example
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1)))
  par(mar=c(2,2,2,2))
  plot(c(120, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('Refined Model p(X) = 0.5 Threshold', cex.main=1.5)
  
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


####################
# confusion matrix # FOR NEW p(x) = 0.68 cutoff
####################
library(caret)

#Creates vectors having data points
Predicted = c()
for (i in 1:length(data_new$p))
{
  if (data_new$p[i] <= 0.68)
  {
    Predicted[i] = 0 # Did not survive
  }else
  {
    Predicted[i] = 1 # Survived
  }
}
Predicted
sum(Predicted==1) # 177 survivals. 288 actually survived in our dataset.
sum(Predicted==0) # 535 survivals. 424 actually survived in our dataset.

Actual = c()
for (i in 1:length(data_new$Survived))
{
  if(data_new$Survived[i] == 1)
  {
    Actual[i] = 1
  } else
  {
    Actual[i] = 0
  }
}
Actual
sum(Actual == data_new$Survived) # So correct!


expected_value <- factor(Actual)
predicted_value <- factor(Predicted)

#Creating confusion matrix
example <- confusionMatrix(data=predicted_value, reference = expected_value)

#Display results 
example


#         Reference
#Prediction   0   1
#         0 408 127
#         1  16 161

example$overall[1]
# Accuracy : 0.7991573         
# 95% CI : (0.7678, 0.828)

# Confusion Matrix Visual:
cm <- example
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1)))
  par(mar=c(2,2,2,2))
  plot(c(120, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('Refined Model p(X) = 0.68 Threshold', cex.main=1.5)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='orange2')
  text(195, 435, 'Did Not Survive', cex=1.2)
  rect(250, 430, 340, 370, col='gray')
  text(295, 435, 'Survived', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='gray')
  rect(250, 305, 340, 365, col='orange2')
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


library(pROC)
par(pty = "s") # set plot type to square if looks weird.
roc(data$Survived,logis_glm2$fitted.values, plot = TRUE, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve", col = "steelblue", print.auc = TRUE)
# Area under curve = 0.8583

par(pty = "m") # Set back to maximum i.e. fill up the whole space provided.











# Ignore the following. Again, this approach is not valid but I have left it in 
# case it may be of use to anyone else.

##############################
### Finding optimal cutoff ###
##############################

#Actual = c()
#for (i in 1:length(data_new$Survived))
#{
#  if(data_new$Survived[i] == 1)
#  {
#    Actual[i] = 1
#  } else
#  {
#    Actual[i] = 0
#  }
#}
#Actual

#accuracy = c()
#for (j in seq(0.02, 0.97, by = 0.01)) # Only works between these probabilities for some reason.
#{
#  Predicted = c()
#  for (i in 1:length(data_new$p))
#  {
#    if (data_new$p[i] <= j)
#    {
#      Predicted[i] = 0 # Did not survive
#    }else
#    {
#      Predicted[i] = 1 # Survived
#    }
#  }

#  expected_value <- factor(Actual)
#  predicted_value <- factor(Predicted)
#  example <- confusionMatrix(data=predicted_value, reference = expected_value)
#  accuracy = append(accuracy,example$overall[1])
#  accuracy
#}
#accuracy = as.vector(accuracy)
#max(accuracy) # 0.8146067 which is observation 59 which would be p = 0.60 for cutoff
#accuracy[50:60]
#plot(accuracy, xlab = "Probability (%)", ylab = "Accuracy", main = "Finding Optimal Probability (Refined Model)", pch = 16, col = "blue",cex.lab=1.25, cex.axis=1.25, cex.main=1.5) 
# Here probability is Probability - 1%


# Conclusion: Best probability cutoff value is 0.55 for Full Model, and 0.60 for
# refined model. Both give exact same accuracy of 81.46%.