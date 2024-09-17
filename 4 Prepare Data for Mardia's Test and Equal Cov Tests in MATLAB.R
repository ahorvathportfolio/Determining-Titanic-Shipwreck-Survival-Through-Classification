# Master's Report Classification Project - Andras Horvath
# Prepare Data for Mardia's Test and Equal Cov Tests in MATLAB

#############
### Goal: ###
#############
# The purpose of checking for Normality and checking for the Variance-Covariance
# Structure is to determine whether LDA and/or QDA are valid.

# Attempt Mardia's Test and Equal Cov Tests in R:
# install.packages("MVN")
library(MVN)
result <- mvn(data = data, mvnTest = "mardia")
result
names(result)
result$multivariateNormality # Not normal according to Mardia Test. This will be
# double checked in MATLAB.

# Subset data for Equal Cov test in Matlab:
setwd("Your directory to titanic data set folder")
data = read.csv("Cleaned Dataset.csv", header = T)
data = data[-1] # remove this pesky column that keeps track of the observations

X1 = data[data$Survived==1,2:8] # X will be our explanatory variables. Our response is Survival.
X2 = data[data$Survived==0,2:8]
write.csv(X1, "Your directory to titanic data set folder\\For Matlab tests\\X1.csv") 
write.csv(X2, "Your directory to titanic data set folder\\For Matlab tests\\X2.csv") 

# You will notice that there will also be a DatasetM file used in addition to X1 and X2.
# This DatasetM will just be the "Cleaned Dataset.csv" with the column names removed.

##############
### MATLAB ###
##############

# Description:

# -- The Mardia_EqualCov_Test_Horvath file is where we will perform the Mardia
#    test and check for Equal Covariance Matrix structure.
# -- The mardiatest and EqualCovtest files are the helper files provided to me
#    by my adviser, Dr. Ellingson, from a previous course to aid in fulfilling
#    the above bullet point.