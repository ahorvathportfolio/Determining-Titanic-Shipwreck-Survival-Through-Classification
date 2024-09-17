# Master's Report Classification Project - Andras Horvath
# Shaping Data + Descriptive Plots of Original Data

###############
### DATASET ###
###############
# Titanic Data set
setwd("Your directory to titanic data set folder")
# df <- read.csv("train.csv", header = T) # Would use the original given trianing data set
df = read.csv("train - Copy1.csv", header = T) # This is a copy of the original training
# data set given by the competition. This step is not strictly necessary but I chose to 
# use a copy just in case to not accidentally alter or override the data.

mean(df$Fare) # 32.20421
# According to the CPI Inflation Calculator on the US Bureau of Labor Statistics
# website, $32.20 in April 1913 (does not go back to 1912) is equivalent to
# 1013.37 in January 2024.
# https://www.bls.gov/data/inflation_calculator.htm
df_new <- read.csv("train - Copy2.csv", header = T)
# train - Copy1.csv assigned NA values to the missing "Age" data but this was not done
# automatically for the "Cabin" variable. "train - Copy2.csv" is another copy of the
# original data set, but, I have manually updated the missing values to "NA" in this
# second copied file. Now df_new can be used to find just how many missing "Cabin"
# observations there are.
sum(is.na(df_new$Cabin)) # 687 cabins missing which is a lot. This is probably not missing
# at random either which is an issue for us.
sum(is.na(df$Age)) # 177 missing ages
# sum(is.na(df_new$Age)) # 177 missing ages would give the same


# I will now reshape the data to get just what we need and remove missing values
data = df[,-1] # remove passenger number
data = data[-3] # remove passenger name
data = data[-9] # remove cabin (as mostly missing data). I may come back to this later
# and do an analysis with the smaller data set with cabin included
data = data[-7] # Ticket variable is nightmare to deal with. Also, it is so unique
# I find it difficult to imagine to classify survivors with this in a sensible manner.
dim(data) # 891   8
data = na.omit(data) # remove any persons with missing ages.
dim(data) # 712   8
# Now we have a much nicer and clean data set to work with. Of course the removal of
# all of these observations is a major area to investigate further as many of these
# removed observations may be rather important for our end goals.

# Now I am going to alter the data set to be easier to read.
# Boxplot:
library(ggplot2)
survived = c()
for (i in 1:length(data$Survived))
{
  if (data$Survived[i] == 1)
  {
    survived[i] = "Survived"
  }else
  {
    survived[i] = "Did Not Survive"
  }
}
survived
survived = as.matrix(survived, nrows = 712)
colnames(survived) = "survived"
data_ggplot = cbind(data,survived) # Everything in the "Survived" and "survived"
# columns line up!

# We only have 2 quantitative variables (age and ticket fare). Let's investigate these guys.

# Sex vs Age Box and Whisker Plot
ggplot(data_ggplot, aes(x=Sex, y=Age, fill=Sex)) + 
  geom_boxplot(alpha=1) + facet_grid(~survived) + 
  theme(legend.position="none") + theme_bw()

# Now that the "Survived" variable is easier to read, let's do the same to "Class"
Class = c()
for (i in 1:length(data$Pclass))
{
  if (data$Pclass[i] == 1)
  {
    Class[i] = "Upper"
  } else if (data$Pclass[i] == 2)
  {
    Class[i] = "Middle"
  }else
  {
    Class[i] = "Lower"
  }
}
Class
Class = as.matrix(Class, nrows = 712)
colnames(Class) = "Economic Class"
data_ggplot = cbind(data_ggplot,Class) # Everything seems to line up again. Let's
# double check with the following 6 lines of code:

sum(data_ggplot$Pclass == 1) # 184
sum(data_ggplot$Pclass == 2) # 173
sum(data_ggplot$Pclass == 3) # 355
sum(data_ggplot$'Economic Class' == "Upper") # 184
sum(data_ggplot$'Economic Class' == "Middle") # 173
sum(data_ggplot$'Economic Class' == "Lower") # 355

# Class vs Age Box Plot
ggplot(data_ggplot, aes(x=Class, y=Age, fill=Class)) + 
  geom_boxplot(alpha=1) + facet_grid(~survived) + 
  theme(legend.position="none") + theme_bw()

ggplot(data_ggplot, aes(x=Embarked, y=Age, fill=Embarked)) + 
  geom_boxplot(alpha=1) + facet_grid(~survived) + 
  theme(legend.position="none") + theme_bw() + scale_fill_brewer(palette="Paired")

# Now how about the SibSp variable?
max(data$SibSp) #5
min(data$SibSp) #0
Siblings_Spouses = c()
for (i in 1:length(data$SibSp))
{
  if (data$SibSp[i] == 0)
  {
    Siblings_Spouses[i] = "zero"
  }else if (data$SibSp[i] == 1)
  {
    Siblings_Spouses[i] = "one"
  } else if (data$SibSp[i] == 2)
  {
    Siblings_Spouses[i] = "two"
  } else if (data$SibSp[i] == 3)
  {
    Siblings_Spouses[i] = "three"
  } else if (data$SibSp[i] == 4)
  {
    Siblings_Spouses[i] = "four"
  } else if (data$SibSp[i] == 5)
  {
    Siblings_Spouses[i] = "five"
  }
}
Siblings_Spouses
Siblings_Spouses = as.matrix(Siblings_Spouses, nrows = 712)
colnames(Siblings_Spouses) = "Siblings/Spouses"
data_ggplot = cbind(data_ggplot,Siblings_Spouses) # Everthing looks good again!

# Number of Siblings/Spouses vs Age
ggplot(data_ggplot, aes(x=factor(Siblings_Spouses, level=c('zero', 'one', 'two', 'three', 'four','five')), y=Age, fill=Siblings_Spouses)) + 
  geom_boxplot(alpha=1) + facet_grid(~survived) + 
  theme(legend.position="none") + theme_bw() + scale_fill_brewer(palette="Accent") + xlab("Number of Siblings/Spouses")

sum(data$SibSp == 5) # Only 5
sum(data$SibSp == 5 & data$Survived == 0) # 5
sum(data$SibSp == 5 & data$Survived == 1) # 0
# No one who had five siblings/spouses survived.


max(data$Parch) #6
min(data$Parch) #0
PARCH = c()
for (i in 1:length(data$Parch))
{
  if (data$Parch[i] == 0)
  {
    PARCH[i] = "zero"
  }else if (data$Parch[i] == 1)
  {
    PARCH[i] = "one"
  } else if (data$Parch[i] == 2)
  {
    PARCH[i] = "two"
  } else if (data$Parch[i] == 3)
  {
    PARCH[i] = "three"
  } else if (data$Parch[i] == 4)
  {
    PARCH[i] = "four"
  } else if (data$Parch[i] == 5)
  {
    PARCH[i] = "five"
  } else if (data$Parch[i] == 6)
  {
    PARCH[i] = "six"
  }
}
PARCH
PARCH = as.matrix(PARCH, nrows = 712)
colnames(PARCH) = "Parents/Children"
data_ggplot = cbind(data_ggplot,PARCH)


# Number of Siblings/Spouses vs Age
ggplot(data_ggplot, aes(x=factor(PARCH, level=c('zero', 'one', 'two', 'three', 'four','five', 'six')), y=Age, fill=PARCH)) + 
  geom_boxplot(alpha=1) + facet_grid(~survived) + 
  theme(legend.position="none") + theme_bw() + scale_fill_brewer(palette="Set3") + xlab("Number of Parents/Children")


# Sex vs Fare
ggplot(data_ggplot, aes(x=Sex, y=Fare, fill=Sex)) + 
  geom_boxplot(alpha=1) + facet_grid(~survived) + 
  theme(legend.position="none") + theme_bw()


# Class vs Fare
ggplot(data_ggplot, aes(x=Class, y=Fare, fill=Class)) + 
  geom_boxplot(alpha=1) + facet_grid(~survived) + 
  theme(legend.position="none") + theme_bw()

# Embarked vs Fare
ggplot(data_ggplot, aes(x=Embarked, y=Fare, fill=Embarked)) + 
  geom_boxplot(alpha=1) + facet_grid(~survived) + 
  theme(legend.position="none") + theme_bw() + scale_fill_brewer(palette="Paired")

# Number of Siblings/Spouses vs Fare
ggplot(data_ggplot, aes(x=factor(Siblings_Spouses, level=c('zero', 'one', 'two', 'three', 'four','five')), y=Fare, fill=Siblings_Spouses)) + 
  geom_boxplot(alpha=1) + facet_grid(~survived) + 
  theme(legend.position="none") + theme_bw() + scale_fill_brewer(palette="Accent") + xlab("Number of Siblings/Spouses")


# Number of Children/Parents vs Fare
ggplot(data_ggplot, aes(x=factor(PARCH, level=c('zero', 'one', 'two', 'three', 'four','five', 'six')), y=Fare, fill=PARCH)) + 
  geom_boxplot(alpha=1) + facet_grid(~survived) + 
  theme(legend.position="none") + theme_bw() + scale_fill_brewer(palette="Set3") + xlab("Number of Parents/Children")


#######################
# Keep Reshaping Data #
#######################
sex = as.numeric(as.factor(data$Sex))
sex
length(sex)
for (i in 1:length(sex))
{
  if (sex[i] == 2)
  {
    sex[i] = 0
  }
}
sex = as.matrix(sex, nrows = 712)
colnames(sex) = "sex"
data = cbind(data,sex)
data = data[-3]
# Sex is now binary and shown in the new variable "sex". 
# 0 = male
# 1 = Female

sum(data$Embarked == "S")
#[1] 554
sum(data$Embarked == "C")
#[1] 130
sum(data$Embarked == "Q")
#[1] 28

# I am just double checking something in this next line:
class(data$Age) # numeric as expected

embark = as.numeric(as.factor(data$Embarked))
embark # Now forced to be numeric so we can work with it
length(embark)
for (i in 1:length(embark))
{
  if (embark[i] == 3)
  {
    embark[i] = 0
  }
}
embark = as.matrix(embark, nrows = 712)
colnames(embark) = "embark"
data = cbind(data,embark)

sum(data$embark == 0) # Southampton
sum(data$embark == 1) # Cherbourg 
sum(data$embark == 2) # Queenstown

data = data[-7] # Get rid of original embark column

# Note: Ticket Class Breakdown (pclass)
# upper class =1, middle class=2, and lower class=3.

# Now that we have gotten all of the initial Box Plots to describe our data which
# is largely categorical, we have altered the "data" dataframe to have all numeric
# values for ease of analysis later on. To prevent having to go through all of this
# transformation again, go ahead and save "data" as "Cleaned Dataset.csv" for future use.
write.csv(data, "Your directory to titanic data set folder\\Cleaned Dataset.csv") 
