# EDA  
# ===

# Load training set 
rm(list=ls())
train <- read.csv("./titanic/train.csv", na.strings="")

# Dimensions
dim(train)

# Look at top rows
head(train,10)

# Get a fuller picture with summary
summary(train)

# notice NAs 
# notice class imbalance in Sex 
# Maybe later use stratification for gender or subsample from male (SMOTE)

# Look at the structure (metadata)
str(train)

# Index = PassengerId: drop later, just an ID, equal to row number 
# trust but verify? 
# no duplicates 
sum(duplicated(train$PassengerId)) == 0
# all stepwise 
sum(train$PassengerId == 1:891) == 891


# Univariate EDA 
# ===============

# Var1 = Survived 
library(ggplot2)
library(ggmosaic)

# Var2 = Pclass 
table(train$Pclass)

# Var3 = Sex 
table(train$Sex)


# Var8 = Cabin 
table(train$Cabin)

# Var9 = Embarked
table(train$Embarked)
#  C   Q   S 
#168  77 644 
# create 2 dummies 
# Var10 = Title 
table(train$Title)


# Var5 = SibSp
table(train$SibSp) # num siblings/spouses
#  0   1   2   3   4   5   8 
#608 209  28  16  18   5   7 
# use log(train$SibSp)?

# Var6 = Parch
table(train$Parch) # num parent/children
#  0   1   2   3   4   5   6 
#678 118  80   5   4   5   1 
# hm...

# Var4 = Age 
hist(train$Age)
# needs imputation, fix Cabin first: too many levels 

# Var7 = Fare 
hist(train$Fare)
# use log fare?
hist(log(train$Fare))
 
# skip NameLength



# Bivariate EDA 
# ==============

# there are (n * (n-1)) / 2 == (11 * 10)/2 == 55 possible bivariate combinations (regardless of order)
# Noticing that a scatterplot matrix would not plot every relationship exactly how we'd want it 

# Going through the 9 relationships with Survival since that is the outcome

# 1. Survived and Pclass 
# mosaic plot 
library(ggmosaic)
Survival <- ifelse(train$Survived==1,"yes","no")
PclassFac <- factor(train$Pclass)
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survival, PclassFac),fill=Survival)) +
   labs(x='Passenger Class', y='', title='Tianic Survival by Passenger Class')



# 2. Survived and Sex:
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survived, Sex),fill=Survived)) +
   labs(x='Sex', y='', title='Tianic Survival by Sex')

# 4. Survived and Age:
train$Survived <- as.numeric(train$Survived)
train$Survived <- ifelse(train$Survived==1,0,1)
plot(train$Survived~train$Age)
l=lm(Survived~Age,data=train)
abline(l,col="blue",lwd=3)
g=glm(Survived~Age,family=binomial,data=train)
curve(predict(g,data.frame(Age=x),type="resp"),col="red",lty=2,lwd=2,add=TRUE) 

# 5. Survived and SibSp
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survived, SibSp),fill=Survived)) +
   labs(x='Number of Siblings/Spouses', y='', 
   title='Tianic Survival by Number of Siblings or Spouses')
   
# Having 1 sibling (or spouse, not sure why they conflated this) is best, 
# 2 is okay, no is "fine", but 3 or more is not good... 
# yet, as the number increases, your confidence should decrease
#  because there is less and less evidence for this effect

# 6. Survived and Parch
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survived, Parch),fill=Survived)) +
   labs(x='Number of Parents/Children', y='', 
   title='Tianic Survival by Number of Parents or Children')
# Having 3 children is oddly enough a good thing?


# HERE +++++++++++++++++++++++++++++++++++++++++++++++++++++++
   
# 7. Survived and Fare 
rm(list=ls())
train <- read.csv("./titanic/train.csv", na.strings="")
plot(train$Survived~train$Fare)
g=glm(Survived~Fare,family='binomial',data=train)
curve(predict(g,data.frame(Fare=x),type="resp"),col="red",lty=2,lwd=2,add=TRUE) 


train$FareLog <- log(train$Fare+0.001)

# AND AGE 
train$Survived <- as.numeric(train$Survived)
plot(train$Survived~train$Age, ylab="Probability of Survival", xlab="Age")
linmod=lm(Survived~Age,data=train)
abline(linmod, col="blue", lwd=2, lty=2)
g=glm(Survived~Age,family='binomial',data=train)
curve(predict(g,data.frame(Age=x),type="resp"),col="red",lty=2,lwd=2,add=TRUE) 


train$FareLog <- log(train$Fare)




# 8. Survived and Cabin:
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survived, Cabin),fill=Survived)) +
   labs(x='Cabin', y='', 
   title='Tianic Survival by Cabin')


Survival <- ifelse(train$SurvivedNum==1,"yes","no") # for ggplot

train2 <- train[!is.na(train$Cabin),]
Survival2 <- ifelse(train2$SurvivedNum==1,"yes","no") 










# 9. Survived and Embarked 
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survived, Embarked),fill=Survived)) +
   labs(x='Port Embarkation', y='', 
   title='Tianic Survival by Port of Embarkation')


# Survival and Title 
Survival <- ifelse(train$Survived==1,"yes","no")
TitleFac <- factor(train$Title)
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survival, TitleFac),fill=Survival)) + 
   labs(x='Title', y='',
   title='Tianic Survival by Title')
   
  
# 10. Survived and NameLength
Survival <- ifelse(train$Survived==1,"yes","no")
plot(Survival~train$NameLength)
g=glm(Survival~train$NameLength,family=binomial)
curve(predict(g,data.frame(train$NameLength=x),type="resp"),add=TRUE) 
# THIS DIDN'T WORK IN RSTUDIO!



# OTHER BI-VARIATE

# the higher your age, the lesser the number of siblings/spouses, 
# this could be the counfounding factor for number of siblings/spouses and survivability
plot(train$Age,train$SibSp)

# the higher your age, the higher the fare 
plot(train$Fare,train$Age)
abline(lm(train$Age~train$Fare))

# where Embarked means different fares (boxplots)
plot(train$Embarked,log(train$Fare))
# higher fares in order: C, S, Q <- so choose Q as baseline when creating dummies 


Survival <- ifelse(train$Survived==1,"yes","no")

PortEmbarked <- ifelse(train$Embarked=="C","Cherbourg", 
                ifelse(train$Embarked=="Q","Queenstown", "Southampton"))
   
   
   

# Multivariate EDA
# ================

# Combinations of 3 or more variables quickly explode
vars <- 1:11
for (i in 2:5) {
	num <- length(combn(vars,i))/i
	print(paste("There are ", num, "combinations of 11 variables taken", i, "at a time."))
}

# So analysis is mostly ad-hoc and based on hunches

# Survival of children and Pclass

# Survival of Children first: defining children as Age < 16
train$Child <- ifelse(train$Age < 16,1,0)
train.copy <- train 
train.copy <- train.copy[!is.na(train.copy$Age),] # excluding NAs from consideration for now
Survival <- ifelse(train.copy$Survived==1,"yes","no")
ggplot(data=train.copy) +
   geom_mosaic(aes(x=product(Survival, Child),fill=Survival)) +
   labs(x='Children or Not (1 = up to 15 yrs)', y='', title='Titanic Survival of Children by Passenger Class') + 
   facet_grid(rows=factor(train.copy$Pclass))

# children in 2nd class survived the best,
# there's a rising percentage of children as class rises,
# adults in 1st class survived more than children in the 3rd class



# Did single males survive more than family men?


# Ex. Embarked + Gender + Age (Panels)

# Fare and Sex?
#hist(log(train$Fare[train$Sex=="male"]),col=rgb(0,0,1,0.2))
#hist(log(train$Fare[train$Sex=="female"]), add=TRUE, col=rgb(1,0,0,0.2))
summary(train$Fare[train$Sex=="male"])
summary(train$Fare[train$Sex=="female"])

par(mfrow=c(1,2))
boxplot(log(train$Fare[train$Sex=="male"]),col=rgb(0,0,1,0.2),main="Male",ylab="log(Fare)")
boxplot(log(train$Fare[train$Sex=="female"]), col=rgb(1,0,0,0.2),main="Female")
par(mfrow=c(1,1))


# PRE-PROCESSING
# ===============

# change Sex to IsMale
train$IsMale <- ifelse(train$Sex=="male",1,0)
train$Sex <- NULL

# Embarked: dummies
train$EmbarkedCherbourg <- ifelse(train$Embarked=="C",1,0)
train$EmbarkedSouthampton <- ifelse(train$Embarked=="S",1,0)
train$Embarked <- NULL

# could create an IsRich variable but wouldn't that already be captured by Fare?
# it definitely correlates with Survived so it would be a good feature...
rich <- train[train$Fare > mean(train$Fare),]
poor <- train[train$Fare <= mean(train$Fare),]
table(rich$Survived)/nrow(rich)
table(poor$Survived)/nrow(poor)
train$IsRich <- ifelse(train$Fare > mean(train$Fare),1,0)


# Cabin:

# Age: after Cabin 
library(tree)
library(caTools)
set.seed(42)
chosen <- c("Survived","Pclass","Sex","SibSp","Parch","Fare","Cabin","Embarked","Title","Age")
age <- train[,which(colnames(train) %in% chosen)]

age_bool <- sample.split(age, SplitRatio = 0.75) 
age_train <- age[age_bool == TRUE,]
age_test <- age[age_bool == FALSE,]

tree.age <- tree(Survived~.,age_train)
summary(tree.age)

# use the ISLR.Lab on Regression Trees




# drop PassengerId and Survived!


# Feature Engineering:
# ===================

# log(Fare)?

# SibSp*Parch or SibSp+Parch?

# Age * Class?
# Fare per Person?









# Modeling
# --- don't forget: NaiveBayes (or GaussianNB?)




