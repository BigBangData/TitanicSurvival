## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=FALSE----------------------------------------------------------
# Load training set 
rm(list=ls()) 
train <- read.csv("../input/train.csv", na.strings="")
str(train)

## ------------------------------------------------------------------------
# Preliminary summary
summary(train)

## ------------------------------------------------------------------------
head(train)

## ------------------------------------------------------------------------
tail(train)

## ----include=FALSE-------------------------------------------------------
# PassengerId: Trust but Verify
sum(duplicated(train$PassengerId)) == 0 # no duplicates
sum(train$PassengerId == 1:891) == 891 # stepwise values
# drop PassengerId
train$PassengerId <- NULL

## ------------------------------------------------------------------------
# Survived
train$SurvivedFac <- ifelse(train$Survived=="1","yes","no")
train$SurvivedFac <- factor(train$Survived) 
train$SurvivedNum <- as.numeric(train$Survived) 
train$Survived <- NULL # drop original

## ------------------------------------------------------------------------
# Pclass
train$PclassFac <- ifelse(train$Pclass==1, "1st Class", ifelse(train$Pclass==2, "2nd Class", "3rd Class"))
train$PclassFac <- factor(train$PclassFac)
train$PclassNum <- as.integer(train$Pclass)
train$Pclass <- NULL

## ------------------------------------------------------------------------
# Create Title attribute
train$Title <- vector("character",length=nrow(train))
for (i in 1:nrow(train)) {
	x <- as.character(train$Name[i])
	m <- regexec(",(\\s+\\w+)+\\.", x) 
	train$Title[i] <- unlist(strsplit(unlist(regmatches(x,m))," "))[2]
}
# looking at unique titles
unique(train$Title)

## ------------------------------------------------------------------------
# Clean up Title
common_titles <- c("Mr.", "Mrs.", "Miss.")
rare_male <- c("Don.","Rev.","Dr.","Major.","Master.", "Sir.","Col.","Capt.","Jonkheer.")
for (i in 1:nrow(train)) {
	train$Title[i] <- ifelse(train$Title[i] %in% common_titles, train$Title[i], # do not replace
	                         ifelse(train$Title[i] %in% rare_male, "rareMale", "rareFemale"))
}
train$Title <- factor(train$Title)
# unique titles
unique(train$Title)

## ----echo=FALSE----------------------------------------------------------
# create NameLength attribute
train$NameLength <- vector("numeric", nrow(train))
for (i in 1:nrow(train)) {
	train$NameLength[i] <- nchar(as.character(train$Name)[i])
}
# see whether NameLength is useful
plot(train$NameLength, train$Fare, 
	pch=19, col=rgb(0,0,1,alpha=0.2),
	xlab="Name Length (chars)", 
	ylab="Fare (Pounds)")
abline(lm(train$Fare ~ train$NameLength), col="red")

## ------------------------------------------------------------------------
train$GenderFac <- train$Sex # factor for plotting
train$IsMale <- ifelse(train$Sex=="male",1,0) # indicator for ML
train$Sex <- NULL # drop original

## ------------------------------------------------------------------------
# Change SibSp and Parch to factor
train$SiblingSpouse <- factor(train$SibSp)
train$ParentChildren <- factor(train$Parch)
train$NumRelatives <- train$SibSp + train$Parch
train$NumRelatives <- factor(train$NumRelatives)
train$SibSp <- NULL
train$Parch <- NULL

## ------------------------------------------------------------------------
# EDA into Ticket and Fare
temp_dfm <- train[, colnames(train) %in% c("Name","Ticket","Fare")]
temp_dfm <- temp_dfm[order(temp_dfm["Ticket"]),] # order by Ticket
temp_dfm[1:10,]

## ------------------------------------------------------------------------
# keep counts of tickets
counts <- aggregate(train$Ticket, by=list(train$Ticket), 
                      FUN=function(ticket) sum(!is.na(ticket)))
# function that takes a data frame's fare and ticket counts and apply
divide_fare_count <- function(dfm) {
  fare <- as.numeric(dfm["Fare"])
  # ticket counts
  count_given_ticket <- counts[which(counts[,1] == dfm["Ticket"]), 2]
  result <- round(fare/count_given_ticket,2)
  return(result)
}
# create FarePerPerson
train$FarePerPerson <- apply(X=train, MARGIN=1, FUN=divide_fare_count)

# looking at the temp dataframe of results again
chosen <- c("Name","Ticket","Fare","FarePerPerson")
temp_dfm <- train[, colnames(train) %in% chosen]
temp_dfm <- temp_dfm[order(temp_dfm["Ticket"]),] # order by Ticket
temp_dfm[1:10,]

## ------------------------------------------------------------------------
# create TicketCount
train$TicketCount <- apply(X=train, MARGIN=1, FUN=function(dfm) counts[which(counts[,1] == dfm["Ticket"]), 2])
# drop Fare, Name, and Ticket
'%ni%' <- Negate('%in%')
not_chosen <- c("Fare","Name","Ticket")
train <- train[,colnames(train) %ni% not_chosen]

## ------------------------------------------------------------------------
# create FareLog
train$FarePerPersonLog <- log(train$FarePerPerson+1)

## ------------------------------------------------------------------------
names(train)

## ----echo=FALSE----------------------------------------------------------
# Cleaning up Cabin
train$CabinClean <- vector("character", nrow(train))
for (i in 1:nrow(train)) {
  # ID digits and white space
	pattern <- "[0-9]*|\\s"
  # reduce to only first letter given multiple cabins
	train$CabinClean[i] <- substr(gsub(pattern, "", train$Cabin[i]),1,1)
	# bin letters higher than F to the F category
	high_cabins <- toupper(letters[letters >"f"])
	if (train$CabinClean[i] %in% high_cabins) train$CabinClean[i] <- "F"
}
# replace old Cabin
train$Cabin <- factor(train$CabinClean)
train$CabinClean <- NULL

## ------------------------------------------------------------------------
summary(train$Cabin)

## ------------------------------------------------------------------------
train$Embarked <- ifelse(train$Embarked=="C","Cherbourg",
                         ifelse(train$Embarked=="Q","Queensland","Southhampton"))
train$Embarked[is.na(train$Embarked)] <- "Southhampton"
train$Embarked <- factor(train$Embarked) # re-factoring

## ----fig.height=4.5, fig.width=9, echo=FALSE-----------------------------
par(mfrow=c(1,2))
hist(train$Age, xlab='Age', main="Before Imputation", ylab="", 
     col=rgb(0,0.4,0.4,0.4), ylim=c(0,420))
AgeCopy <- train$Age
AgeCopy[is.na(AgeCopy)] <- median(AgeCopy, na.rm=TRUE)
hist(AgeCopy, xlab='Age', main="After Imputation with Medians", ylab="", 
     col=rgb(0.4,0,0.4,0.4), ylim=c(0,420))

## ------------------------------------------------------------------------
# choose features for modeling
chosen <- c("Age","SurvivedFac","PclassFac","Title","NameLength",
            "SiblingSpouse","ParentChildren","FarePerPerson","TicketCount")
yesAge <- train[!is.na(train$Age), colnames(train) %in% chosen] # with ages <- to train and evaluate models
noAge <- train[is.na(train$Age), colnames(train) %in% chosen] # without ages <- to predict
# drop the outcome since it only has missing values
noAge$Age <- NULL

## ----fig.height=8, fig.width=9-------------------------------------------
library(tree)
library(caTools)
set.seed(1) 
# split on outcome
Y_age <- yesAge[, "Age"]
age_bool <- sample.split(Y_age, SplitRatio = 2/3) 
age_train <- yesAge[age_bool, ]
age_test <- yesAge[!age_bool, colnames(yesAge) != "Age"]
# fit model
age_mod <- tree(Age~.,data=age_train)
# plot tree
plot(age_mod)
text(age_mod, pretty=0)

## ----fig.height=5.5,fig.width=5.5----------------------------------------
y_hat <- predict(age_mod, newdata=age_test)
y_test <- yesAge[!age_bool, "Age"]
test_RMSE <- round(sqrt(mean((y_hat - y_test)^2)),2)
plot(y_hat, y_test,ylab="Actual Age",xlab="Predicted Age",pch=19,col=rgb(0,0,1,0.3))
text(c(35,40),10, c("RMSE = ", test_RMSE))
abline(0,1, col="red",lty=2)

## ----fig.height=4,fig.width=6--------------------------------------------
suppressMessages(library(randomForest))
# split on outcome
set.seed(1)
Y_age <- yesAge[, "Age"]
age_bool <- sample.split(Y_age, SplitRatio = 2/3) 
age_train <- yesAge[age_bool, ]
age_test <- yesAge[!age_bool, colnames(yesAge) != "Age"]
y_test <- yesAge[!age_bool, "Age"]

# checking various RMSEs
rf_RMSEs <- vector("numeric", length=8)
for (i in 1:8) {
  rf_age <- randomForest(Age ~., data=age_train, mtry=i, na.action=na.omit)
  rf_yhat <- predict(rf_age, newdata=age_test)
  rf_RMSEs[i] <- sqrt(mean((rf_yhat - y_test)^2,na.rm=TRUE))
}
plot(rf_RMSEs, ylim=range(rf_RMSEs), ylab="Root Mean Squared Error", col="red",
     xlab="Num. of Features Randomly Sampled at Each Split", type="l")

## ----fig.height=5.5,fig.width=5.5----------------------------------------
rf_age <- randomForest(Age ~., data=age_train, mtry=2, na.action=na.omit)
rf_yhat <- predict(rf_age, newdata=age_test)
test_RMSE <- round(sqrt(mean((rf_yhat - y_test)^2, na.rm=TRUE)), 2)
plot(rf_yhat, y_test,ylab="Actual Age",xlab="Predicted Age",pch=19,col=rgb(0,0,1,0.5))
text(c(38,45),10, c("RMSE = ", test_RMSE))
abline(0,1, col="red",lty=2)

## ----fig.height=4,fig.width=5--------------------------------------------
varImpPlot(rf_age)

## ------------------------------------------------------------------------
set.seed(1)
rf_age <- randomForest(Age ~., data=yesAge, mtry=2, na.action=na.omit)
# imputing Age predictions
train$Age[is.na(train$Age)] <- round(predict(rf_age, newdata=noAge),0)
sum(is.na(train$Age)) == 0

## ----fig.height=4.5, fig.width=9, echo=FALSE-----------------------------
par(mfrow=c(1,2))
hist(train$Age, xlab='Age', main="After Random Forest Imputation", ylab="", 
     col=rgb(0,0.4,0.4,0.4), ylim=c(0,420))
hist(AgeCopy, xlab='Age', main="After Imputation with Medians", ylab="", 
     col=rgb(0.4,0,0.4,0.4), ylim=c(0,420))

## ------------------------------------------------------------------------
# create AgeFac for Age Categories
train$AgeFac <- ifelse(train$Age > 0 & train$Age < 13, "Child",
                    ifelse(train$Age > 12 & train$Age < 20, "Teen",
                    ifelse(train$Age > 19 & train$Age < 36, "YoungAdult", 
                    ifelse(train$Age > 35 & train$Age < 56, "MiddleAged", "Elderly"))))
train$AgeFac <- factor(train$AgeFac)
train$AgeNum <- as.integer(train$Age)
train$Age <- NULL

## ------------------------------------------------------------------------
new_order <- c("Cabin","Embarked","PclassNum","PclassFac","NameLength","Title","IsMale",
               "GenderFac","SiblingSpouse","ParentChildren","NumRelatives","TicketCount",
               "FarePerPerson","FarePerPersonLog","AgeNum","AgeFac","SurvivedNum","SurvivedFac")
train <- train[,new_order]
head(train)

## ------------------------------------------------------------------------
# Summary after pre-processing
summary(train)

## ------------------------------------------------------------------------
names(train)

## ----fig.height=5, fig.width=9, echo=FALSE-------------------------------
par(mfrow=c(1,2)); par(oma=c(3,1,1,1))
plot(train$Cabin, main="Cabin", col=terrain.colors(6))
plot(train$Embarked, main="Port Embarked", col=terrain.colors(3), cex.axis=0.8, cex.sub=0.8, las=2)

## ----fig.height=5, fig.width=9, echo=FALSE-------------------------------
par(mfrow=c(1,2)); par(oma=c(3,1,1,1))
plot(train$PclassFac, main="Pclass Categorical", col=terrain.colors(3),cex.axis=0.8, las=2)
hist(train$NameLength, main="Name Lengths", xlab="Length (characters)", col="steelblue")

## ----fig.height=5, fig.width=9, echo=FALSE-------------------------------
par(mfrow=c(1,2)); par(oma=c(3,1,1,1))
plot(train$Title, main="Titles", col=terrain.colors(5),cex.axis=0.8, las=2)
plot(train$GenderFac, main="Sex", col=c("palevioletred1","cadetblue2"))

## ----fig.height=5, fig.width=9, echo=FALSE-------------------------------
SS <- table(train$SiblingSpouse)
PC <- table(train$ParentChildren)
counts <- rbind(SS,PC)
rownames(counts) <- c("Sibling or Spouse", "Parent or Child")
par(mfrow=c(1,1))
barplot(counts, main="Number of Siblings/Spouses vs Parents/Children",
  xlab="Number of Relatives", ylab="", col=c(rgb(0.2,0.4,0,0.3),rgb(0.2,0,0.5,0.3)),
  legend = rownames(counts), beside=TRUE)

## ------------------------------------------------------------------------
library(knitr)
purl("Titanic_Survival.Rmd", documentation=2)

