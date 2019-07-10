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

## ----include=FALSE-------------------------------------------------------
# drop Name 
train$Name <- NULL 

## ------------------------------------------------------------------------
train$GenderFac <- train$Sex # factor for plotting
train$IsMale <- ifelse(train$Sex=="male",1,0) # indicator for ML
train$Sex <- NULL # drop original

## ------------------------------------------------------------------------
# Logs of SibSp and Parch
train$SibSpLog <- log(train$SibSp+0.1)
train$ParchLog <- log(train$Parch+0.1)
# Change SibSp and Parch to factor
train$SibSp <- factor(train$SibSp)
train$Parch <- factor(train$Parch)

## ------------------------------------------------------------------------
library(knitr)
purl("Titanic_Survival.Rmd")

## ------------------------------------------------------------------------
sample_tickets <- train[train$Ticket %in% 2650:2670, colnames(train) %in% c("Ticket","Fare")]
head(sample_tickets[order(sample_tickets$Ticket), ],10)

## ------------------------------------------------------------------------
# ticket counts
counts <- aggregate(train$Ticket, by=list(train$Ticket), FUN=function(ticket) sum(!is.na(ticket)))
# override Fare with a "fare per person" value

train$FarePerPerson <- apply(train, 1, function(dat) as.numeric(dat["Fare"]) / counts[which(counts[,1] == dat["Ticket"]), 2])



## ------------------------------------------------------------------------
# drop Ticket
head(train[,c("Ticket","Fare","FarePerPerson")])
#train$Ticket <- NULL

## ------------------------------------------------------------------------
# create FareLog
table(train$Fare)
train$FareLog <- log(train$Fare+1)

## ------------------------------------------------------------------------
#install.packages('reshape')
library(reshape)
library(caret)
ticket.count <- aggregate(train$Ticket, by=list(train$Ticket), function(x) sum( !is.na(x) ))
ticket.count



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
hist(train$FareLog)

## ------------------------------------------------------------------------
# choose features for modeling
chosen <- c("Age","SibSp","Parch","Fare","Cabin","Embarked","SurvivedFac",
            "PclassFac","Title","NameLength","GenderFac")
yesAge <- train[!is.na(train$Age), colnames(train) %in% chosen] # with ages <- to model
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
#cplot tree
plot(age_mod)
text(age_mod, pretty=0)

## ----fig.height=5,fig.width=7--------------------------------------------
y_hat <- predict(age_mod, newdata=age_test)
y_test <- yesAge[!age_bool, "Age"]
test_RMSE <- round(sqrt(mean((y_hat - y_test)^2)),2)
plot(y_hat, y_test,ylab="Actual Age",xlab="Predicted Age",pch=19,col=rgb(0,0,1,0.3))
text(c(42,47),10, c("RMSE = ", test_RMSE))
abline(0,1, col="red",lty=2)

## ----fig.height=4,fig.width=6--------------------------------------------
suppressMessages(library(randomForest))
set.seed(1)
# checking various RMSEs
rf_RMSEs <- vector("numeric", length=10)
for (i in 1:10) {
  rf_age <- randomForest(Age ~., data=age_train, mtry=i, na.action=na.omit)
  rf_yhat <- predict(rf_age, newdata=age_test)
  rf_RMSEs[i] <- sqrt(mean((rf_yhat - y_test)^2,na.rm=TRUE))
}
plot(rf_RMSEs, ylim=range(rf_RMSEs), ylab="Root Mean Squared Error", col="red",
     xlab="Num. of Features Randomly Sampled at Each Split", type="l")

## ----fig.height=5,fig.width=7--------------------------------------------
set.seed(1)
rf_age <- randomForest(Age ~., data=age_train, mtry=8, na.action=na.omit)
rf_yhat <- predict(rf_age, newdata=age_test)
test_RMSE <- round(sqrt(mean((rf_yhat - y_test)^2, na.rm=TRUE)), 2)
plot(rf_yhat, y_test,ylab="Actual Age",xlab="Predicted Age",pch=19,col=rgb(0,0,1,0.5))
text(c(38,42),10, c("RMSE = ", test_RMSE))
abline(0,1, col="red",lty=2)

## ------------------------------------------------------------------------
set.seed(1)
rf_age <- randomForest(Age ~., data=yesAge, mtry=4, na.action=na.omit)
noAge$Age <- predict(rf_age, newdata=noAge)
round(colMeans(is.na(noAge))[colSums(is.na(noAge))>0]*100,2)

## ----fig.height=4,fig.width=5--------------------------------------------
varImpPlot(rf_age)

## ------------------------------------------------------------------------
set.seed(1)
yesAge <- yesAge[,colnames(yesAge) != "Cabin"]
noAge <- noAge[,colnames(noAge) != "Cabin"]
rf_age <- randomForest(Age ~., data=yesAge, mtry=4, na.action=na.omit)
# imputing Age predictions
train$Age[is.na(train$Age)] <- round(predict(rf_age, newdata=noAge),0)
sum(is.na(train$Age)) == 0

## ----fig.height=4.5, fig.width=9.5, echo=FALSE---------------------------
Child <- sum(ifelse(train$Age > 0 & train$Age < 13,1,0))
Teen <-  sum(ifelse(train$Age > 12 & train$Age < 20,1,0))
YoungAdult <- sum(ifelse(train$Age > 19 & train$Age < 36,1,0))
MiddleAged <- sum(ifelse(train$Age > 35 & train$Age < 56,1,0))
Elderly <- sum(ifelse(train$Age > 55 & train$Age < 100,1,0))
discrete_age_vec3 <- c(Child,Teen,YoungAdult,MiddleAged,Elderly)
par(mfrow=c(1,2))
# original values
barplot(discrete_age_vec1, col=terrain.colors(5), ylim=c(0,515),
        cex.names=.7, cex.main=0.8, cex.axis=0.8, las=2,
        main="Age Distribution with Missing Values",  space=c(0,0,0,0,0),
        names.arg=c("Child","Teen","Young Adult","Middle Aged","Elderly"))
# imputed values
barplot(discrete_age_vec3, col=terrain.colors(5), ylim=c(0,515),
        cex.names=.7, cex.main=0.8, cex.axis=0.8, las=2,
        main="Age Distribution with Imputed Values",  space=c(0,0,0,0,0),
        names.arg=c("Child","Teen","Young Adult","Middle Aged","Elderly"))

## ------------------------------------------------------------------------
train$AgeFac <- ifelse(train$Age > 0 & train$Age < 13, "Child",
                    ifelse(train$Age > 12 & train$Age < 20, "Teen",
                    ifelse(train$Age > 19 & train$Age < 36, "YoungAdult", 
                    ifelse(train$Age > 35 & train$Age < 56, "MiddleAged", "Elderly"))))
train$AgeFac <- factor(train$AgeFac)
train$AgeNum <- as.integer(train$Age)
train$Age <- NULL

## ------------------------------------------------------------------------
# Post-Processing summary
summary(train)

## ----fig.height=4, fig.width=9, echo=FALSE-------------------------------
par(mfrow=c(1,3))
# Survived, Pclass, Sex
plot(train$SurvivedFac, main="Survived", col=c("red","chartreuse3"))
plot(train$PclassFac, main="Passenger Class", col=c("chartreuse3", "cadetblue3", "chocolate1"))
plot(train$GenderFac, main="Sex", col=c("palevioletred1","cadetblue2"))

## ----fig.height=4, fig.width=9, echo=FALSE-------------------------------
par(mfrow=c(1,3))
par(oma=c(3,1,1,1))
plot(train$Cabin, main="Cabin", col=terrain.colors(6))
plot(train$Embarked, main="Port Embarked", col=terrain.colors(3),cex.axis=0.8, las=2)
train$Title <- factor(train$Title)
plot(train$Title, main="Titles", col=terrain.colors(5),cex.axis=0.8, las=2)

## ----fig.height=4.5, fig.width=8, echo=FALSE-----------------------------
par(mfrow=c(1,2))
hist(train$AgeNum, xlab='Age', main="Ages", ylab="", col=rgb(0,0.4,0.4,0.4))
hist(train$Fare, xlab="Fare (Pounds Sterling)", ylab="", main="Fares", col=rgb(1,0,0,0.3))
hist(log(train$Fare)*max(train$Fare)/8, col=rgb(0,0,1,0.2), ylab="", add=TRUE)
legend(250, 450, pch=15, col=rgb(0,0,1,0.3), "log distribution")

## ----fig.height=4.5, fig.width=8, echo=FALSE-----------------------------
# 
SS <- table(train$SibSp)
PC <- table(train$Parch)
counts <- rbind(SS,PC)
rownames(counts) <- c("Sibling or Spouse", "Parent or Child")
par(mfrow=c(1,1))
barplot(counts, main="Number of Siblings/Spouses vs Parents/Children",
  xlab="Number of Relatives", ylab="", col=c(rgb(0.2,0.4,0,0.3),rgb(0.2,0,0.5,0.3)),
  legend = rownames(counts), beside=TRUE)

## ------------------------------------------------------------------------
names(train)

## ------------------------------------------------------------------------
# combinations
head(t(data.frame(combn(11, 2))))
tail(t(data.frame(combn(11, 2))))

## ----fig.height=6, fig.width=8.5-----------------------------------------
# Scatterplot matrix
chosen <- c("SurvivedNum", "Pclass", "Sex","Age","Fare","Embarked")
plot(train[,colnames(train) %in% chosen])

## ----fig.height=4, fig.width=7, echo=FALSE-------------------------------
# 1. Survived and Pclass 
suppressMessages(require(ggplot2))
suppressMessages(require(ggmosaic))
Survival <- ifelse(train$SurvivedNum==1,"yes","no") # for ggplot
PclassFac <- factor(train$Pclass)
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survival, PclassFac),fill=Survival)) +
   labs(x='Passenger Class', y='', title='Tianic Survival by Passenger Class')

## ----fig.height=4, fig.width=6, echo=FALSE-------------------------------
# 2 Survived & Sex
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survival, GenderFac),fill=Survival)) +
   labs(x='Sex', y='', title='Tianic Survival by Gender')

## ----fig.height=5, fig.width=8.5, echo=FALSE-----------------------------
# 3 Survived & Age
plot(train$SurvivedNum~train$Age, pch=19, col=rgb(0,0,.6,.2),
    main="Titanic Survival by Age",ylab="Probability of Survival", xlab="Age")
linmod=lm(SurvivedNum~Age,data=train)
abline(linmod, col="green", lwd=2, lty=2)
g=glm(SurvivedNum~Age,family='binomial',data=train)
curve(predict(g,data.frame(Age=x),type="resp"),col="red",lty=2,lwd=2,add=TRUE) 
legend(60,0.7,c("linear fit","logistic fit"), col=c("green","red"), lty=c(1,2))

## ----fig.height=5, fig.width=8.5, echo=FALSE-----------------------------
# 4 Survived & SibSp
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survival, SibSp),fill=Survival)) +
   labs(x='Number of Siblings/Spouses', y='', 
   title='Tianic Survival by Number of Siblings or Spouses')

## ----fig.height=5, fig.width=8.5, echo=FALSE-----------------------------
# 5 Survived & Parch
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survival, Parch),fill=Survival)) +
   labs(x='Number of Parents/Children', y='', 
   title='Tianic Survival by Number of Parents or Children')

## ----fig.height=5, fig.width=8.5, echo=FALSE-----------------------------
# 6 Survived & Fare
plot(train$SurvivedNum~train$Fare, pch=19, col=rgb(0,0,.6,.2),
    main="Titanic Survival by Fare",ylab="Probability of Survival", xlab="Fare (Pounds Sterling)")
linmod=lm(SurvivedNum~Fare,data=train)
abline(linmod, col="green", lwd=1, lty=2)
g=glm(SurvivedNum~Fare,family='binomial',data=train)
curve(predict(g,data.frame(Fare=x),type="resp"),col="red",lty=2,lwd=2,add=TRUE) 
legend(200,0.7,c("linear fit","logistic fit"), col=c("green","red"), lty=c(1,2))

## ----fig.height=5, fig.width=8.5, echo=FALSE-----------------------------
# 6 Survived & Log(Fare)
train$FareLog <- log(train$Fare+1) # adding a pound to avoid Inf log values for 0 fares
# plot
plot(train$SurvivedNum~train$FareLog, pch=19, col=rgb(0,0,.6,.2),
    main="Titanic Survival by Log of Fare",
    ylab="Probability of Survival", xlab="Fare in Log(Pounds Sterling)")
linmod=lm(SurvivedNum~FareLog,data=train)
abline(linmod, col="green", lwd=1, lty=2)
g=glm(SurvivedNum~FareLog,family='binomial',data=train)
curve(predict(g,data.frame(FareLog=x),type="resp"),col="red",lty=2,lwd=2,add=TRUE) 
legend(1,0.7,c("linear fit","logistic fit"), col=c("green","red"), lty=c(1,2))

## ----fig.height=5, fig.width=8.5, echo=FALSE-----------------------------
# 7 Survived & Cabin
train2 <- train[!is.na(train$Cabin),] # copy of train w/o NAs
Survival2 <- ifelse(train2$SurvivedNum==1,"yes","no") 
ggplot(data=train2) +
   geom_mosaic(aes(x=product(Survival2, Cabin),fill=Survival2)) +
   labs(x='Cabin', y='', title='Tianic Survival by Cabin')

## ----fig.height=5, fig.width=8.5, echo=FALSE-----------------------------
# 8 Survived & Embarked 
PortEmbarked <- ifelse(train$Embarked=="C","Cherbourg", 
                ifelse(train$Embarked=="Q","Queenstown", "Southampton"))
dat <- data.frame(Survival, PortEmbarked)

ggplot(data=dat) +
   geom_mosaic(aes(x=product(Survival, PortEmbarked),fill=Survival)) +
   labs(x='Port of Embarkation', y='', 
   title='Tianic Survival by Port of Embarkation')

## ----fig.height=5, fig.width=9, echo=FALSE-------------------------------
# 8 Survived & Embarked & Fare
dat$FareLog <- train$FareLog
dat <- dat[!is.na(dat$PortEmbarked),]
ggplot(data=dat) +
   geom_boxplot(aes(x=PortEmbarked,y=FareLog, fill=Survival)) +
   labs(x='Port of Embarkation', y='Fare in Log(Pounds Sterling)', 
   title='Titanic Survival by Port of Embarkation and Fare')

## ----fig.height=5, fig.width=9, echo=FALSE-------------------------------
# 9 Survival and Title
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survival, Title),fill=Survival)) + 
   labs(x='Title', y='',
   title='Tianic Survival by Title') + 
   theme(axis.text.x = element_text(angle = 90))

## ----fig.height=4.5, fig.width=8, echo=FALSE-----------------------------
# 10 Survived and NameLength
plot(train$SurvivedNum~train$NameLength, pch=19, col=rgb(0,0,.6,.2),
    main="Titanic Survival by Name Length",
    ylab="Probability of Survival", xlab="Name Length (chars)")
linmod=lm(SurvivedNum~NameLength,data=train)
abline(linmod, col="green", lwd=1, lty=2)
g=glm(SurvivedNum~NameLength,family='binomial',data=train)
curve(predict(g,data.frame(NameLength=x),type="resp"),col="red",lty=2,lwd=2,add=TRUE) 
legend(60,0.5,c("linear fit","logistic fit"), col=c("green","red"), lty=c(1,2))

## ------------------------------------------------------------------------
# Combinations of 3 or more variables quickly explode
vars <- 1:11
for (i in 2:9) {
	num <- length(combn(vars,i))/i
	print(paste("There are ", num, "combinations of 11 variables taken", i, "at a time."))
}

