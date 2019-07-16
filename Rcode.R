## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
# PassengerId: Trust but Verify
sum(duplicated(train$PassengerId)) == 0 # no duplicates
sum(train$PassengerId == 1:891) == 891 # stepwise values
# drop PassengerId
train$PassengerId <- NULL

## ------------------------------------------------------------------------
# Survived
train$SurvivedFac <- ifelse(train$Survived=="1","yes","no")
train$SurvivedFac <- factor(train$Survived, levels=0:1, labels=c("no","yes"))
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

## ------------------------------------------------------------------------
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
train$TicketCount <- apply(X=train, MARGIN=1, 
                          FUN=function(dfm) counts[which(counts[,1] == dfm["Ticket"]), 2])
# drop Fare, Name, and Ticket
'%ni%' <- Negate('%in%')
not_chosen <- c("Fare","Name","Ticket")
train <- train[,colnames(train) %ni% not_chosen]

## ------------------------------------------------------------------------
# create FareLog
train$FarePerPersonLog <- log(train$FarePerPerson+1)

## ------------------------------------------------------------------------
names(train)

## ------------------------------------------------------------------------
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

## ----fig.height=4.5, fig.width=9-----------------------------------------
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

## ----fig.height=4.5, fig.width=9-----------------------------------------
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
train$AgeFac <- factor(train$AgeFac, levels=c("Child","Teen","YoungAdult","MiddleAged","Elderly"))
train$AgeNum <- as.integer(train$Age)
train$Age <- NULL


## ------------------------------------------------------------------------
new_order <- c("Cabin","Embarked","PclassNum","PclassFac","NameLength","Title","IsMale",
               "GenderFac","SiblingSpouse","ParentChildren","NumRelatives","TicketCount",
               "FarePerPerson","FarePerPersonLog","AgeNum","AgeFac","SurvivedNum","SurvivedFac")
train <- train[,new_order]
head(train)

## ------------------------------------------------------------------------
# Summary After Pre-Processing
summary(train)

## ------------------------------------------------------------------------
names(train)

## ----fig.height=5, fig.width=9-------------------------------------------
par(mfrow=c(1,2)); par(oma=c(3,1,1,1))
plot(train$Cabin, main="Cabin", col=terrain.colors(6))
plot(train$Embarked, main="Port Embarked", col=terrain.colors(3), cex.axis=0.8, cex.sub=0.8, las=2)

## ----fig.height=5, fig.width=9-------------------------------------------
par(mfrow=c(1,2)); par(oma=c(3,1,1,1))
plot(train$PclassFac, main="Pclass Categorical", col=terrain.colors(3),cex.axis=0.8, las=2)
hist(train$NameLength, main="Name Lengths", xlab="Length (characters)", col="steelblue")

## ----fig.height=5, fig.width=9-------------------------------------------
par(mfrow=c(1,2)); par(oma=c(3,1,1,1))
plot(train$Title, main="Titles", col=terrain.colors(5),cex.axis=0.8, las=2)
plot(train$GenderFac, main="Sex", col=c("palevioletred1","cadetblue2"))

## ----fig.height=5, fig.width=9-------------------------------------------
SS <- table(train$SiblingSpouse)
PC <- table(train$ParentChildren)
counts <- rbind(SS,PC)
rownames(counts) <- c("Sibling or Spouse", "Parent or Child")
par(mfrow=c(1,1))
barplot(counts, main="Number of Siblings/Spouses vs Parents/Children",
  xlab="Number of Relatives", ylab="", col=c(rgb(0.2,0.4,0,0.3),rgb(0.2,0,0.5,0.3)),
  legend = rownames(counts), beside=TRUE)

## ----fig.height=5, fig.width=9-------------------------------------------
par(mfrow=c(1,2))
barplot(table(train$NumRelatives), main="Total Number of Relatives",
  xlab="Number of Relatives", ylab="", col=rgb(0.4,0.1,0.2,0.3))

barplot(table(train$TicketCount), main="Group Tickets",
  xlab="Number of People per Ticket", ylab="", col=rgb(0,0.1,0.9,0.3))

## ----fig.height=5, fig.width=9-------------------------------------------
par(mfrow=c(1,1))
hist(train$FarePerPerson, xlab="Fare (Pounds Sterling)", ylab="", main="Fares", col=rgb(1,0,0,0.3))
multiplication_factor <- max(train$FarePerPerson)/max(train$FarePerPersonLog)
hist(train$FarePerPersonLog*multiplication_factor, col=rgb(0,0,1,0.2), ylab="", add=TRUE)
legend(150, 450, pch=15, col=c(rgb(1,0,0,0.4),rgb(0,0,1,0.5)), c("Fare","Log(Fare) scaled up"))

## ----fig.height=8, fig.width=9-------------------------------------------
par(mfrow=c(2,1))
hist(train$AgeNum, xlab='Age (yrs)', main="Age Distribution", ylab="", col=terrain.colors(8))
barplot(table(train$AgeFac), xlab='Age Group', ylab="", col=terrain.colors(5), space=c(0,0,0,0,0))

## ----fig.height=4.5, fig.width=4.5---------------------------------------
par(mfrow=c(1,1))
# Survived
plot(train$SurvivedFac, main="Survived", col=c("red","chartreuse3"))

## ------------------------------------------------------------------------
names(train)

## ------------------------------------------------------------------------
# combinations
head(t(data.frame(combn(12, 2))))
tail(t(data.frame(combn(12, 2))))

## ----fig.height=6, fig.width=8.5-----------------------------------------
# Scatterplot matrix
chosen <- c("SurvivedFac", "PclassNum", "GenderFac","AgeNum","FarePerPerson","Embarked")
plot(train[,colnames(train) %in% chosen])

## ----fig.height=5, fig.width=8.5, message=FALSE, warning=FALSE-----------
# 1 Survived & Cabin
suppressMessages(library(ggplot2))
suppressMessages(library(ggmosaic))
train2 <- train[!is.na(train$Cabin),] # copy of train w/o NAs
Survival2 <- ifelse(train2$SurvivedNum==1,"yes","no") 
ggplot(data=train2) +
   geom_mosaic(aes(x=product(Survival2, Cabin),fill=Survival2)) +
   labs(x='Cabin', y='', title='Tianic Survival by Cabin')

## ----fig.height=5, fig.width=8.5-----------------------------------------
# 2 Survived & Embarked 
ggplot(data=train) +
   geom_mosaic(aes(x=product(SurvivedFac, Embarked),fill=SurvivedFac )) +
   labs(x='Port of Embarkation', y='', 
   title='Tianic Survival by Port of Embarkation')

## ----fig.height=5, fig.width=9-------------------------------------------
# Survived & Embarked & Fare
ggplot(data=train) +
   geom_boxplot(aes(x=Embarked,y=FarePerPerson, fill=SurvivedFac)) +
   labs(x='Port of Embarkation', y='Fare (Pounds Sterling)', 
   title='Titanic Survival by Port of Embarkation and Fare')

## ----fig.height=5, fig.width=9-------------------------------------------
# Survived & Embarked & Fare
ggplot(data=train) +
   geom_boxplot(aes(x=Embarked,y=FarePerPersonLog, fill=SurvivedFac)) +
   labs(x='Port of Embarkation', y='Fare (Log of Pounds Sterling)', 
   title='Titanic Survival by Port of Embarkation and Fare')

## ----fig.height=4, fig.width=7, message=FALSE----------------------------
# 3 Survived and Pclass 
ggplot(data=train) +
   geom_mosaic(aes(x=product(SurvivedFac, PclassFac),fill=SurvivedFac)) +
   labs(x='Passenger Class', y='', title='Titanic Survival by Passenger Class')

## ------------------------------------------------------------------------
cor(train$SurvivedNum,train$PclassNum)

## ----fig.height=4.5, fig.width=8-----------------------------------------
# 4 Survived and NameLength
plot(train$SurvivedNum~train$NameLength, pch=19, col=rgb(0,0,.6,.2),
    main="Titanic Survival by Name Length",
    ylab="Probability of Survival", xlab="Name Length (chars)")
linmod=lm(SurvivedNum~NameLength,data=train)
abline(linmod, col="green", lwd=1, lty=2)
g=glm(SurvivedNum~NameLength,family='binomial',data=train)
curve(predict(g,data.frame(NameLength=x),type="resp"),col="red",lty=2,lwd=2,add=TRUE) 
legend(60,0.5,c("linear fit","logistic fit"), col=c("green","red"), lty=c(1,2))

## ------------------------------------------------------------------------
cor(train$SurvivedNum,train$NameLength)

## ----fig.height=5, fig.width=9-------------------------------------------
# 5 Survival and Title
ggplot(data=train) +
   geom_mosaic(aes(x=product(SurvivedFac, Title),fill=SurvivedFac)) + 
   labs(x='Title', y='',
   title='Tianic Survival by Title') + 
   theme(axis.text.x = element_text(angle = 90))

## ----fig.height=4, fig.width=6-------------------------------------------
# 6 Survived & Gender
ggplot(data=train) +
   geom_mosaic(aes(x=product(SurvivedFac, GenderFac),fill=SurvivedFac)) +
   labs(x='Sex', y='', title='Titanic Survival by Gender')

## ------------------------------------------------------------------------
cor(train$SurvivedNum,train$IsMale)

## ----fig.height=5, fig.width=8.5-----------------------------------------
# 7 Survived and Sibling/Spouse
ggplot(data=train) +
   geom_mosaic(aes(x=product(SurvivedFac, SiblingSpouse),fill=SurvivedFac)) +
   labs(x='Number of Siblings/Spouses', y='', 
   title='Titanic Survival by Number of Siblings or Spouses')

## ----fig.height=5, fig.width=8.5-----------------------------------------
# 8 Survived and Parent/Children
ggplot(data=train) +
   geom_mosaic(aes(x=product(SurvivedFac, ParentChildren),fill=SurvivedFac)) +
   labs(x='Number of Parents/Children', y='', 
   title='Titanic Survival by Number of Parents or Children')

## ----fig.height=5, fig.width=8.5-----------------------------------------
# Survived and Number of Relatives
ggplot(data=train) +
   geom_mosaic(aes(x=product(SurvivedFac, NumRelatives),fill=SurvivedFac)) +
   labs(x='Number of Parents/Children', y='', 
   title='Titanic Survival by Number of Relatives')

## ------------------------------------------------------------------------
cor(train$SurvivedNum,as.numeric(train$NumRelatives))

## ----fig.height=5, fig.width=9-------------------------------------------
# 9 Survived and Ticket Counts
ticketcounts <- factor(train$TicketCount)
ggplot(data=train) +
   geom_mosaic(aes(x=product(SurvivedFac, ticketcounts),fill=SurvivedFac)) +
   labs(x='Number of People Per Ticket', y='', 
   title='Titanic Survival by Size of Groups Per Ticket')

## ------------------------------------------------------------------------
cor(train$SurvivedNum,train$TicketCount)

## ----fig.height=5, fig.width=8.5-----------------------------------------
# 10 Survived & Fare
plot(train$SurvivedNum~train$FarePerPerson, pch=19, col=rgb(0,0,.6,.2),
    main="Titanic Survival by Fare",ylab="Probability of Survival", xlab="Fare (Pounds Sterling)")
linmod=lm(SurvivedNum~FarePerPerson,data=train)
abline(linmod, col="green", lwd=1, lty=2)
g=glm(SurvivedNum~FarePerPerson,family='binomial',data=train)
curve(predict(g,data.frame(FarePerPerson=x),type="resp"),col="red",lty=2,lwd=2,add=TRUE) 
legend(120,0.7,c("linear fit","logistic fit"), col=c("green","red"), lty=c(1,2))

## ----fig.height=5, fig.width=8.5-----------------------------------------
# 10 Survived & Log(Fare)
plot(train$SurvivedNum~train$FarePerPersonLog, pch=19, col=rgb(0,0,.6,.2),
    main="Titanic Survival by Log of Fare",
    ylab="Probability of Survival", xlab="Fare in Log(Pounds Sterling)")
linmod=lm(SurvivedNum~FarePerPersonLog,data=train)
abline(linmod, col="green", lwd=1, lty=2)
g=glm(SurvivedNum~FarePerPersonLog,family='binomial',data=train)
curve(predict(g,data.frame(FarePerPersonLog=x),type="resp"),col="red",lty=2,lwd=2,add=TRUE) 
legend(1,0.7,c("linear fit","logistic fit"), col=c("green","red"), lty=c(1,2))

## ------------------------------------------------------------------------
cor(train$SurvivedNum,train$FarePerPerson)
cor(train$SurvivedNum,train$FarePerPersonLog)

## ----fig.height=5, fig.width=8.5-----------------------------------------
# 11 Survived & Age
plot(train$SurvivedNum~train$AgeNum, pch=19, col=rgb(0,0,.6,.2),
    main="Titanic Survival by Age",ylab="Probability of Survival", xlab="Age")
linmod=lm(SurvivedNum~AgeNum,data=train)
abline(linmod, col="green", lwd=2, lty=2)
g=glm(SurvivedNum~AgeNum,family='binomial',data=train)
curve(predict(g,data.frame(AgeNum=x),type="resp"),col="red",lty=2,lwd=2,add=TRUE) 
legend(60,0.7,c("linear fit","logistic fit"), col=c("green","red"), lty=c(1,2))

## ------------------------------------------------------------------------
cor(train$SurvivedNum,train$AgeNum)

## ----fig.height=5, fig.width=8.5-----------------------------------------
# 11 Survived & Age as Factor
ggplot(data=train) +
   geom_mosaic(aes(x=product(SurvivedFac, AgeFac),fill=SurvivedFac)) +
   labs(x='Age Group', y='', title='Titanic Survival by Age Group')

## ------------------------------------------------------------------------
# Combinations of 3 or more variables quickly explode
vars <- 1:12
for (i in 2:10) {
	num <- length(combn(vars,i))/i
	print(paste("There are ", num, "combinations of 12 variables taken", i, "at a time."))
}

## ------------------------------------------------------------------------
# creating IsSingle variable
train$IsSingle <- ifelse(train$NumRelatives==0,1,0)
train$IsSingle <- factor(train$IsSingle, levels=0:1, labels=c("Not Single","Single"))

## ----fig.height=5, fig.width=9-------------------------------------------
ggplot(data=train) +
   geom_mosaic(aes(x=product(GenderFac, IsSingle),fill=SurvivedFac)) +
   labs(x='Single or Not, Survived or Not', y='Gender', 
   title='Titanic Survival by Gender and Single Status')

## ----fig.height=5, fig.width=9-------------------------------------------
ggplot(data=train) +
   geom_bar(aes(x=SurvivedFac,y=IsMale,fill=AgeFac), stat="identity") +
   facet_wrap(aes(IsSingle))+
           coord_flip() +
   labs(x='Survived', y='Count', 
   title='Titanic Survival of Men by Age and Single Status')

## ----fig.height=5, fig.width=9-------------------------------------------
# Women by Age and Single Status
ggplot(data=train) +
   geom_bar(aes(x=SurvivedFac,y=-(IsMale-1),fill=AgeFac), stat="identity") +
   facet_wrap(aes(IsSingle))+
           coord_flip() +
   labs(x='Survived', y='Count', 
   title='Titanic Survival of Women by Age and Single Status')

## ------------------------------------------------------------------------
chosen <- c("PclassFac","GenderFac","NumRelatives","TicketCount","AgeNum","SurvivedFac")
train[,colnames(train) %in% chosen][train$AgeFac=="Child" & train$NumRelatives == 0,]

## ----fig.height=4, fig.width=9-------------------------------------------
# Survival of 1st Class Passengers by Gender and Age Group
ggplot(data=train[train$PclassNum==1,]) +
   geom_bar(aes(x=AgeFac,y=-(SurvivedNum-2), fill=SurvivedFac), stat="identity") +
   facet_wrap(aes(GenderFac)) +
   labs(x='', y='', 
   title='Survival of 1st Class Passengers by Gender and Age Group') +
   coord_flip()

## ----fig.height=4, fig.width=9-------------------------------------------
# Survival of 2nd Class Passengers by Gender and Age Group
ggplot(data=train[train$PclassNum==2,]) +
   geom_bar(aes(x=AgeFac,y=-(SurvivedNum-2), fill=SurvivedFac), stat="identity") +
   facet_wrap(aes(GenderFac))+
   labs(x='', y='', 
   title='Survival of 2nd Class Passengers by Gender and Age Group') +
   coord_flip()

## ----fig.height=4, fig.width=9-------------------------------------------
# Survival of 3rd Class Passengers by Gender and Age Group
ggplot(data=train[train$PclassNum==3,]) +
   geom_bar(aes(x=AgeFac,y=-(SurvivedNum-2), fill=SurvivedFac), stat="identity") +
   facet_wrap(aes(GenderFac))+
   labs(x='', y='', 
   title='Survival of 3rd Class Passengers by Gender and Age Group') +
   coord_flip()

## ----include=FALSE-------------------------------------------------------
# uncomment to run, creates Rcode file with R code
#library(knitr)
#purl("Titanic_Survival.Rmd", output = "Rcode.R")

