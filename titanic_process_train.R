
# load training set 
rm(list=ls()) 
train <- read.csv("../input/train.csv", na.strings="", stringsAsFactors=FALSE)

# Title 
train$Title <- vector("character",length=nrow(train))
for (i in 1:nrow(train)) {
	x <- as.character(train$Name[i])
	m <- regexec(",(\\s+\\w+)+\\.", x) 
	train$Title[i] <- unlist(strsplit(unlist(regmatches(x,m))," "))[2]
}

common_titles <- c("Mr.", "Mrs.", "Miss.")
rare_male <- c("Don.","Rev.","Dr.","Major.","Master.", "Sir.","Col.","Capt.","Jonkheer.")
for (i in 1:nrow(train)) {
	train$Title[i] <- ifelse(train$Title[i] %in% common_titles, train$Title[i], # do not replace
	                  ifelse(train$Title[i] %in% rare_male, "rareMale", "rareFemale"))
}
train$Title <- factor(train$Title)
train$TitleMr <- ifelse(train$Title == 'Mr.', 1, 0)
train$TitleMissMrs <- ifelse(train$Title == 'Miss.' | train$Title == 'Mrs.', 1, 0)
train$TitlerareFemale <- ifelse(train$Title == 'rareFemale', 1, 0)

# NameLength
train$NameLength <- vector("numeric", nrow(train))
for (i in 1:nrow(train)) {
	train$NameLength[i] <- nchar(as.character(train$Name)[i])
}

train$NameLength <- round(train$NameLength/max(train$NameLength),2)


# Sex 
train$IsMale <- ifelse(train$Sex=="male",1,0) # indicator for ML

# NumRelatives
train$NumRelatives <- train$SibSp + train$Parch

train$NumRelatives0 <- ifelse(train$NumRelatives == 0, 1, 0)
train$NumRelatives123 <- ifelse(train$NumRelatives > 0 & train$NumRelatives < 4, 1, 0)

# Embarked
train$Embarked[is.na(train$Embarked)] <- 'S'
train$EmbarkedS <- ifelse(train$Embarked == 'S', 1, 0)
train$EmbarkedC <- ifelse(train$Embarked == 'C', 1, 0)


# Pclass
train$Pclass1 <- ifelse(train$Pclass == 1, 1, 0)
train$Pclass2 <- ifelse(train$Pclass == 2, 1, 0)


# FarePerPerson
counts <- aggregate(train$Ticket, by=list(train$Ticket), 
                    FUN=function(ticket) sum(!is.na(ticket)))

divide_fare_count <- function(dfm) {
  fare <- as.numeric(dfm["Fare"])
  # ticket counts
  count_given_ticket <- counts[which(counts[,1] == dfm["Ticket"]), 2]
  result <- round(fare/count_given_ticket,2)
  return(result)
}
train$FarePerPerson <- apply(X=train, MARGIN=1, FUN=divide_fare_count)
train$FarePerPerson[is.na(train$FarePerPerson)] <- mean(train$FarePerPerson, na.rm=TRUE)
train$FarePerPerson <- round(train$FarePerPerson/max(train$FarePerPerson),2)


# TicketCount
train$TicketCount <- apply(X=train, MARGIN=1, 
                          FUN=function(dfm) counts[which(counts[,1] == dfm["Ticket"]), 2])
						  
train$TicketCount1 <- ifelse(train$TicketCount == 1, 1, 0)
train$TicketCount234 <- ifelse(train$TicketCount > 1 & train$TicketCount < 5, 1, 0)


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

train$CabinACF <- ifelse(train$CabinClean == 'A' | 
						 train$CabinClean == 'C' |
						 train$CabinClean == 'F', 1, 0)

train$CabinACF[is.na(train$CabinACF)] <- 0

train$CabinBDE <- ifelse(train$CabinClean == 'B' | 
						 train$CabinClean == 'D' |
						 train$CabinClean == 'D', 1, 0)

train$CabinBDE[is.na(train$CabinBDE)] <- 0


# Age 
train$isMinor <- ifelse(train$Age > 0 & train$Age < 14, 1, 0)
train$isMinor[is.na(train$isMinor)] <- 0

train$isElder <- ifelse(train$Age > 64, 1, 0)
train$isElder[is.na(train$isElder)] <- 0


# drop unused 
'%ni%' <- Negate('%in%')
not_chosen <- c("PassengerId","Name","Title","Sex","Fare","Ticket","TicketCount","Pclass",
				"Cabin","CabinClean","Embarked","SibSp","Parch","Age","NumRelatives")

train <- train[,colnames(train) %ni% not_chosen]


write.csv(train, "../input/train_processed.csv", row.names=FALSE)



# test keeping vs not keeping features like Namelength, Cabin, Embarked, isMale vs Title 
# specially engineered: SingleFem, SingleMale, Not1stClass?




