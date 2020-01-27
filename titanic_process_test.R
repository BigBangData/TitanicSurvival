
# load testing set 
rm(list=ls()) 
test <- read.csv("../input/test.csv", na.strings="", stringsAsFactors=FALSE)

# Title 
test$Title <- vector("character",length=nrow(test))
for (i in 1:nrow(test)) {
	x <- as.character(test$Name[i])
	m <- regexec(",(\\s+\\w+)+\\.", x) 
	test$Title[i] <- unlist(strsplit(unlist(regmatches(x,m))," "))[2]
}

common_titles <- c("Mr.", "Mrs.", "Miss.")
rare_male <- c("Don.","Rev.","Dr.","Major.","Master.", "Sir.","Col.","Capt.","Jonkheer.")
for (i in 1:nrow(test)) {
	test$Title[i] <- ifelse(test$Title[i] %in% common_titles, test$Title[i], # do not replace
	                  ifelse(test$Title[i] %in% rare_male, "rareMale", "rareFemale"))
}
test$Title <- factor(test$Title)
test$TitleMr <- ifelse(test$Title == 'Mr.', 1, 0)
test$TitleMissMrs <- ifelse(test$Title == 'Miss.' | test$Title == 'Mrs.', 1, 0)
test$TitlerareFemale <- ifelse(test$Title == 'rareFemale', 1, 0)

# NameLength
test$NameLength <- vector("numeric", nrow(test))
for (i in 1:nrow(test)) {
	test$NameLength[i] <- nchar(as.character(test$Name)[i])
}

test$NameLength <- round(test$NameLength/max(test$NameLength),2)


# Sex 
test$IsMale <- ifelse(test$Sex=="male",1,0) # indicator for ML

# NumRelatives
test$NumRelatives <- test$SibSp + test$Parch

test$NumRelatives0 <- ifelse(test$NumRelatives == 0, 1, 0)
test$NumRelatives123 <- ifelse(test$NumRelatives > 0 & test$NumRelatives < 4, 1, 0)

# Embarked
test$Embarked[is.na(test$Embarked)] <- 'S'
test$EmbarkedS <- ifelse(test$Embarked == 'S', 1, 0)
test$EmbarkedC <- ifelse(test$Embarked == 'C', 1, 0)


# Pclass
test$Pclass1 <- ifelse(test$Pclass == 1, 1, 0)
test$Pclass2 <- ifelse(test$Pclass == 2, 1, 0)


# FarePerPerson
counts <- aggregate(test$Ticket, by=list(test$Ticket), 
                    FUN=function(ticket) sum(!is.na(ticket)))

divide_fare_count <- function(dfm) {

  fare <- as.numeric(dfm["Fare"])
  # ticket counts
  count_given_ticket <- counts[which(counts[,1] == dfm["Ticket"]), 2]
  result <- round(fare/count_given_ticket,2)
  return(result)
}
test$FarePerPerson <- apply(X=test, MARGIN=1, FUN=divide_fare_count)
test$FarePerPerson[is.na(test$FarePerPerson)] <- mean(test$FarePerPerson, na.rm=TRUE)
test$FarePerPerson <- round(test$FarePerPerson/max(test$FarePerPerson),2)


# TicketCount
test$TicketCount <- apply(X=test, MARGIN=1, 
                          FUN=function(dfm) counts[which(counts[,1] == dfm["Ticket"]), 2])
						  
test$TicketCount1 <- ifelse(test$TicketCount == 1, 1, 0)
test$TicketCount234 <- ifelse(test$TicketCount > 1 & test$TicketCount < 5, 1, 0)


# Cleaning up Cabin
test$CabinClean <- vector("character", nrow(test))
for (i in 1:nrow(test)) {
  # ID digits and white space
	pattern <- "[0-9]*|\\s"
  # reduce to only first letter given multiple cabins
	test$CabinClean[i] <- substr(gsub(pattern, "", test$Cabin[i]),1,1)
	# bin letters higher than F to the F category
	high_cabins <- toupper(letters[letters >"f"])
	if (test$CabinClean[i] %in% high_cabins) test$CabinClean[i] <- "F"
}

test$CabinACF <- ifelse(test$CabinClean == 'A' | 
						 test$CabinClean == 'C' |
						 test$CabinClean == 'F', 1, 0)

test$CabinACF[is.na(test$CabinACF)] <- 0

test$CabinBDE <- ifelse(test$CabinClean == 'B' | 
						 test$CabinClean == 'D' |
						 test$CabinClean == 'D', 1, 0)

test$CabinBDE[is.na(test$CabinBDE)] <- 0


# Age 
test$isMinor <- ifelse(test$Age > 0 & test$Age < 14, 1, 0)
test$isMinor[is.na(test$isMinor)] <- 0

test$isElder <- ifelse(test$Age > 64, 1, 0)
test$isElder[is.na(test$isElder)] <- 0


# drop unused 
'%ni%' <- Negate('%in%')
not_chosen <- c("PassengerId","Name","Title","Sex","Fare","Ticket","TicketCount","Pclass",
				"Cabin","CabinClean","Embarked","SibSp","Parch","Age","NumRelatives")

test <- test[,colnames(test) %ni% not_chosen]


write.csv(test, "../input/test_processed.csv", row.names=FALSE)



# test keeping vs not keeping features like Namelength, Cabin, Embarked, isMale vs Title 
# specially engineered: SingleFem, SingleMale, Not1stClass?




