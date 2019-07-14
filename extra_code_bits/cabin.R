# 687 NAs, 147 levels... drop?
# Cabin places might be important
# The accident happened at 11:39pm and the ship sank around 2:00am, people could be at their cabins
# cabin A are near the deck and cabins F are near the keel (where the ship hit the iceberg)
# grab cabin letters, create dummies
unique(train$Cabin)



# removing ending digits 
train$CabinClean <- vector("character", nrow(train))
for (i in 1:nrow(train)) {
	pattern <- "[0-9]+$"
	m <- regexec(pattern, train$Cabin[i])
	digits <- regmatches(train$Cabin[i], m)
	train$CabinClean[i] <- trimws(sub(digits, "", train$Cabin[i]), which="right")
}

# recategorizing manually, F is F or higher 
for (i in 1:nrow(train)) {
train$CabinClean[i] <- ifelse(train$CabinClean[i] == "B51 B53 B", "B",
					 ifelse(train$CabinClean[i] == "B57 B59 B63 B", "B",
					 ifelse(train$CabinClean[i] == "B58 B", "B",
					 ifelse(train$CabinClean[i] == "B82 B", "B",
					 ifelse(train$CabinClean[i] == "B96 B", "B",
					 ifelse(train$CabinClean[i] == "C22 C", "C",
					 ifelse(train$CabinClean[i] == "C23 C25 C", "C",						 
					 ifelse(train$CabinClean[i] == "C62 C", "C",	
					 ifelse(train$CabinClean[i] == "D10 D", "D",
					 ifelse(train$CabinClean[i] == "F E", "F",
					 ifelse(train$CabinClean[i] == "F G", "F",
					 ifelse(train$CabinClean[i] == "G", "F",
					 ifelse(train$CabinClean[i] == "T", "F",
					 train$CabinClean[i])))))))))))))
}

# results 
table(train$CabinClean)
