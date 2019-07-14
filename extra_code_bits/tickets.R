# Var9 = Ticket
# Ticket: 681 levels! ... use ReGex
# grab indefinite digits from right, stop at first space 
# Either drop or use the letters 
train$Ticket <- as.character(train$Ticket) 

# removing ending digits 
train$TicketClean <- vector("character", nrow(train))
for (i in 1:nrow(train)) {
	pattern <- "[0-9]+$"
	m <- regexec(pattern, train$Ticket[i])
	digits <- regmatches(train$Ticket[i], m)
	train$TicketClean[i] <- trimws(sub(digits, "", train$Ticket[i]), which="right")
}

# remove periods 
for (i in 1:nrow(train)) {
	train$TicketClean[i] <- gsub("[.]", "", train$TicketClean[i])
	
}	
# remove forward slashes
for (i in 1:nrow(train)) {
	train$TicketClean[i] <- gsub("[/]", "", train$TicketClean[i])
	
}
# remove spaces that removing previous symbols left 
for (i in 1:nrow(train)) {
	train$TicketClean[i] <- gsub("[\\s]", "", train$TicketClean[i])
	
}

for (i in 1:nrow(train)) {
train$TicketClean[i] <- ifelse(train$TicketClean[i] == "STONO2", "SOTONO2",
					 ifelse(train$TicketClean[i] == "STONO 2", "SOTONO2",
					 ifelse(train$TicketClean[i] == "SCAH Bale", "SCAH",
					 ifelse(train$TicketClean[i] == "SCPari", "SCPARIS",
					 ifelse(train$TicketClean[i] == "", "Other", train$TicketClean[i])))))
}

table(train$TicketClean)
