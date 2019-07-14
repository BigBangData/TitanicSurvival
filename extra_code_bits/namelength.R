# use length of name? 
# Are longer names from richer people?
train$NameLength <- vector("numeric", nrow(train))
for (i in 1:nrow(train)) {
	train$NameLength[i] <- nchar(as.character(train$Name)[i])
}

plot(train$NameLength, train$Fare, 
	pch=19, col=rgb(0,0,1,alpha=0.2),
	xlab="Name Length (chars)", 
	ylab="Fare (Pounds)")
abline(lm(train$Fare ~ train$NameLength), col="red")
