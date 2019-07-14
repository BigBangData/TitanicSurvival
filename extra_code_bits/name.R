# Var4 = Name 
# Name: create Title variable
train$Title <- vector("character",length=nrow(train))
for (i in 1:nrow(train)) {
	x <- as.character(train$Name[i])
	m <- regexec(",(\\s+\\w+)+\\.", x) 
	train$Title[i] <- unlist(strsplit(unlist(regmatches(x,m))," "))[2]
}

train$Title[train$Title == "the"] <- "the Countess"
unique(train$Title)
# still too many, bin rare titles?

Survival <- ifelse(train$Survived==1,"yes","no")
TitleFac <- factor(train$Title)
ggplot(data=train) +
   geom_mosaic(aes(x=product(Survival, TitleFac),fill=Survival))
   
# remove Mr. Mrs. Miss. and inspect 
'%ni%' <- Negate('%in%')
common <- c("Mr.","Mrs.","Miss.")


train_uncommon <- train[train$Title %ni% common,]
Survival <- ifelse(train_uncommon$Survived==1,"yes","no")
TitleFac <- factor(train_uncommon$Title)
ggplot(data=train_uncommon) +
   geom_mosaic(aes(x=product(Survival, TitleFac),fill=Survival))

unique(train_uncommon$Titles)
# Are all Doctors men?
train_uncommon$Name[train_uncommon$Title == "Dr."]
train[train$Name == "Leader, Dr. Alice (Farnham)",] # female doctor survived 
train[train$Title == "Dr." & train$Name != "Leader, Dr. Alice (Farnham)",] # 2/6 male Dr. survived 


rareMale <- c("Don.","Rev.","Dr.","Major.","Master.", "Sir.","Col.","Capt.","Jonkheer.")
rareFemale <- c("Mme.","Ms.","Lady.","Mlle.","the Countess")
for (i in 1:nrow(train)) {
	train$Title[i] <- ifelse(train$Title[i] %in% rareMale, "rareMale",
					  ifelse(train$Title[i] %in% rareFemale, "rareFemale", train$Title[i]))
}

# dropping Name 
train$Name <- NULL 