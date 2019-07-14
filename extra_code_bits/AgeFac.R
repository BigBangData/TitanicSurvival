
Before moving on with the imputation am creating an `AgeFac` variable that bins ages into the following groups: `Child` ($0-12$) `Teen` ($13-19$), `YoungAdult` ($20-35$), `MiddleAged` ($36-55$), and `Elderly` ($56-80$). 


```{r fig.height=4.5, fig.width=9.5, echo=FALSE}
Child <- sum(ifelse(train$Age > 0 & train$Age < 13,1,0),na.rm=TRUE)
Teen <-  sum(ifelse(train$Age > 12 & train$Age < 20,1,0),na.rm=TRUE)
YoungAdult <- sum(ifelse(train$Age > 19 & train$Age < 36,1,0),na.rm=TRUE)
MiddleAged <- sum(ifelse(train$Age > 35 & train$Age < 56,1,0),na.rm=TRUE)
Elderly <- sum(ifelse(train$Age > 55 & train$Age < 100,1,0),na.rm=TRUE)
discrete_age_vec1 <- c(Child,Teen,YoungAdult,MiddleAged,Elderly)
par(mfrow=c(1,2))
barplot(discrete_age_vec1, col=terrain.colors(5), ylim=c(0,515),
        cex.names=.7, cex.main=0.8, cex.axis=0.8, las=2,
        main="Age Distribution with Missing Values",  space=c(0,0,0,0,0),
        names.arg=c("Child","Teen","Young Adult","Middle Aged","Elderly"))
# adding all 177 NAs to the Young Adult category
YoungAdult <- YoungAdult + sum(is.na(train$Age))
discrete_age_vec2 <- c(Child,Teen,YoungAdult,MiddleAged,Elderly)
barplot(discrete_age_vec2, col=terrain.colors(5), ylim=c(0,515),
        cex.names=.7, cex.main=0.8, cex.axis=0.8, las=2,
        main="Age Distribution with Imputed Averages", space=c(0,0,0,0,0),
        names.arg=c("Child","Teen","Young Adult","Middle Aged","Elderly"))
```
