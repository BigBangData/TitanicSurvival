```{r fig.height=4, fig.width=9}
# Survival of Children by Class and Gender
ggplot(data=train[train$AgeFac=="Child",]) +
   geom_bar(aes(x=SurvivedFac,y=PclassNum, fill=GenderFac), stat="identity") +
   facet_wrap(aes(PclassNum))+
   labs(x='Survived', y='', 
   title='Survival of Children by Class and Gender')
```


```{r fig.height=4, fig.width=9}
# Survival of Teens by Class and Gender
ggplot(data=train[train$AgeFac=="Teen",]) +
   geom_bar(aes(x=SurvivedFac,y=PclassNum, fill=GenderFac), stat="identity") +
   facet_wrap(aes(PclassNum))+
   labs(x='Survived', y='', 
   title='Survival of Teens by Class and Gender')
```

```{r fig.height=4, fig.width=9}
# Survival of Young Adults by Class and Gender
ggplot(data=train[train$AgeFac=="YoungAdult",]) +
   geom_bar(aes(x=SurvivedFac,y=PclassNum, fill=GenderFac), stat="identity") +
   facet_wrap(aes(PclassNum))+
   labs(x='Survived', y='', 
   title='Survival of Young Adults by Class and Gender')
```

```{r fig.height=4, fig.width=9}
# Survival of Middle Aged Adults by Class and Gender
ggplot(data=train[train$AgeFac=="MiddleAged",]) +
   geom_bar(aes(x=SurvivedFac,y=PclassNum, fill=GenderFac), stat="identity") +
   facet_wrap(aes(PclassNum))+
   labs(x='Survived', y='', 
   title='Survival of Middle Aged Adults by Class and Gender')
```

```{r fig.height=4, fig.width=9}
# Survival of the Elderly by Class and Gender
ggplot(data=train[train$AgeFac=="Elderly",]) +
   geom_bar(aes(x=SurvivedFac,y=PclassNum, fill=GenderFac), stat="identity") +
   facet_wrap(aes(PclassNum))+
   labs(x='Survived', y='', 
   title='Survival of the Elderly by Class and Gender')
```