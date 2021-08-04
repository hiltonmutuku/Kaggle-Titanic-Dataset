setwd("C:/Users/Admin/Desktop/PROJECTS/R Projects")
titanic.train<-read.csv(file="train.csv",stringsAsFactors = FALSE,header=TRUE)
tail(titanic.train)
titanic.test<-read.csv(file="test.csv",stringsAsFactors = FALSE,header=TRUE)
str(titanic.test)
titanic.train$IsTrainSet<-TRUE
tail(titanic.train$IsTrainSet)
titanic.test$IsTrainSet<-FALSE
titanic.test$Survived<-NA
names(titanic.test)
titanic.full<-rbind(titanic.train,titanic.test)
tail(titanic.full)
table(titanic.full$IsTrainSet)
table(titanic.full$Embarked)
titanic.full[titanic.full$Embarked=='',"Embarked"]
titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'
table(titanic.full$Embarked)
table(is.na(titanic.full$Age))
age.median<-median(titanic.full$Age,na.rm=TRUE)
titanic.full[is.na(titanic.full$Age),"Age"]<-age.median
table(is.na(titanic.full$Age))
table(is.na(titanic.full$Fare))
fare.median<-median(titanic.full$Fare,na.rm=TRUE)
titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.median
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)

table(is.na(titanic.full$Fare))
titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]
tail(titanic.train)
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]
str(titanic.full)
titanic.train$Survived<-as.factor(titanic.train$Survived)
survived.equation<-"Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked"
survived.formula<-as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)
titanic.model<-randomForest(formula=survived.formula,data=titanic.train,ntree=500,mtry=3,nodesize=0.01*nrow(titanic.test))
features.equation<-"Pclass+Sex+Age+SibSp+Parch+Fare+Embarked"
Survived<-predict(titanic.model,newdata=titanic.test)
