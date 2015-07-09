library(ggplot2)
library(rpart) #random forest
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

head(train)
names(train)
summary(train)
str(train) #structure of data
summary(train$Sex)
table(train$Survived,train$Sex)
?prop.table 
prop.table(table(train$Survived,train$Sex)) #proportional table
View(train)

train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x){sum(x)/length(x)})
?aggregate()
summary(train$Age)
summary(train$Fare)
table(train$Fare)
ggplot(train,aes(x=Fare))+geom_histogram(bin=10)

train$Fare2 <- '50+'
train$Fare2[train$Fare < 50 & train$Fare >= 20] <- '20-50'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

table(train$Fare2)

mosaicplot(train$Fare2 ~ train$Survived, main="Passenger Survival by Fare",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)

dev.off()

aggregate(Survived ~ Fare2 + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

ggplot(train,aes(x=Pclass,y=Fare))+geom_point()



ggplot(train,aes(x=Age,y=Survived,color=Sex))+geom_point()
ggplot(train,aes(x=Fare,y=Survived,color=Sex))+geom_point()
ggplot(train,aes(x=Pclass,y=Survived,color=Sex))+geom_point()

?mosaicplot
mosaicplot(data=train[train$Sex=="female",],Pclass ~ Survived, main="Passenger Survival by Class",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)

dev.off()
mosaicplot(train$Sex ~ train$Survived, main="Passenger Survival by Gender",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)
dev.off()



#random forest
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
fit
summary(fit)
plot(fit)
text(fit)
fancyRpartPlot(fit)


#predict
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

