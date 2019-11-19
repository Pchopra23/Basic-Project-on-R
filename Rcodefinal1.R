#SETTING THE WORKING DIRECTORY
setwd("C:/Users/pooja/Desktop/HU Semester 1/502/CP")


## Upload dataset

Blackfriday = read.csv("BlackFriday.csv" , header = T)

## Library Required ##

library(dplyr)
library(ggplot2)
library(car)
library(stringr)
library(skimr)
library(Hmisc)
library(gvlma)

str(Blackfriday)

## Data Cleaning ##


BFcleandata <- Blackfriday

## Identify Null Value ##

nulls <- sapply(BFcleandata, function(x) sum(is.na(x)))
nulls[nulls > 0]

#Imputing 0 for NA in Product_Category_2

BFcleandata[is.na(BFcleandata$Product_Category_2), "Product_Category_2"] <- 0

#Imputing 0 For NA in Product_Category_3


BFcleandata[is.na(BFcleandata$Product_Category_3), "Product_Category_3"] <- 0

## Summary of clean dataset
str(BFcleandata)

## Converting Data into factor

BFcleandata$Product_Category_1 <- as.factor(BFcleandata$Product_Category_1)
#BFcleandata$Product_Category_2 <- as.factor(BFcleandata$Product_Category_2)
#BFcleandata$Product_Category_3 <- as.factor(BFcleandata$Product_Category_3)
BFcleandata$Occupation <- as.factor(BFcleandata$Occupation)
BFcleandata$User_ID <- as.factor(BFcleandata$User_ID)
BFcleandata$Marital_Status <- as.factor(BFcleandata$Marital_Status)
BFcleandata$Stay_In_Current_City_Years <- as.factor(BFcleandata$Stay_In_Current_City_Years)

##
boxplot(BFcleandata$Purchase)$out

outliers <- boxplot(BFcleandata$Purchase)$out

summary(outliers)

BFcleandata[which(BFcleandata$Purchase %in% outliers),]

BFcleandata <- BFcleandata[-which(BFcleandata$Purchase %in% outliers),]

boxplot(BFcleandata$Purchase)


summary(BFcleandata)


str(BFcleandata)

## Exploratory data analysis

## Histogram for pUrchase

hist(BFcleandata$Purchase)
boxplot(BFcleandata$Purchase)

par(mfrow=c(1,1))


## Box Plot with purchase and Gender column

ggplot(BFcleandata,aes(x=Gender, y=Purchase))+
  geom_boxplot(fill="dodgerblue4")+
  stat_summary(fun.y=mean, geom="line", aes(group=1))+
  stat_summary(fun.y=mean, geom="point", aes(group=1))+
  labs(x='Gender', y="Purchase", title="Boxplot of Purchase by gender")




## Histogram with purchase and Age column


ggplot(BFcleandata, aes(x = Purchase, fill = Age)) +geom_histogram(bins = 75) +
  facet_wrap(~ Age) +
  labs(title= "Purchases Histogram by Age") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Histogram with purchase and Occupation column

ggplot(BFcleandata, aes(x = Purchase, fill = Occupation)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Occupation) +
  labs(title= "Purchases Histogram by Occupation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Histogram with purchase and City_Category column

ggplot(BFcleandata, aes(x = Purchase, fill = City_Category)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ City_Category) +
  labs(title= "Purchases Histogram by City_Category") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Histogram with purchase and Stay_In_Current_City_Years column


ggplot(BFcleandata, aes(x = Purchase, fill = Stay_In_Current_City_Years)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Stay_In_Current_City_Years) +
  labs(title= "Purchases Histogram by Stay_In_Current_City_Years") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Histogram with purchase and Marital_Status column


ggplot(BFcleandata, aes(x = Purchase, fill = Marital_Status)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Marital_Status) +
  labs(title= "Purchases Histogram by Marital_Status") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Histogram with purchase and Product_Category_1 column


ggplot(BFcleandata, aes(x = Purchase, fill = Product_Category_1)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Product_Category_1) +
  labs(title= "Purchases Histogram by Product_Category_1") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Partitioning Data SEPARATING TRAIN AND TEST DATASETS FOR THE LINEAR MODELS



train <- BFcleandata[c(rep(TRUE,8), FALSE),] #EVERY 1ST TO 8TH OBSERVATION INTO TRAIN DATASET
test <- BFcleandata[c(rep(FALSE,8), TRUE),]  #EVERY 9TH OBSERVATION INTO TEST DATASET


### Linear regression with All Variable

Model1 <- lm(Purchase ~ Occupation+Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3 , data = train)

summary(Model1)

#R2=0.631


### Linear regression without Occupation Variable

Model2 <- lm(Purchase ~ Gender+Age+City_Category+Stay_In_Current_City_Years+Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3, data = train)
summary(Model2)

#R2=0.630

### Linear regression without Marital_Status Variable


Model3 <- lm(Purchase ~ Gender+Age+City_Category+Stay_In_Current_City_Years+Product_Category_1+Product_Category_2+Product_Category_3, data = train)

summary(Model3)

Model4 <- lm(Purchase ~ Gender+Age+City_Category+Product_Category_1+Product_Category_2+Product_Category_3, data = train)

summary(Model4)


Model5 <- lm(Purchase ~ Gender+Age+City_Category+Product_Category_1+Product_Category_3, data = train)

summary(Model5)

#R2=0.63

vif(Model5)

###As per VIF occupation VIF is very high so removed occupation ##

FinalModel4 <- lm(Purchase ~ +City_Category+Stay_In_Current_City_Years+Product_Category_1,data = train)


FinalModel4 <- lm(Purchase ~ City_Category+Product_Category_1,data = train)


vif(FinalModel4)

summary(FinalModel4)

FinalModel4$coefficients

par(mfrow=c(2,2))

plot(FinalModel4)


## Residual Analysis 

residualPlot(FinalModel4, type = "rstandard")


#Predicting THE DATA


newdata=subset(train,select = c(Purchase,Gender,Age,City_Category,Stay_In_Current_City_Years,Product_Category_1))

trainpred<-predict(FinalModel4,newdata=subset(train,select = c(Purchase,Gender,Age,City_Category,Stay_In_Current_City_Years,Product_Category_1)))

newdata1=subset(test,select = c(Purchase,Gender,Age ,City_Category,Stay_In_Current_City_Years,Product_Category_1))

testpred<-predict(FinalModel4,newdata=subset(test,select = c(Purchase,Gender,Age ,City_Category,Stay_In_Current_City_Years,Product_Category_1)))




## Confidence Interval

COnfidence_INT<-predict(FinalModel4,newdata,interval="confidence")


COnfidence_INT1<-predict(FinalModel4,newdata1,interval="confidence")

summary(COnfidence_INT)
summary(COnfidence_INT1)

#CHECK HOW GOOD IS THE MODEL IN TRAINING DATASET
train.cor<-round(cor( trainpred, train$Purchase),2)
train.RMSE<-sqrt(mean(trainpred-train$Purchase)^2)
train.MAE <- mean(abs(trainpred-train$Purchase))
c(train.cor^2,train.RMSE,train.MAE)


#CHECK HOW GOOD IS THE MODEL IN TEST DATASET
test.cor<-round(cor( testpred, test$Purchase),2)
test.RMSE<-sqrt(mean(testpred-test$Purchase)^2)
test.MAE <- mean(abs(testpred-test$Purchase))
c(test.cor^2,test.RMSE,test.MAE)

## Stepwise regression ##

stp<- step(lm(train$Purchase ~ train$Gender + train$Age + train$City_Category + train$Stay_In_Current_City_Years + train$Marital_Status + train$Product_Category_1),direction = "both")

summary(stp)


table(train$Gender,train$Age)

chisq.test(table(train$Gender,train$Age))


chisq.test(table(train$Gender,train$Age))










