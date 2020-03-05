rm(list=ls())
library(gdata) 
#faster xls reader but requires perl!
library(readxl)
#bronx1<-read.xls(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,perl="c/perl/bin/perl.exe") 
bronx1<-read_excel(file.choose(),sheet=1, skip = 4)
bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]

#alternate
#library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
#bronx1<-read.xlsx("<SOMEWHERE>/rollingsales_bronx.xls",pattern="BOROUGH",stringsAsFactors=FALSE,sheetIndex=1,startRow=5,header=TRUE)
View(bronx1)
#
attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
SALE.PRICE<-sub("\\$","",SALE.PRICE) 
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE)) 
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET)) 
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 
m1<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2

m2<-lm(log(bronx1$SALE.PRICE)~log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#


library(ISLR)
set.seed(1)
train = sample(392,196)
lm.fit<-lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2 <-lm(mpg~poly(horsepower,2), data = Auto, subset = train) 
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3 <-lm(mpg~poly(horsepower,3), data = Auto, subset = train) 
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
set.seed(2)
train = sample(392,196)
lm.fit<-lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2 <-lm(mpg~poly(horsepower,2), data = Auto, subset = train)  
mean((mpg-predict(lm.fit2,Auto))[-train]^2) 
lm.fit3 <-lm(mpg~poly(horsepower,3), data = Auto, subset = train) 
mean((mpg-predict(lm.fit3,Auto))[-train]^2)




abalone <-read.csv(file.choose(), header = T)
colnames(abalone) <-c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' )
summary(abalone)
str(abalone)
summary(abalone$rings)
abalone$rings<-as.numeric(abalone$rings)
abalone$rings<-cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings<-as.factor(abalone$rings)
summary(abalone$rings)
z <-abalone
aba <-abalone
aba$sex<-NULL

normalize <-function(x) {
  return ((x -min(x)) / (max(x) -min(x)))
}
aba[1:7] <-as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)
ind<-sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain<-aba[ind==1,]
KNNtest<-aba[ind==2,]
sqrt(2918)
library(class)
help("knn") 
KNNpred<-knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)

library(ggplot2)
head(iris)
sapply(iris[,-5], var)
summary(iris)
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()
set.seed(300)
k.max<-12
wss<-sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart= 20,iter.max = 20)$tot.withinss})
wss
plot(1:k.max,wss, type= "b", xlab= "Number of clusters(k)", ylab= "Within cluster sum of squares")
icluster<-kmeans(iris[,3:4],3,nstart = 20)
table(icluster$cluster,iris$Species)