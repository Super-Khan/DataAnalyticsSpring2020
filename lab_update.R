#Lab3 in February 20th (Thursday):
library(rpart)
library(rpart.plot)
library(ggplot2)
data("msleep")
str(msleep)
str(data)
mSleepDF1 <-msleep[,c(3,6,10,11)] 
str(mSleepDF1)
head(mSleepDF1)
sleepModel_1 <-rpart(sleep_total~ ., data=mSleepDF1, method = "anova")
sleepModel_1
rpart.plot(sleepModel_1, type = 3, fallen.leaves= TRUE)
rpart.plot(sleepModel_1, type = 3,digits = 3, fallen.leaves= TRUE) 
rpart.plot(sleepModel_1, type = 3,digits = 4, fallen.leaves= TRUE)

require(C50)
data("iris")
head(iris)
str(iris)
table(iris$Species)
set.seed(9850)
grn<-runif(nrow(iris))
irisrand<-iris[order(grn),]
str(irisrand)
classificationmodel1 <-C5.0(irisrand[1:100,-5], irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)
prediction1 <-predict(classificationmodel1,irisrand[101:150,])
prediction1
table(irisrand[101:150,5],prediction1)
plot(classificationmodel1)

rm(list = ls())
library("e1071")
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
classifier$aprioriclassifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")

#Lab3 in March 5th (Thursday):
rm(list = ls())
set.seed(12345)
par(mar = rep(0.2,4))
data_Matrix<-matrix(rnorm(400), nrow= 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
par(mar=rep(0.2,4))
heatmap(data_Matrix)
set.seed(678910)
for(i in 1:40){
  coin_Flip<-rbinom(1, size = 1, prob= 0.5)
  if(coin_Flip){
    data_Matrix[i, ] <-data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}
par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])
par(mar=rep(0.2, 4))
heatmap(data_Matrix)

#Titanic
rm(list = ls())
library(rpart)
library(rpart.plot)
library(ggplot2)

dim(Titanic)
Titanic
str(Titanic)
TitanicDF1 <-Titanic
str(TitanicDF1)
head(TitanicDF1)
TitanicModel_1 <-rpart(Survived~ ., data=TitanicDF1, method = "anova")
TitanicModel_1
rpart.plot(TitanicModel_1, type = 3, fallen.leaves= TRUE)
rpart.plot(TitanicModel_1, type = 3,digits = 3, fallen.leaves= TRUE) 
rpart.plot(TitanicModel_1, type = 3,digits = 4, fallen.leaves= TRUE)

rm(list = ls())
require(C50)

set.seed(9850)
Titanicdf <- as.data.frame(Titanic)
grn<-runif(nrow(Titanicdf))
Titanicrand<-Titanicdf[order(grn),]
str(Titanicrand)
classificationmodel1 <-C5.0(Titanicrand[1:30,-4], Titanicrand[1:30,4])
classificationmodel1
summary(classificationmodel1)
prediction1 <-predict(classificationmodel1,Titanicrand[31:32,])
prediction1

plot(classificationmodel1)

rm(list = ls())
library("e1071")

names(Titanic)
Titanicdf <- as.data.frame(Titanic)
Titanicdf
dim(Titanicdf$Survived)
classifier<-naiveBayes(Titanicdf[,1:3], Titanicdf[,4])
table(predict(classifier, Titanicdf[,-4]), Titanicdf[,4], dnn=list('predicted','actual'))

plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")

library(randomForest)
data1 <- as.data.frame(Titanic)
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
trainset <- data1[train,]
validset <- data1[-train,]
model1 <- randomForest(Survived~., data = trainset, importance = TRUE)
model1
model2 <- randomForest(Survived~., data = trainset, ntree = 500, mtry = 6 ,importance = TRUE)
model2
predtrain <- predict(model2, trainset, type = "class")
table(predtrain, trainset$Survived)
predvalid <- predict(model2, validset, type = "class")
table(predvalid, validset$Survived)


#Lab3 in March 26th (Thursday):
rm(list = ls())
wine_data<-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep= ",")
head(wine_data)
nrow(wine_data)
dim(wine_data)
colnames(wine_data) <-c("Cvs", "Alcohol", "Malic_Acid", "Ash", "Alkalinity_of_Ash", "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols","Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", "Proline")
head(wine_data) 
heatmap(cor(wine_data),Rowv= NA, Colv= NA)
cultivar_classes<-factor(wine_data$Cvs)
cultivar_classes
wine_data_PCA<-prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)

#Lab4 in April 02 (Thursday):
library(e1071)
set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
x
y
plot(x, col=(3-y))
dat<-data.frame(x = x,y= as.factor(y))
svmfit<-svm(y ~., data=dat, kernel="linear", cost=10,scale=FALSE)
plot(svmfit, dat)
summary(svmfit)
svmfit<-svm(y ~., data=dat, kernel="linear", cost = 0.1, scale=FALSE)
plot(svmfit, dat)
svmfit$index

set.seed(1)
tune.out<-tune(svm, y ~.,data=dat,kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred<-predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)
svmfit<-svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred, truth=testdat$y)
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)
dat=data.frame(x=x,y=as.factor(y))
svmfit<-svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit,dat)
svmfit<-svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat)
library(e1071)
library(ISLR)
names(Khan)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)
dat<-data.frame(x=Khan$xtrain, y = as.factor(Khan$ytrain))
out <-svm(y ~., data=dat, kernel="linear",cost=10)
summary(out)
dat.te=data.frame(x=Khan$xtest, y = as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

#Lab5 in April 9th (Thursday):
n <- 150 
p <- 2 
sigma <- 1 
meanpos <- 0 
meanneg <- 3 
npos <- round(n/2) 
nneg <- n-npos 

xpos <- matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg <- matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),npos,p)
x <- rbind(xpos,xneg)

y <- matrix(c(rep(1,npos),rep(-1,nneg)))

plot(x,col=ifelse(y>0,1,2))
legend("topleft",c('Positive','Negative'),col=seq(2),pch=1,text.col=seq(2))

ntrain <- round(n*0.8)
tindex <- sample(n,ntrain) 
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]
istrain=rep(0,n)
istrain[tindex]=1

plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend("topleft",c('Positive Train','Positive Test','Negative Train','Negative Test'),col=c(1,1,2,2), pch=c(1,2,1,2), text.col=c(1,1,2,2))

data(iris)
attach(iris)


model <- svm(Species ~ ., data = iris)


x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 

print(model)
summary(model)


pred <- predict(model, x)

pred <- fitted(model)


table(pred, y)


pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]


plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])


x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)


m   <- svm(x, y)
new <- predict(m, x)


plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)


X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)


m <- svm(X, gamma = 0.1)


m <- svm(~., data = X, gamma = 0.1)

m <- svm(~ a + b, gamma = 0.1)


newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)


plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)


i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- svm(Species ~ ., data = i2, class.weights = wts)
m

#Lab in April 16th (Thursday):
library(e1071) 
m1 <- matrix( c(0,0,0,1,1,2,1,2,3,2,3,3,0,1,2,3,0,1,2,3,1,2,3,2,3,3,0,0,0,1,1,2,4,4,4,4,0,1,2,3,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,-1,-1), ncol = 3 ) 

Y = m1[,3] 
X = m1[,1:2] 

df = data.frame( X , Y ) 

par(mfcol=c(4,2)) 
for( cost in c( 1e-3 ,1e-2 ,1e-1, 1e0,  1e+1, 1e+2 ,1e+3)) { 

  model.svm <- svm( Y ~ . , data = df ,  type = "C-classification" , kernel = 
                      "linear", cost = cost, 
                    scale =FALSE ) 
 
  
  plot(x=0,ylim=c(0,5), xlim=c(0,3),main= paste( "cost: ",cost, "#SV: ", 
                                                 nrow(model.svm$SV) )) 
  points(m1[m1[,3]>0,1], m1[m1[,3]>0,2], pch=3, col="green") 
  points(m1[m1[,3]<0,1], m1[m1[,3]<0,2], pch=4, col="blue") 
  points(model.svm$SV[,1],model.svm$SV[,2], pch=18 , col = "red") 
}

library(kernlab)
data(spam)
index <- sample(1:dim(spam)[1])
spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]
filter <- ksvm(type~.,data=spamtrain,kernel="rbfdot",
               kpar=list(sigma=0.05),C=5,cross=3)
filter
mailtype <- predict(filter,spamtest[,-58])
table(mailtype,spamtest[,58])

data(iris)
rbf <- rbfdot(sigma=0.1)
rbf
irismodel <- ksvm(Species~.,data=iris,type="C-bsvc",
                  kernel=rbf,C=10,prob.model=TRUE)
irismodel
fitted(irismodel)
predict(irismodel, iris[,-5], type="probabilities")
