rm(list = ls())
library(pROC)
library(boot)
df.ori <- read.csv(file.choose(), header = T)


library(tidyverse)
df<-read_csv('monroe-county-crash-data2003-to-2015.csv')
df<-na.omit(df)

# outliers

df%>%ggplot(aes(y=Latitude,x=Longitude))+geom_point() #remove lat<35 and lon>-50
df<-df%>%filter(Latitude>35 & Longitude<(-50))
df<-df%>%filter(Latitude < 39.3 & Latitude > 39.1 & Longitude<(-86.4) & Longitude > (-86.7))

hist(df$Year)
hist(df$Month, breaks = seq(0, 12, 1))
hist(df$Day, breaks = seq(0, 7, 1))
hist(df$Hour)

IT.num <- c(length(which(df$`Injury Type` == "No injury/unknown")), length(which(df$`Injury Type` == "Non-incapacitating")),
            length(which(df$`Injury Type` == "Incapacitating")), length(which(df$`Injury Type` == "Fatal")))
IT.name <- c("No injury/unknown", "Non-incapacitating", "Incapacitating", "Fatal")
barplot(IT.num, names.arg = IT.name)

wk.num <- c(length(which(df$`Weekend?` == "Weekday")), length(which(df$`Weekend?` == "Weekend")))
wk.name <- c("Weekday", "Weekend")
barplot(wk.num, names.arg = wk.name)

cot.num <- c(length(which(df$`Collision Type` == "1-Car")), length(which(df$`Collision Type` == "2-Car")),
            length(which(df$`Collision Type` == "3+ Cars")), length(which(df$`Collision Type` == "Bus")),
            length(which(df$`Collision Type` == "Cyclist")), length(which(df$`Collision Type` == "Moped/Motorcycle")),
              length(which(df$`Collision Type` == "Pedestrian")))
cot.name <- c("1-Car", "2-Car", "3+ Cars", "Bus", "Cyclist", "Moped/Motor", "Pedestrian")
barplot(cot.num, names.arg = cot.name)

# visualization on map
library(ggmap)

lat <- range(df$Latitude,na.rm = T)
long <- range(df$Longitude,na.rm = T)

# Define a box of interest
bbox <- make_bbox(long,lat,f=0.05)

# And then get a map of that area.
a <- get_map(bbox,maptype="toner-lite",source="stamen")
ggmap(a)

# summary distribution for traffic accidents 
ggmap(a)+geom_point(aes(y=Latitude,x=Longitude),data=df,colour='orange',alpha=0.2)

# Collision Type
ggmap(a)+geom_point(aes(y=Latitude,x=Longitude,colour=`Collision Type`),data=df,alpha=0.7)

# Injury Type
ggmap(a)+geom_point(aes(y=Latitude,x=Longitude,colour=`Injury Type`),data=df,alpha=0.7)

library(homals)
fit.mca<-homals(data.frame(df[,c('Collision Type','Injury Type','Primary Factor')]))
summary(fit.mca)
plot(fit.mca$catscores$Collision.Type,type='n')
text(fit.mca$catscores$Collision.Type,labels=rownames(fit.mca$catscores$Collision.Type),col=1,cex=0.7)
text(fit.mca$catscores$Injury.Type,labels=rownames(fit.mca$catscores$Injury.Type),col=2,cex=0.7)
text(fit.mca$catscores$Primary.Factor,labels=rownames(fit.mca$catscores$Primary.Factor),col=3,cex=0.7)

dft <- df.ori
dft <- df.ori[which(df.ori$Injury.Type == "Incapacitating"),]
ct <- dft$Collision.Type
ct.num <- c(length(which(ct == "1-Car")), length(which(ct == "2-Car")),
length(which(ct == "3+ Cars")), length(which(ct == "Bus")),
length(which(ct == "Cyclist")) + length(which(ct == "Moped/Motorcycle")) +
length(which(ct == "Pedestrian")))
ct.name <- c("1-Car", "2-Car", "3+ Cars", "Bus", "Cyclist&Moped/Motor")
dct <- data.frame(ct.name, ct.num)
dct$ct.name
barplot(dct$ct.num, names.arg = dct$ct.name, main = "Fatal Accidents")
 
df.num <- c(length(which(df.ori$Collision.Type == "1-Car")), length(which(df.ori$Collision.Type == "2-Car")),
             length(which(df.ori$Collision.Type == "3+ Cars")), length(which(df.ori$Collision.Type == "Bus")),
             length(which(df.ori$Collision.Type == "Cyclist")) + length(which(df.ori$Collision.Type == "Moped/Motorcycle")) +
             length(which(df.ori$Collision.Type == "Pedestrian")))
fatal.rate <- c(ct.num / df.num)
barplot(fatal.rate, names.arg = dct$ct.name, main = "Fatal Rate")




df1 <- df
df1[which(df1$`Injury Type` == "Fatal") , 8] <- 1
df1[which(df1$`Injury Type`  == "Incapacitating") , 8] <- 1 
df1[which(df1$`Injury Type`  != "1"), 8] <- 0
df1[which(df1$`Weekend?` == "Weekend"), 5] <- 1
df1[which(df1$`Weekend?` == "Weekday"), 5] <- 0
summary(df1$`Injury Type`)

df1$`Injury Type` 
df1$`Injury Type`  <- as.factor(df1$`Injury Type` )
str(df1)
df1$Hour <- df1$Hour / 100
df1$Hour
df1 <- data.frame(df1)
dim(df1)
s_df1<-sample(34892,10000) 
df1_train<-df1[s_df1,]
df1_test<-df1[-s_df1,]
dim(df1_test)
dim(df1_train)
model2 <- glm(Injury.Type ~ Year + Month, data = df1_train, family = binomial())
#model2 <- glm(df1$`Injury Type` ~ df1$Day + df1$Month + df1$Longitude, family = binomial())
summary(model2)
coef(model2)
pre_ <- predict(model2, df1_test)
summary(pre_)
dim(df1_test)
obs_p <- data.frame(prob = pre_, obs = df1_test$Injury.Type)
table(df1_test$Injury.Type, pre_, dnn=c("real","pre"))

roc.df <- roc(df1_test$Injury.Type, pre_)
plot(roc.df, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='ROC')
cv.error.10 = rep(0,10)
for(i in 1:10){
  cv.error.10[i] = cv.glm(df1_train, model2, K=10) $delta[1]
}
cv.error.10
mean(cv.error.10)

dfy <- df1[which(df1$Injury.Type == 1),]
hist(dfy$Year, breaks = seq(2003, 2015, 1))
hist(dfy$Month, breaks = seq(1, 12, 1))
