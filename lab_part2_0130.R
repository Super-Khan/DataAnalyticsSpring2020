data1 <- read.csv(file.choose(), header = T)
data1
# install.packages("xlsx")
# library(xlsx)
EPI_1 <- read.csv(file.choose(), header = T, skip = 1)
EPI_1$EPI
# code <- c(EPI$code)
# iso <-c(EPI$ISO3V10)
# EPI.fram <- data.frame(code, iso)
# EPI.fram
EPI_1
attach(EPI_1)
fix(EPI_1)
tf <- is.na(EPI_1$EPI)
E <- EPI_1$EPI[!tf]
E
summary(E)
fivenum(E, na.rm = T)
stem(E)
hist(E, seq(30., 95., 1.0), prob = T)
lines(density(E, na.rm = T, bw = 1.)) 
rug(E) 
plot(ecdf(E), do.points = F, verticals = T)
par(pty = "s")
qqnorm(E); qqline(E)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab= "Q-Q plot for t dsn")
qqline(x)

plot(ecdf(EPI_1$EPI), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(EPI_1$EPI); qqline(EPI_1$EPI)

x <-seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab= "Q-Q plot")
qqline(x)

boxplot(EPI_1$EPI,EPI_1$DALY)
qqplot(EPI_1$EPI, EPI_1$DALY)

multivariate <- read.csv(file.choose(), header = T)
attach(multivariate)
mm <- lm(Homeowners~Immigrant)
mm
summary(mm)
plot(Homeowners~Immigrant)
abline(mm)
abline(mm,col=2,lwd=3)

abline(mm,col=3,lwd=3)
attributes(mm)

plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data = mtcars)
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "blue")
qplot(pressure$temperature, pressure$pressure/2, geom = "line")
qplot(temperature, pressure, data = pressure, geom = "line")
ggplot(pressure, aes(x = temperature, y = pressure)) + geom_point()

barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x = factor(cyl))) + geom_bar()

hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 5)

plot(ToothGrowth$len, ToothGrowth$supp)
boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x = supp, y = len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x = interaction(supp, dose), y = len)) + geom_boxplot()
