# EPI_data <- read.csv(file.choose(), header = T)
# EPI_data
# install.packages("xlsx")
# library(xlsx)
EPI_data <- read.csv(file.choose(), header = T, skip = 1)
EPI_data$EPI
# code <- c(EPI$code)
# iso <-c(EPI$ISO3V10)
# EPI.fram <- data.frame(code, iso)
# EPI.fram
#EPI_1
attach(EPI_data)
fix(EPI_data)
tf <- is.na(EPI_data$EPI)
E <- EPI_data$EPI[!tf]
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

tf <- is.na(EPI_data$DALY)
E <- EPI_data$DALY[!tf]
E
summary(E)
fivenum(E, na.rm = T)
stem(E)
hist(E)
hist(E, seq(0., 100., 1.0), prob = T)
lines(density(E, na.rm = T, bw = 1.)) 
rug(E) 
plot(ecdf(E), do.points = F, verticals = T)
par(pty = "s")
qqnorm(E); qqline(E)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab= "Q-Q plot for t dsn")
qqline(x)

tf <- is.na(EPI_data$WATER_H)
E <- EPI_data$WATER_H[!tf]
E
summary(E)
fivenum(E, na.rm = T)
stem(E)
hist(E)
hist(E, seq(0., 100., 1.0), prob = T)
lines(density(E, na.rm = T, bw = 1.)) 
rug(E) 
plot(ecdf(E), do.points = F, verticals = T)
par(pty = "s")
qqnorm(E); qqline(E)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab= "Q-Q plot for t dsn")
qqline(x)

boxplot(EPI_data$EPI, EPI_data$DALY)
qqplot(EPI_data$EPI, EPI_data$DALY)

boxplot(EPI_data$EPI, EPI_data$WATER_H)
qqplot(EPI_data$EPI, EPI_data$WATER_H)

EPILand<-EPI_data$EPI[!Landlock] 
ELand <-EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)
