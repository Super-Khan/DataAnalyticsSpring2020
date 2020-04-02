rm(list = ls())
wine_data<-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep= ",")
head(wine_data)
nrow(wine_data) 
dim(wine_data)
colnames(wine_data) <-c("Cvs", "Alcohol", "Malic_Acid", "Ash", "Alkalinity_of_Ash", "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols","Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", "Proline")
heatmap(cor(wine_data),Rowv= NA, Colv= NA) 
cultivar_classes<-factor(wine_data$Cvs) 
cultivar_classes
wine_data_PCA<-prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)

rm(list = ls())
data("iris")
library(ggplot2)
library(e1071)
qplot(Petal.Length, Petal.Width, data=iris, color = Species)
svm_model1 <-svm(Species~., data = iris)
summary(svm_model1)
plot(svm_model1, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width= 3, Sepal.Length= 4))
pred1 <-predict(svm_model1, iris)
table1 <-table(Predicted = pred1, Actual = iris$Species)
table1
table1 <-table(Predicted = pred1, Actual = iris$Species)
table1
Model1_accuracyRate = sum(diag(table1))/sum(table1)
Model1_accuracyRate
Model1_MissClassificationRate = 1 -Model1_accuracyRate
Model1_MissClassificationRate
svm_model2 <-svm(Species~., data = iris, kernel = "linear")
summary(svm_model2)
plot(svm_model2, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width= 3, Sepal.Length= 4))
pred2 <-predict(svm_model2, iris)
table2 <-table(Predicted = pred2, Actual = iris$Species)
table2
table2 <-table(Predicted = pred2, Actual = iris$Species)
table2
Model2_accuracyRate = sum(diag(table2))/sum(table2)
Model2_accuracyRate
Model2_MissClassificationRate = 1 -Model2_accuracyRate
Model2_MissClassificationRate