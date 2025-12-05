install.packages("devtools")
install.packages("randomForest")
install.packages("caret")

# 2. Load devtools
library(devtools)
# 3. Install reprtree from GitHub
install_github("araastat/reprtree")
library(randomForest)
library(caret) 
library(reprtree)

data(iris)
View(iris)

# predict method for random forest objects
set.seed(111)
Data_set_Div <- sample(2, nrow(iris), replace = TRUE, prob=c(0.8, 0.2))
Data_set_Div


iris.rf <- randomForest(Species ~ ., data=iris[Data_set_Div == 1,])
iris.rf

reprtree:::plot.getTree(iris.rf, k=45)

iris.pred <- predict(iris.rf, iris[Data_set_Div == 2,])
iris.pred <- factor(iris.pred, levels = levels(iris$Species))
confusionMatrix(
  data = iris.pred, 
  reference = iris[Data_set_Div == 2, "Species"] 
)


## Get prediction for all trees.
predict(iris.rf, iris[Data_set_Div == 2,], predict.all=TRUE)

#Predict data that RF has not seen 
new_flowers <- data.frame(
  Sepal.Length = c(6.0, 4.8, 5.8),  
  Sepal.Width  = c(3.0, 3.2, 2.7),  
  Petal.Length = c(5.2, 1.4, 4.1),  
  Petal.Width  = c(1.8, 0.2, 1.3)   
)

prediction_NF= predict(iris.rf, new_flowers)
results_NF <- cbind(new_flowers, Predicted_Species = prediction_NF)
results_NF

#Prototypes of group
data(iris)
iris.rf <- randomForest(iris[,-5], iris[,5], prox=TRUE)
View(iris.rf)
iris.p <- classCenter(iris[,-5], iris[,5], iris.rf$prox)
View(iris.p)
plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4],
     bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))],
     main="Iris Data with Prototypes")
points(iris.p[,3], iris.p[,4], pch=21, cex=2, bg=c("red", "blue", "green"))