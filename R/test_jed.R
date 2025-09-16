library(car) #- needed for Anova function

#- check out the iris dataset
data(iris)
head(iris) # note we have four continuous variables measured for three species
str(iris)

#- it sure looks like the petal widths vary across species
boxplot(Petal.Width~Species,data=iris)

#- fit simple one-way anova to test this idea
model1 <- lm(Petal.Width~Species,data=iris)
Anova(model1) #- does type II Sums of Squares by default

#... need to assess assumptions... get code from students
