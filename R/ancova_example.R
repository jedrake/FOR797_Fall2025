head(iris)

## plot the sample data
ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  theme_minimal(base_size = 14) +
  labs(title = "Sepal Length by Species",
       x = "Petal Length (covariate)",
       y = "Sepal Length")

## run ANCOVA, to test interaction
model = lm (Sepal.Length ~ Petal.Length + Species + Petal.Length:Species,
              data = iris)
# or 
model = lm (Sepal.Length ~ Petal.Length * Species,
            data = iris)
## test assumptions
## histogram of residuals
hist(residuals(model.2),
     col="darkgray")

## residuals vs. predicted values
plot(fitted(model.2),
     residuals(model.2))

plot(model)

install.packages("visreg")
library(visreg)
visreg(model,xvar="Petal.Length",by="Species",overlay=T)
visreg(model.2,xvar="Petal.Length",by="Species",overlay=T)

## post hoc to see differences
library(emmeans)
emmeans(model.2, pairwise ~ Species)

library(car)
Anova(model, type="II")

## model to test slopes
model.2 = lm (Sepal.Length ~ Petal.Length + Species,
              data = iris)
Anova(model.2, type="II")

## model summary
summary(model.2)

## test assumptions
## histogram of residuals
hist(residuals(model.2),
     col="darkgray")

## residuals vs. predicted values
plot(fitted(model.2),
     residuals(model.2))

plot(model)

## post hoc to see differences
library(emmeans)
emmeans(model.2, pairwise ~ Species)
