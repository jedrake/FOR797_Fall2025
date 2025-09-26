head(iris)
summary(iris)
### Use head and summary functions to preview data.

model = lm(Sepal.Length ~ Species, data=iris)
### Fit linear model, set as "model". Set dependent variable as Sepal.Length, independent variable as Species, and data as iris.

hist(residuals(model))
### Plot histogram of residuals to check normality (looking for bell curve). 

plot(model)
### Plot residuals to check for equal variance. Can run multiple times for different plots.

library(car)
### Download car package.

Anova(model)
### Run ANOVA, can set type (e.g. type = "II") if needed.

### Moving on to post-hoc pairwise comparisons!

library(emmeans)
### Download emmeans package.

marginal = emmeans(model, ~Species)
### Set emmeans of model as "marginal". Can use adjustment (e.g. "sidak") if needed.

marginal
### Run marginal to view and compare estimated marginal means.

pairs(marginal)
### Calculate p-values for pairwise comparisons and estimate differences of means. Can use adjustment if needed.

library(multcomp)
### Download multcomp package. 

CLD = cld(marginal, Letters = letters)
### Set cld (compact letter display) of "marginal" as "CLD".

CLD
### Run CLD to see letter groupings of means. Means that share letters are not significantly different.