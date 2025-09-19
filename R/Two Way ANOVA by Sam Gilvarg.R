# install.packages("car")
# install.packages("multcomp")
# install.packages("multcompView")
# install.packages("emmeans")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("rstatix")

library(car)
library(multcomp)
library(multcompView)
library(emmeans)
library(tidyr)
library(dplyr)
library(rstatix)

data <- read.csv("R/two-way-anova_data.csv",stringsAsFactors = T)
head(data)
str(data)

#- this tests if the data are normal, which doens't really matter.
# You should test if the RESIDUALS are normal. So don't do this.
# Fit your model, then assess assumptions. Note that this test
#   is overly conservative.
normality_test <- shapiro.test(data$spp)

#- 
variance_test <- leveneTest(spp ~ interaction(shape, season), data = data)


plot(fullmodel, which = 2)
hist(residuals(fullmodel))


# fit model, assess normality of residuals
fullmodel =lm(spp ~ shape * season, data = data)
hist(fullmodel$residuals) # don't look too bad
normality_test <- shapiro.test(fullmodel$residuals)
normality_test # not statistically different than normal

Anova(fullmodel, type = "II")



marginal = emmeans(fullmodel, ~ shape * season)

CLD = cld(marginal, alpha = 0.05, Letters = letters, adjust = "sidak")
CLD

pairs(marginal)
