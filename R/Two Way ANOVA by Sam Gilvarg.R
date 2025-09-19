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

data <- read.csv("R/two-way-anova_data.csv")


normality_test <- shapiro.test(data$spp)


variance_test <- leveneTest(spp ~ interaction(shape, season), data = data)


plot(fullmodel, which = 2)
hist(residuals(fullmodel))



fullmodel =lm(spp ~ shape * season, data = data)


Anova(fullmodel, type = "II")



marginal = emmeans(fullmodel, ~ shape * season)

CLD = cld(marginal, alpha = 0.05, Letters = letters, adjust = "sidak")
CLD

pairs(marginal)
