# install & load packages
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}
if(!require(car)){install.packages("car")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(emmeans)){install.packages("emmeans")}
if(!require(nlme)){install.packages("nlme")}
if(!require(rcompanion)){install.packages("rcompanion")}

library(psych)
library(FSA)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(multcomp)
library(multcompView)
library(emmeans)
library(nlme)
library(rcompanion)

# load data & assign blocks
data("PlantGrowth")
head(PlantGrowth)
PlantGrowth <- PlantGrowth[order(PlantGrowth$group, PlantGrowth$weight), ]
PlantGrowth$block <- gl(5, 1, length = nrow(PlantGrowth))

# check data frame
headTail(PlantGrowth)
str(PlantGrowth)
summary(PlantGrowth)
Summarize(weight ~ group + block, data=PlantGrowth, digits=3)

# one-way ANOVA w/ blocks (FIXED)
model_fixed <- lm(weight ~ group + block, data = PlantGrowth)
summary(model_fixed)
Anova(model_fixed, type = "II")

# histogram & plot of residuals (FIXED)
x <- residuals(model_fixed)
plotNormalHistogram(x)
plot(fitted(model_fixed), residuals(model_fixed))
par(mfrow = c(2,2))
plot(model_fixed)

# post-hoc analysis (FIXED)
emm_fixed <- emmeans(model_fixed, ~ group)
pairs(emm_fixed, adjust="tukey")
CLD_fixed <- cld(emm_fixed, alpha = 0.05, Letters = letters, adjust = "tukey")
CLD_fixed

# plot of means, confidence intervals, & mean-separation letters (FIXED)
CLD_fixed$.group <- gsub(" ", "", CLD_fixed$.group)
ggplot(CLD_fixed, aes(x = group, y = emmean, label = .group)) +
  geom_point(shape = 15, size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, size = 0.7) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  ylab("Estimated marginal mean\nweight (g)") +
  geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.75, 0.75, 0.75), color = "black")

# one-way ANOVA w/ random blocks (MIXED)
model_mixed <- lmer(weight ~ group + (1|block), data=PlantGrowth, REML=TRUE)
anova(model_mixed)
rand(model_mixed)

# p-value and pseudo R-squared for model vs mixed null (MIXED)
model_mixed_null <- lmer(weight ~ 1 + (1|block), data = PlantGrowth, REML = TRUE)
anova(model_mixed, model_mixed_null)
nagelkerke(fit = model_mixed, null = model_mixed_null)

# p-value and pseudo R-squared for model vs lm null (MIXED)
model_lm_null <- lm(weight ~ 1, data = PlantGrowth)
anova(model_mixed, model_lm_null)
nagelkerke(fit = model_mixed, null = model_lm_null)

# post-hoc analysis (MIXED)
emm_mixed <- emmeans(model_mixed, ~ group)
pairs(emm_mixed, adjust="tukey")
CLD_mixed <- cld(emm_mixed, alpha=0.05, Letters=letters, adjust="tukey")
CLD_mixed

# one-way ANOVA (NO BLOCKS)
model_oneway <- lm(weight ~ group, data = PlantGrowth)
emm_oneway   <- emmeans(model_oneway, ~ group)
CLD_oneway   <- cld(emm_oneway, alpha=.05, adjust="tukey", Letters=letters)

# plot all means, confidence intervals, & mean-separation letters
CLD_mixed$.group <- gsub(" ", "", CLD_mixed$.group)
CLD_oneway$.group <- gsub(" ", "", CLD_oneway$.group)

to_plot <- function(cld_obj, label){
  df <- as.data.frame(cld_obj)
  df$model <- label
  df[, c("model","group","emmean","lower.CL","upper.CL",".group")]
}

plot_df <- rbind(
  to_plot(CLD_oneway, "One-way (no blocks)"),
  to_plot(CLD_fixed,  "One-way (fixed blocks)"),
  to_plot(CLD_mixed,  "One-way (random blocks)")
)

plot_df$model <- factor(
  plot_df$model,
  levels = c("One-way (no blocks)", "One-way (fixed blocks)", "One-way (random blocks)")
)

ggplot(plot_df, aes(x = group, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  geom_text(aes(label = .group), nudge_y = 0.75) +
  facet_wrap(~ model, nrow = 1) +
  theme_bw() +
  labs(y = "Estimated marginal mean weight (g)",
       x = "Group",
       title = "PlantGrowth EMMs by model")