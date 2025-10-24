####  Load packages
install.packages("remotes")
remotes::install_github("mbrussell/stats4nr")
install.packages("ggplot2")
install.packages("plotly")

#- the lme4 package doesn't do some things that most people need
#  like p-values and r2 values. Here are some additional packages that 
#   add these useful things
install.packages("lmerTest")
install.packages("LMERConvenienceFunctions")
install.packages("MuMIn") # helps get conditional and marginal r2 values

library(stats4nr)
library(dplyr)
library (lme4)
library(ggplot2)
library(plotly)
library(lmerTest)
library(LMERConvenienceFunctions)
library(MuMIn)

### Data characteristics
str(redpine)
summary(redpine)
head(redpine)
# Model: Y=(β0+bi+bij)+β1X+ϵ

### Checking for NA's
which(is.na(redpine))

### Visualizing the data
hist(redpine$DBH)
plot(redpine$DBH, redpine$HT)

ggplot(redpine, aes(x=DBH, y=HT))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, color="red") +
              labs(title = " Diameter vs Height",
                   x = "Diameter", y = "Height") +
                theme_minimal()
  

### Explore relationships
cor(redpine$DBH, redpine$HT)  #strong positive linear relationship

### Model without random effects
model_lm<-lm(HT~DBH, data=redpine)
plot(model_lm)
summary(model_lm)

### Forest cover type as a random effect
n_distinct(redpine$CoverType)

ggplot(redpine, aes(DBH, HT)) +
  geom_point() +
  facet_wrap(~CoverType, ncol = 4) +
  labs(x = "Diameter at breast height (inches)",
       y = "Height (feet)") 

###  Model with random effect of cover type
model_1 <- lmer(HT~DBH + (1|CoverType), data=redpine) #fit  linear mixed-effects model
plot(model_1)
summary(model_1)
qqnorm(residuals(model_1))
qqline(residuals(model_1))
plot(fitted(model_1), residuals(model_1))
r.squaredGLMM(model_1) # outputs conditional and marginal r2 values
  # R2m is the marginal r2 value reflecting the effects of fixed effects only
  # R2c is the conditional r2 value reflecting BOTH fixed and random effects
  #  comparing these helps us understand how much the model has been improved
  #  by adding random effects.

### Function to extract the random effect terms from a mixed model
ranef(model_1)
###Assumption for random effects
ranef_model_1 <- ranef(model_1)
qqnorm(ranef_model_1$CoverType[,1])
qqline(ranef_model_1$CoverType[,1])

###  Regression lines for different intercepts: cover types 
ggplot(redpine, aes(DBH, HT)) +
  geom_point(size = 0.2) +
  geom_line(aes(y = predict(model_1), #to visualize predictions
                group = CoverType, 
                color = CoverType)) +
  labs(x = "Diameter at breast height (inches)",
       y = "Height (feet)")

### Random effect on slope
model_2 <- lmer(HT ~ 1 + DBH + (1 + DBH | CoverType),
                     data = redpine) #boundary (singular) fit:?isSingular indicates that the model is likely overfitted
summary(model_2)

###  Visualize the random effect of plot
n_distinct(redpine$PlotNum)

### For visualizing important differences in trends.
summary_data <- redpine %>%
  group_by(PlotNum) %>%
  summarise(mean_D = mean(DBH), mean_H = mean(HT))

p<- ggplot(summary_data, aes(x = mean_D, y = mean_H, 
                             text = paste("Plot:", PlotNum))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue")

ggplotly(p, tooltip = "text")

redpine %>% 
  filter(PlotNum == c(24, 102)) %>% 
  ggplot(aes(DBH, HT)) +
  geom_point() +
  facet_wrap(~PlotNum) +
  labs(title = "Red pine at Cloquet Forestry Center",
       subtitle = "HT-DBH by plot number",
       x = "Diameter at breast height (inches)",
       y = "Height (feet)")


###  Nested random effect on the intercept (plot number nested within cover type)
model_3 <- lmer(HT ~ DBH + (1 | CoverType/PlotNum),
                     data = redpine)
summary(model_3)
r.squaredGLMM(model_3) # outputs conditional and marginal r2 values. Marginal went way up

###  Testing for the significance of the models
anova(model_1,model_2, model_3) # This compares models 2 and 3 against model 1

###  Comparing AIC for each mixed model with random effects applied to the intercept:
AIC(model_lm,model_1,model_2,model_3)

###  Makingp predictions for observations used in model fitting:
###  best linear unbiased predictions (BLUPs)
### Using Model 1
redpine <- redpine %>% 
  mutate( HT_pred_fixed = predict(model_1, #predictions use fixed effects only
                                  redpine, 
                                  re.form = NA),
          HT_pred_re = predict(model_1, 
                           redpine, 
                           re.form = NULL) #predictions include all random effects
        ) 

redpine %>% 
  top_n(8)


### Using model 3
redpine_3 <- redpine %>% 
  mutate(HT_pred_re = predict(model_3, 
                              redpine, 
                              re.form = NULL), #predictions include all random effects
         HT_pred_fixed = predict(model_3, 
                                 redpine, 
                                 re.form = NA)) #predictions use fixed effects only

redpine_3 %>% 
  top_n(8)

#Which one is better? Root Mean Square Error (RMSE)
# RMSE for predictions including random effects
rmse_re <- sqrt(mean((redpine_3$HT - redpine_3$HT_pred_re)^2, na.rm = TRUE))
rmse_re
# RMSE for predictions using only fixed effects
rmse_fixed <- sqrt(mean((redpine_3$HT - redpine_3$HT_pred_fixed)^2, na.rm = TRUE))
rmse_fixed

