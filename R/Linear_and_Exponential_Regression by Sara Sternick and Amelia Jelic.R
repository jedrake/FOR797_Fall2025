##Linear Regression
#packages needed for linear regression tutorial: 
library(car)

#look at Orange dataset
data(Orange)
head(Orange)
#plot data
plot(Orange$age, Orange$circumference)

#create linear model
model.Orange <- lm(Orange$age ~ Orange$circumference)
summary(model.Orange)

#plot the data and our model!
plot(Orange$age ~ Orange$circumference,
     xlab="Age (days)", ylab="Circumference (mm)")
abline(model.Orange, col="red", lwd=2)

#time to test assumptions!
#get residuals
resid(model.Orange)
#test residuals to see if residuals are normally distributed
shapiro.test(resid(model.Orange)) #P-value > 0.05, so residuals are normally distributed
#plot to get a visual
hist(resid(model.Orange))
qqPlot(resid(model.Orange)) #looks good enough!

#make a residual plot to see what the variance looks like (homoscedasticity)
plot(resid(model.Orange) ~ fitted(model.Orange))
abline(0,0, col="red")
#no obvious patterns, so variance of residuals looks okay


##Exponential Regression
#packages needed for exponential regression tutorial: 
library(GrowthCurveME)
library(ggplot2)

data("exp_mixed_data")
head(exp_mixed_data)
ggplot(exp_mixed_data, 
       aes(x = time, y = growth_metric)) +
  geom_point() +
  labs(title = "Time vs Growth metric",
       x = "Time",
       y = "Growth metric") +
  theme_minimal()

#fit the model
model.growth_metric <- lm(log(growth_metric) ~ time, data = exp_mixed_data)
summary(model.growth_metric)
coef(model.growth_metric)


#residuals
ggplot(model.growth_metric, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  xlab("Fitted Values (log scale)") +
  ylab("Residuals (log scale)") +
  ggtitle("Residuals vs Fitted Values for Exponential Model")

#view the output of the model
summary(model.growth_metric)
#ln(y)=1.885+0.006554(x)
exp(1.885)
#6.586
exp(0.006554)
#1.0066
#y=6.586+1.0066^x

ggplot(exp_mixed_data, aes(x = time, y = growth_metric)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = (y ~ a + b^x), 
              se = FALSE, 
              color = "red", 
              start = list(a = 6.586, b = 1.0066)) + 
  labs(title = "Exponential Fit to Data",
       x = "Time",
       y = "Growth metric") +
  theme_minimal()

