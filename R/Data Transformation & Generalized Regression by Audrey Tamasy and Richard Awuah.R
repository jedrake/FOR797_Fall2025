install.packages ("e1071")
library(e1071)   # for skewness
library(ggplot2) # for visualization
library(dplyr)   # for data wrangling
library(cowplot) # to arrange plots together
# Install and load cowplot
install.packages("cowplot")   # run once
library(cowplot)

#Load dataset
data(airquality)
air <- na.omit(airquality)

#Inspect data
head(air)
summary(air)


#Check Right-Skewed Variable: Ozone
hist(air$Ozone, probability = TRUE, col = "tomato", breaks = 15,
     main = "Histogram of Ozone", xlab = "Ozone (ppb)")
curve(dnorm(x, mean = mean(air$Ozone, na.rm = TRUE), 
            sd = sd(air$Ozone, na.rm = TRUE)), 
      add = TRUE, col = "blue", lwd = 2)
qqnorm(air$Ozone, main = "Q–Q Plot: Ozone")
qqline(air$Ozone, col = "blue", lwd = 2)


# Optional: quick skewness approximation without packages
skew_approx <- function(x) {
  n <- length(x)
  m3 <- mean((x - mean(x))^3)
  s3 <- sd(x)^3
  (m3 / s3)
}
skew_approx(air$Ozone)

#️Apply Transformations (Right Skew)

air$log_Ozone <- log(air$Ozone)
air$sqrt_Ozone <- sqrt(air$Ozone)
air$recip_Ozone <- 1/air$Ozone


# Raw
hist(air$Ozone, probability = TRUE, main = "Original Ozone", col = "tomato")
curve(dnorm(x, mean = mean(air$Ozone, na.rm = TRUE), sd = sd(air$Ozone, na.rm = TRUE)),
      add = TRUE, col = "blue", lwd = 2)

# Log
hist(air$log_Ozone, probability = TRUE, main = "Log(Ozone)", col = "lightgreen")
curve(dnorm(x, mean = mean(air$log_Ozone, na.rm = TRUE), sd = sd(air$log_Ozone, na.rm = TRUE)),
      add = TRUE, col = "blue", lwd = 2)

# Square Root
hist(air$sqrt_Ozone, probability = TRUE, main = "Sqrt(Ozone)", col = "lightblue")
curve(dnorm(x, mean = mean(air$sqrt_Ozone, na.rm = TRUE), sd = sd(air$sqrt_Ozone, na.rm = TRUE)),
      add = TRUE, col = "blue", lwd = 2)

# Reciprocal
hist(air$recip_Ozone, probability = TRUE, main = "reciprocal/Ozone", col = "lightpink")
curve(dnorm(x, mean = mean(air$recip_Ozone, na.rm = TRUE), sd = sd(air$recip_Ozone, na.rm = TRUE)),
      add = TRUE, col = "blue", lwd = 2)


transforms <- list(
  Raw = air$Ozone,
  Log = log(air$Ozone),
  Sqrt = sqrt(air$Ozone),
  Recip = 1/air$Ozone
)

par(mfrow = c(2, 2))
for (name in names(transforms)) {
  res <- residuals(lm(transforms[[name]] ~ air$Temp))
  qqnorm(res, main = paste("Q–Q Plot:", name, "Residuals"))
  qqline(res, col = "blue", lwd = 2)
}

# Normality test for each
shapiro.test(air$Ozone)        # Raw
shapiro.test(log(air$Ozone))   # Log
shapiro.test(sqrt(air$Ozone))  # Square Root
shapiro.test(1/air$Ozone)      # Reciprocal

#Modeling transformed data
modelraw = lm(Ozone ~ Wind, data = airquality)
modellog = lm(log(Ozone) ~ Wind, data = airquality)
modelsqrt = lm(sqrt(Ozone) ~ Wind, data = airquality)
modelinverse = lm((1/Ozone) ~ Wind, data = airquality)

#Check each individual residual qqplot
plot(modelraw, which = 2)
plot(modellog, which = 2)
plot(modelsqrt, which = 2)
plot(modelinverse, which = 2)

#Backtransform predictions and estimates

exp(coef(modellog))
log_predictions = exp(predict(modellog))

sqrtestimates = ((coef(modelsqrt))^2)
sqrtestimates
sqrt_predictions = ((predict(modelsqrt))^2)

inverseestimates = 1 / coef(modelinverse)
inverseestimates
inverse_predictions = (1 / predict(modelinverse))




###############################################################################
# Added by John Drake- examples of how to find the "best" transformation.
# As always... there is an R package for that!
###############################################################################
data(airquality)
air <- na.omit(airquality)
hist(air$Ozone) # not normal

install.packages("bestNormalize")
library(bestNormalize) # this package runs a bunch of transformations and 
                       # helps you find the "best" transformation
bestNormalize(air$Ozone) # suggests the Yeo-Johnson transformation, but log is close

air$Ozone_YJ <- predict(yeojohnson(air$Ozone))
hist(air$Ozone_YJ) # much closer to normal!

air$Ozone_log <- log(air$Ozone)
hist(air$Ozone_log) # much closer to normal, but still minor left skew

###############################################################################
###############################################################################



# Load dataset
data("USJudgeRatings")
df <- USJudgeRatings

#Inspect data
head(df)
summary(df)


# Focus on PHYS
pH <- df$PHYS

# Define reflected log function
reflected_log <- function(x, eps = 1e-6) {
  C <- max(x, na.rm = TRUE) + eps
  log(C - x)
}

# Transformations
pH_sq   <- pH^2
pH_cube <- pH^3
pH_ref  <- reflected_log(pH)

# Store in list
vars <- list(Raw = pH, Square = pH_sq, Cube = pH_cube, RefLog = pH_ref)

# Function to safely run Shapiro–Wilk
safe_shapiro <- function(x) {
  x <- as.numeric(na.omit(x))
  if (length(unique(x)) < 3) return(list(W = NA, p.value = NA))
  if (length(x) > 5000) x <- sample(x, 5000)  # cap at 5000 obs
  test <- shapiro.test(x)
  list(W = unname(test$statistic), p.value = unname(test$p.value))
}


for (nm in names(vars)) {
  x <- vars[[nm]]
  test <- safe_shapiro(x)
  sk <- skewness(x)
  cat(sprintf("%-10s  W = %-7.4f  p = %-8.4g  Skewness = %-6.4f\n",
              nm, test$W, test$p.value, sk))
}

# Raw
hist(pH, main = "Histogram: PHYS (Raw)", xlab = "PHYS", col = "lightblue", border = "white")
qqnorm(pH, main = "Q–Q Plot: PHYS (Raw)"); qqline(pH, col = "blue")

# Square
hist(pH_sq, main = "Histogram: PHYS²", xlab = "PHYS²", col = "lightblue", border = "white")
qqnorm(pH_sq, main = "Q–Q Plot: PHYS²"); qqline(pH_sq, col = "blue")

# Cube
hist(pH_cube, main = "Histogram: PHYS³", xlab = "PHYS³", col = "lightblue", border = "white")
qqnorm(pH_cube, main = "Q–Q Plot: PHYS³"); qqline(pH_cube, col = "blue")

# Reflected Log
hist(pH_ref, main = "Histogram: Reflected Log (PHYS)", xlab = "log(C - PHYS)", col = "lightblue", border = "white")
qqnorm(pH_ref, main = "Q–Q Plot: Reflected Log (PHYS)"); qqline(pH_ref, col = "blue")


###Generalized Regression###
###Poisson for whole number counts###

data(warpbreaks)
str(warpbreaks)
head(warpbreaks)

#fitting a poisson model#
poisson_model <- glm(breaks ~ wool + tension,
             data = warpbreaks,
             family = poisson(link = "log")) 

summary(poisson_model) #coefficients on a log scale 

exp(coef(poisson_model)) #transforms from log scale to intuitive data scale

##lets try some other tests##

install.packages("MuMIn") 
library(MuMIn)

##pseudo r.square
r.squaredGLMM(poisson_model)

##QQ plot
deviance_residuals = residuals(poisson_model,type="deviance")

qqnorm(deviance_residuals)
qqline(deviance_residuals)

#testing dispersion dispersion ratio should be ~1 to use poisson
poisson_model$deviance / poisson_model$df.residual

##it is much higher than 1 so this poisson is not the best model to use for this data set
#Poisson distribution is inherent of the data






