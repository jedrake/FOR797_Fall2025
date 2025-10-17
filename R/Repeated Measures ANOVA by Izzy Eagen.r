
#Step 1
#Load datasets
library(nlme)
data(BodyWeight)

#Create histogram to model data
hist(model$residuals)
summary(BodyWeight)

#Load library(ggplot2) if needed before this step
ggplot(data = BodyWeight, aes(x = Time, y = weight, color = Rat))+
    geom_smooth(formula = y~x, method = "lm")+
    facet_wrap(~ Diet)

#Step 2
#Code from Jacob (Izzy struggled)
repeated = lmer(weight ~ Diet * Time + (1|Rat), data = BodyWeight)
repeated = lme(weight ~ Diet * Time,
               random = ~ 1 | Rat,
                    correlation = corAR1(form = ~ Time | Rat),
                    data = BodyWeight)
#Step 2 (ish)
#LME Model (Izzy's Version) 
#Grab nlme from library if needed again 
library(nlme)

# Load the dataset
data(BodyWeight)

# Fit a repeated-measures model with AR(1) correlation
repeated <- lme(
  weight ~ Diet * Time,
  random = ~ 1 | Rat,
  correlation = corAR1(form = ~ Time | Rat),
  data = BodyWeight
)

summary(repeated)

#Step 3 (Emmeans)
library(multcompView)
library(emmeans)
marginal = emmeans(repeated, ~ Diet*Time)
pairs(marginal, adjust="tukey")
#Output
# contrast                                                estimate   SE df
#Diet1 Time33.5454545454545 - Diet2 Time33.5454545454545   -221.0 22.4 13
#Diet1 Time33.5454545454545 - Diet3 Time33.5454545454545   -262.1 22.4 13
#Diet2 Time33.5454545454545 - Diet3 Time33.5454545454545    -41.1 25.9 13
#t.ratio p.value
#-9.844  <.0001
#-11.674  <.0001
#-1.585  0.2864

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 3 estimates

#Step 3 (continued) Jacob's Code ~
install.packages("multcomp")
library(multcomp)
CLD = cld(marginal, alpha = 0.05, Letters = letters, adjust = "sidak")
CLD

marginal = emtrends(repeated, ~ Diet, var = "Time")
pairs(marginal, adjust="sidak")
library(multcomp)
CLD = cld(marginal, alpha = 0.05, Letters = letters, adjust = "sidak")
CLD

#Step 3 Izzy's modified version (accounting for missing 2nd line from Jacob's earlier)
#If Jacob's doesn't work on the first go, do this first then enter Jacob's code!
library(nlme)
library(emmeans)

data(BodyWeight)

# Fit repeated measures model
model <- lme(weight ~ Diet * Time, random = ~1|Rat, data = BodyWeight)

# Get trends (slopes) of Time for each Diet
trend <- emtrends(model, ~ Diet, var = "Time")
trend

# Compare slopes between Diets
pairs(trend)

#Step 4 - If you wanna be fancy 
#Graphing using mean weights from CLD & time trend data
dev.off()  # reset graphics device

ggplot() +
  geom_line(data = do.call(rbind, lapply(1:3, function(i) {
    data.frame(
      Diet = slopes$Diet[i],
      Time = Time,
      weight = slopes$intercept[i] + slopes$slope[i] * Time,
      letters = slopes$letters[i]
    )
  })), aes(x = Time, y = weight, color = Diet), linewidth = 1.2) +  # <- changed
  geom_point(data = slopes, aes(x = 0, y = intercept, color = Diet), size = 3) +
  geom_text(data = slopes, aes(x = 0, y = intercept + 20, label = letters, color = Diet), size = 5) +
  labs(x = "Time", y = "Weight", title = "Weight Trajectories by Diet") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green"))

#alt alt code??
library(ggplot2)

# Example dataset of trends for visualization
slopes <- data.frame(
  Diet = factor(c(1, 3, 2)),
  intercept = c(263, 530, 487),      # mean weights from CLD
  slope = c(0.36, 0.658, 0.965),    # Time.trend from CLD
  letters = c("a", "b", "c")
)

# Simulate Time points
Time <- seq(0, 50, by = 1)

# Build a plot
ggplot() +
  # Lines showing growth trajectories for each diet
  geom_line(data = do.call(rbind, lapply(1:3, function(i) {
    data.frame(
      Diet = slopes$Diet[i],
      Time = Time,
      weight = slopes$intercept[i] + slopes$slope[i] * Time,
      letters = slopes$letters[i]
    )
  })), aes(x = Time, y = weight, color = Diet), size = 1.2) +
  # Points at Time = 0 (starting weight)
  geom_point(data = slopes, aes(x = 0, y = intercept, color = Diet), size = 3) +
  # Annotate letters for significance
  geom_text(data = slopes, aes(x = 0, y = intercept + 20, label = letters, color = Diet), size = 5) +
  labs(x = "Time", y = "Weight", title = "Weight Trajectories by Diet") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green"))


