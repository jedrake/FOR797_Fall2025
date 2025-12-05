## Visualizations by Jacob Olichney
library(mgcv)
library(lme4)
library(Matrix)
library(lmerTest)
library(nlme)
library(multcompView)
library(lsmeans)
library(rcompanion)
library(psych)
library(car)
library(emmeans)
library(robustbase)
library(minpack.lm)
library(multcomp)
library(grid)
library(gridExtra)
library(dplyr)
library(viridis)
library(ggplot2)
library(plotrix)
library(effects)
library(cowplot)
library(see)
library(patchwork)
library(DHARMa)
library(merDeriv)
library(performance)
library(devtools)
install_github("bsurial/bernr")
library(bernr)
library(glmmTMB)
library(coreSim)

####### Looking at the data first #################

summary(Admission)
admission = Admission
admission$rank = as.factor(admission$rank)

############# base R plotting examples ###############

#scatterplot
plot(x = admission$gre, y = admission$gpa)
plot(admission$gpa ~ admission$gre, col = "darkgreen")
plot(admission$gpa ~ admission$gre, col = admission$rank, pch = 16, cex = 1)
abline(lm(admission$gpa ~ admission$gre), col = "red", lwd = 3)
plot(admission$gpa ~ admission$gre, col = admission$rank, pch = 2, cex = 2)

admission1 = subset(admission, rank == 1)
admission2 = subset(admission, rank == 2)
admission3 = subset(admission, rank == 3)
admission4 = subset(admission, rank == 4)

par(mfrow = c(2,2))
plot(admission1$gpa ~ admission1$gre, col = "darkred",
     main = "rank 1", pch = 15, cex = 1, cex.main = 2)
plot(admission2$gpa ~ admission2$gre, col = "darkorange", 
     main = "rank 2", pch = 16, cex = 1)
plot(admission3$gpa ~ admission3$gre, col = "yellow", 
     main = "rank 3", pch = 17, cex = 1)
plot(admission4$gpa ~ admission4$gre, col = "darkgreen", 
     main = "rank 4", pch = 18, cex = 1)


#box plot
boxplot(admission$gpa ~ admission$rank, 
        col = c("#FDE725FF", "#73D055FF", "#1F968BFF", "#440154FF"),
        ylab = "GPA", xlab = "Rank")

#correlation plots
pairs(admission)


################ ggplot2 plotting examples #############

ggplot(data = admission, 
       aes(x = gpa, y = gre, color = rank, fill = rank, shape = rank))+
  geom_point(size=2)

ggplot(data = admission, 
       aes(x = gpa, y = gre, fill = rank))+
  geom_boxplot()+
  scale_fill_manual(values = c("#FDE725FF", "#73D055FF", "#1F968BFF", "#440154FF"))

ggplot(data = admission, 
       aes(x = gpa, y = gre, color = rank))+
  geom_point(size=2)+
  facet_wrap(~rank)

############## Modeling and predictions example ################
diameter_data = read.csv("C:/Users/jacob/OneDrive/Documents/StatsR/diameter_visualization_data.csv")
diameter = diameter_data
diameter$Diam_mm = as.numeric(diameter$Diam_mm)
diameter$Taxa = as.factor(diameter$Taxa)
diameter$State = as.factor(diameter$State)
diameter$Condition = as.factor(diameter$Condition)
diameter$Tree_Type = diameter$Taxa
diameter$Silvicultural_Treatment = diameter$Condition
diameter$plotid = paste(diameter$Condition, diameter$State, sep = "")
diameter = subset(diameter, diameter$Diam_mm != 0)


diameter = aggregate(Diam_mm ~ Year + State + Condition + Tree_Type + plotid+ blockid + groupid,
                   data = diameter, FUN = mean)
summary(diameter)

ggplot(data = diameter, aes(x = Year, y = Diam_mm, color = Tree_Type, fill = Tree_Type))+
  geom_point()+
  facet_grid(State ~ Condition)



diametermodel = lme(Diam_mm ~ Tree_Type*Condition*Year, 
  random = ~ 1 | State/plotid/blockid/groupid,
  weights = varIdent(form = ~ 1 | Condition*Tree_Type),
  correlation = corAR1(form = ~ 1 | State/plotid/blockid/groupid),
  data = diameter, 
  na.action = na.omit,
  control =list(msMaxIter = 250, msMaxEval = 250))

anova(diametermodel)

############ Let's plot mean and standard error #############
datapoints = aggregate(Diam_mm ~ 
                       Tree_Type+Condition+Year, 
                     data = diameter, 
                     FUN = mean)

standarderrors = aggregate(Diam_mm ~ 
                       Tree_Type+Condition+Year, 
                     data = diameter, 
                     FUN = std.error)
diamSEs = standarderrors$Diam_mm


datapoints$diamSEs = diamSEs
datapoints = na.omit(datapoints)

########## Now to plot our data ############
ggplot()+
  geom_point(data = datapoints, 
             aes(x = Year, y = Diam_mm, color = Tree_Type, fill = Tree_Type),
             position = position_dodge(width = 0.5), size=2)+
  geom_errorbar(data = datapoints,
                aes(x = Year, ymin = Diam_mm - diamSEs, ymax = Diam_mm + diamSEs,
                    color = Tree_Type),
                position = position_dodge(width = 0.5))+
  facet_wrap(~Condition)

############## Extracting Predictions #############
diameter$predictions = predict(diametermodel, newdata = diameter, se.fit=T)
diameter$predictions2 = predict(diametermodel, newdata = diameter)
summary(diameter)

#predict sometimes struggles to handle lme and lmer
#nifty function from bernr

diamgrid = bolker_ci(diametermodel, 
                     newdat = diameter, 
                     pred_int = FALSE, 
                     conf_level = 0.95)
summary(diamgrid)

ggplot()+
  geom_point(data = datapoints, 
             aes(x = Year, y = Diam_mm, color = Tree_Type, fill = Tree_Type),
             position = position_dodge(width = 0.5), size=2)+
  geom_errorbar(data = datapoints,
                aes(x = Year, ymin = Diam_mm - diamSEs, ymax = Diam_mm + diamSEs,
                    color = Tree_Type),
                position = position_dodge(width = 0.5))+
  geom_line(data = diamgrid,
            aes(x = Year, y = pred, color = Tree_Type),
            lwd = 1)+
  geom_ribbon(data = diamgrid,
              aes(x = Year, ymin = ci_l, ymax = ci_h, fill = Tree_Type),
              linetype = 2, lwd = 0.5, alpha = .2)+
  facet_wrap(~Condition)


ggplot()+
  geom_point(data = datapoints, 
             aes(x = Year, y = Diam_mm, color = Tree_Type, fill = Tree_Type),
             position = position_dodge(width = 0.5), size=2)+
  geom_errorbar(data = datapoints,
                aes(x = Year, ymin = Diam_mm - diamSEs, ymax = Diam_mm + diamSEs,
                    color = Tree_Type),
                position = position_dodge(width = 0.5))+
  geom_line(data = diamgrid,
            aes(x = Year, y = pred, color = Tree_Type),
            lwd = 1)+
  geom_ribbon(data = diamgrid,
              aes(x = Year, ymin = ci_l, ymax = ci_h, fill = Tree_Type),
              linetype = 2, lwd = 0.5, alpha = 0.2)+
  geom_boxplot(data = datapoints,
               aes(x = Year, y = Diam_mm, group = Year),
               alpha = 0.1)+
  facet_wrap(~Condition)

################# Other Example ####################33


leafrmassplotdata = read.csv("C:/Users/jacob/OneDrive/Documents/American Chestnut/LeafRmassplot.csv")
leaftempplotdata = read.csv("C:/Users/jacob/OneDrive/Documents/American Chestnut/Leaftempplot.csv")
leafrmassplotdata$Months = as.factor(leafrmassplotdata$Monthnum)
leafrmassplotdata$Silvicultural_Treatment = as.factor(leafrmassplotdata$Silvicultural_Treatment)
leafrmassplotdata$Variety = as.factor(leafrmassplotdata$Variety)
leafrmassplotdata <- leafrmassplotdata %>%
    mutate(Variety = recode(Variety, "OxO-" = 'OxO-', "OxO+" = 'OxO+', "B3" =  'B3F3', "Hyb" = "Hybrid"))
leafrmassplotdata$Variety = factor(leafrmassplotdata$Variety,
                                     levels=c("OxO-","OxO+","B3F3", "Hybrid"))
leafrmassplotdata <- leafrmassplotdata %>%
    mutate(Months = recode(Months, "5" = 'May', "6" = 'June', "7" =  'July', "8" = "August"))
leaftempplotdata$Months = as.factor(leaftempplotdata$Monthnum)
leaftempplotdata<- leaftempplotdata %>%
    mutate(Months = recode(Months, "5" = 'May', "6" = 'June', "7" =  'July', "8" = "August"))

leafrmassplotdataOpen = subset(leafrmassplotdata, Silvicultural_Treatment == "Open")
leafrmassplotdataShelter = subset(leafrmassplotdata, Silvicultural_Treatment == "Shelter")

rmasstempplot = ggplot() + 
  geom_point(data = leafrmassplotdata, 
             aes(x = Months, y = Rmass.mean, fill = Variety, color = Variety, 
                 shape = Silvicultural_Treatment),
             size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(data = leafrmassplotdata, 
                aes(x = Months, ymin = Rmass.mean - Rmass.se, ymax = Rmass.mean + Rmass.se,
                    color = Variety, shape = Silvicultural_Treatment),
                position = position_dodge(width = 0.5), width = 0.25, linewidth = 1.1)+
  geom_point(data = leaftempplotdata, 
             aes(x = Months, y = avgtempc.mean, size = "Preceding Temperature"),
             position = position_nudge(x = -0.3)) +
  geom_line(data = leaftempplotdata,
            aes(x = Months, y = avgtempc.mean, group = Silvicultural_Treatment),
            position = position_nudge(x = -0.3))+
  scale_fill_viridis(discrete = T, option = "C") +
  scale_color_viridis(discrete = T, option = "C")+
  scale_shape_manual(values = c(1,16))+
  scale_y_continuous(name = "Leaf Respiration nmol g-1 s-1", 
                     sec.axis = sec_axis(~ (. - 0), 
                                         name = "Temperature (°C)",
                                         breaks = c(0, 10, 20, 30))) +
  theme_classic()+
  theme(text=element_text(size=20), panel.spacing = unit(2, "lines"))+
  guides(size = guide_legend(title = NULL),
         shape = guide_legend(title="Silvicultural Treatment"))
rmasstempplot
