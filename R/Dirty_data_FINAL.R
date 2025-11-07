#Creating consistent data:

#NA insertion in the dataset 
#Coercion of variable classes
#Combination of levels within factor variables
#Reordering of levels within factor variables
#Dealing with NA's
#Case normalization within character variables
#Formatting dates and times
#Identifying and correcting inconsistencies in DBH
#Sorting and summarizing data before analysis

#download csv from Github
dirtydata <- read.csv("C:/Users/marci/Desktop/R seminar/FOR797_Fall2025/R/tree_sites_unclean.csv")

#make a copy of the data in case we mess up!
dirtydata2 <- dirtydata

#first see summary of data to see data types as R interpreted them
summary(dirtydata)

#install packages: tidyr, dplyr
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("sjmisc")
library(tidyr)
library(dplyr)
library(lubridate)
library(sjmisc)

#return to csv read-in to use na.strings to turn missing observations into NAs
dirtydata <- read.csv("C:/Users/marci/Desktop/R seminar/FOR797_Fall2025/R/tree_sites_unclean.csv",na.strings = c("", "NA"))
dirtydata2 <- dirtydata

#rerun code to transform variables
dirtydata2$DBH = as.numeric(dirtydata2$DBH)
dirtydata2$COND = as.factor(dirtydata2$COND)

#got a NAs introduced by coercion message for DBH, but worked fine for COND
summary(dirtydata2$DBH)
#see that there are 2 NA's that have been introduced by coercion
#restore to original data.frame
dirtydata2 = dirtydata

#use gsub() to try to fix any typos in DBH
dirtydata2$DBH = gsub("[l]", "1", dirtydata2$DBH)
dirtydata2$DBH = gsub("[Oo]", "0", dirtydata2$DBH)

#try coercing again
dirtydata2$DBH = as.numeric(dirtydata2$DBH)
#no warning message returned this time

#run the rest of the coercion code to turn CD into a factor variable
dirtydata2$COND = as.factor(dirtydata2$COND)
#no warning messages returned
summary(dirtydata2$COND) 
#shows that there are two categories with NAs now: one labeled NA's containing the previously blank values and one labeled "N/A"

#First method of dealing with the category "N/A"
#since gsub() doesn't work well for factor variables, let's try a conditional operator to give the variable "N/A" a new name ("Unassigned")
#first we need to create a new factor level called "Unassigned" that R will recognize
levels(dirtydata2$COND) <- c(levels(dirtydata2$COND), "Unassigned")
dirtydata2$COND[dirtydata2$COND == "N/A"] = "Unassigned"
summary(dirtydata2$COND)
#now, if we look at the summary of our data.frame, we see that we added a couple of new variables in order to do this, so we just need to remove those
summary(dirtydata2)
#we can remove the extra "COND" variables easily by making them NULL:

#Then, if you want to you can drop the now empty N/A level; first, we create a new object that doesn't include the N/A level
dirtydata2$COND_cleaning <- dirtydata2$COND[dirtydata2$COND != "N/A"]
#drop the empty level "N/A"
dirtydata2$COND_cleaned <- droplevels(dirtydata2$COND_cleaning)

summary(dirtydata2$COND_cleaned)
dirtydata2$COND <- NULL
dirtydata2$COND_cleaning <- NULL
summary(dirtydata2)
#rename new column from COND_cleaned to Condition
library(dplyr)
dirtydata2 <- dirtydata2 %>%
  rename(Condition = COND_cleaned)
summary(dirtydata2)

#to get rid of these changes, run code again up to this point
dirtydata2 = dirtydata
dirtydata2$DBH = gsub("[l]", "1", dirtydata2$DBH)
dirtydata2$DBH = gsub("[Oo]", "0", dirtydata2$DBH)
dirtydata2$DBH = as.numeric(dirtydata2$DBH)
dirtydata2$COND = as.factor(dirtydata2$COND)
summary(dirtydata2$COND)

#alternatively, to just replace the level "N/A" with "NA", you can use a conditional operator while the variable is still a character variable
dirtydata2$COND[dirtydata2$COND == "N/A"] <- NA
summary(dirtydata2$COND)
#Still need to drop the "N/A" level:
dirtydata2$COND_cleaning<- dirtydata2$COND[dirtydata2$COND != "N/A"]
dirtydata2$COND_cleaned <- droplevels(dirtydata2$COND_cleaning)
summary(dirtydata2$COND_cleaned)
dirtydata2$COND <- NULL
dirtydata2$COND_cleaning <- NULL
summary(dirtydata2)
#rename variable COND_cleaning to Condition
library(dplyr)
dirtydata2 <- dirtydata2 %>%
  rename(Condition = COND_cleaned)
summary(dirtydata2)
#In this case, you can remove all NA's from the data set using na.omit
dirtydata_nona <- na.omit(dirtydata2)
summary(dirtydata_nona)
summary(dirtydata_nona$Condition)

#reorder levels using factor()
dirtydata_nona$Condition <- factor(dirtydata_nona$Condition, levels = c("Excellent", "Very Good", "Good", "Fair", "Poor", "Critical", "Dead"))

#to see outcome: Condition variable now a reordered factor variable with no NA's
summary(dirtydata_nona$Condition) 

##################################################################################################################################################
#change site_id from numeric to factor
summary(dirtydata_nona)
dirtydata_nona$site_id <- as.factor(dirtydata_nona$site_id)
summary(dirtydata_nona)
#now each site ID is it's own category as it is in the tree database in real life

###########################################################################################################################
#change PlantDate from character to date
#let's look at the format it's in
print(dirtydata_nona$PlantDate)
#we don't have a month to make it into a date, but we can change it to factor at least
dirtydata_nona$PlantDate <- as.factor(dirtydata_nona$PlantDate)
summary(dirtydata_nona$PlantDate)

#store inspect_tm and inspect_dt to time-date
class(dirtydata_nona$inspect_tm)
head(dirtydata_nona$inspect_tm)

#we have a column for time and a column for date.
#ultimately, we want a format of mm/dd/yyyy and hh:mm:ss.

#first, convert delimiters to the same
dirtydata_nona$inspect_tm = gsub("[:]", "-", dirtydata_nona$inspect_tm)
#create a new variable "datetime" by pasting date and time columns together
dirtydata_nona = dirtydata_nona %>%
  mutate(datetime = paste(inspect_dt, inspect_tm, sep = "-"))
#this is now a character, but at least the delimiters are consistent
summary(dirtydata_nona$datetime)

#change "datetime" to an actual date-time class
dirtydata_nona$datetime = ymd_hms(dirtydata_nona$datetime)

dirtydata_nona$datetime <- as.POSIXct(dirtydata_nona$datetime, tz = "EST", format = "%Y-%m-%d-%H-%M-%S")
head(dirtydata_nona$datetime)

####################################################################################################################################

#case normalization for SPP_bot
#see summary of character variable
summary(dirtydata_nona$SPP_bot)
#coerce the variable into a factor in order to see the categories
dirtydata_nona$SPP_bot = as.factor(dirtydata_nona$SPP_bot)
summary(dirtydata_nona$SPP_bot)
#coerce back into a character variable
dirtydata_nona$SPP_bot = as.character(dirtydata_nona$SPP_bot)

#case normalization within the variable, wherein every entry will have the first letter uppercase and the rest of the entry lowercase
#load library for stringr package (for str_to_sentence fn) and dplyr package (for mutate fn)
library(stringr)
library(dplyr)
#apply str_to_sentence() using mutate() 
#here, we use the pipe operator to chain together data manipulation steps and allow dplyr to mutate the entire column of data to fit the str_to_sentence() requirements
dirtydata_nona <- dirtydata_nona %>%
  mutate(SPP_bot = str_to_sentence(SPP_bot))
#If we convert to a factor variable, we see that this reduced the number of observations in the (Other) category by 4
dirtydata_nona$SPP_bot = as.factor(dirtydata_nona$SPP_bot)
summary(dirtydata_nona$SPP_bot)
#looking at the data, individual misspellings can be picked out, ex: Acer campester instead of Acer campestre
#these individual typos can be fixed like this:
dirtydata_nona$SPP_bot[dirtydata_nona$SPP_bot == "Acer campester"] <- "Acer campestre"
#let's try filtering out all non-Other categories (after making this a factor variable) to be able to more easily sort through the observations that don't fit into any other category
categories_to_exclude <- c("Malus spp.", "Platanus occidentalis", "Carpinus caroliniana", "Syringa reticulata", "Ostrya virginiana", "Asimina triloba", "Tilia americana", "Zelkova serrata", "Amelanchier grandifolia", "Quercus alba", "Cercis canadensis", "Ginkgo biloba", "Acer campestre", "Gleditsia triacanthos", "Acer rubrum", "Platanus x acerifolia", "Celtis occidentalis", "Liriodendron tulipifera", "Acer freemanii", "Quercus bicolor", "Diospyros virginiana", "Amelanchier laevis", "Gymnocladus dioicus", "Ulmus americana", "Cladrastis kentukea", "Carya ovata", "Acer saccharum", "Hamamelis virginiana", "Quercus macrocarpa", "Carya laciniosa", "Prunus serotina", "Tilia cordata", "Crataegus viridis", "Carpinus betulus", "Quercus rubra", "Quercus muehlenbergii", "Acer griseum", "Juniperus virginiana", "Liquidambar styraciflua", "Betula nigra", "Carya illinoensis", "Unknown spp.", "Amalanchier canadensis", "Cornus mas", "Gleditsia triacanthos inermis", "Cornus kousa", "Amelanchier spp.", "Prunus virginiana", "Ulmus x", "Juglans cinerea", "Nyssa sylvatica", "Corylus colurna", "Carya spp.", "Stump", "Acer miyabei", "Quercus coccinea", "Quercus imbricaria", "Castanea spp.", "Cercidiphyllum japonicum", "Pinus strobus", "Prunus sargentii", "Tilia euchlora crimean", "Crataegus spp.", "Sambucus canadensis", "Amelanchier arborea", "Maackia amurensis", "Tilia tomentosa", "Carya glabra", "Metasequoia glyptostroboides", "Morus rubra", "Picea abies", "Tilia mongolica", "Cornus spp.", "Quercus robur x macrocarpa", "Thuja occidentalis", "Acer nigrum", "Acer saccharinum", "Crataegus crusgalli", "Fagus grandifolia", "Planting site - needs approval", "Platanus occodentalis", "Quercus velutina", "Rhus spp.", "Abies balsamea", "Acer truncatum", "Corylus avellana", "Prunus americana", "Salix babylonica", "Syringa reticulata 'ivory silk'", "Carya cordiformis", "Cercis spp.", "Crataegus phaenopyrum", "Fagus sylvatica", "Pinus sylvestris", "Fraxinus spp.", "Maclura pomifera", "Morus spp." )
df_other <- dirtydata_nona %>%
  filter(!SPP_bot %in% categories_to_exclude)
summary(df_other$SPP_bot)
#this did work but was painstaking
#did not reveal more typos except for use of "Acer freemanii" in place of "Acer x freemanii"
dirtydata_nona$SPP_bot[dirtydata_nona$SPP_bot == "Acer freemanii"] <-"Acer x freemanii"
dirtydata_nona$SPP_bot = as.character(dirtydata_nona$SPP_bot)

#########################################################################################################################

#identify obvious inconsistencies in DBH 
#context: these trees were all planted in the last three years
summary(dirtydata_nona$DBH)
#there is definitely an inconsistency at the maximum. What species is it?
filtered_DBH <- dirtydata_nona %>% filter(dirtydata_nona$DBH > 10)
print(filtered_DBH)
#none of these species grow that fast - someone forgot decimals

#manually fix the typos since there aren't that many
dirtydata_nona$DBH[dirtydata_nona$DBH == "275"] = 2.75
dirtydata_nona$DBH[dirtydata_nona$DBH == "11"] = 1.1
dirtydata_nona$DBH[dirtydata_nona$DBH == "12"] = 1.2
dirtydata_nona$DBH[dirtydata_nona$DBH == "17"] = 1.7
summary(dirtydata_nona$DBH)

#What are those zeros? Let's get rid of them
filtered_DBH_zeros <- dirtydata_nona %>% filter(dirtydata_nona$DBH == 0)
print(filtered_DBH_zeros)
dirtydata_nona = subset(dirtydata_nona, DBH >0)
summary(dirtydata_nona$DBH)
#our minimum is now a reasonable value of 0.2
#can also check for inconsistencies/outliers using a histogram. Our data is so skewed it's hard to see.
histDBH <- hist(dirtydata_nona$DBH, main = "DBH distribution", xlab = "DBH", ylab = "Frequency", breaks = 50)

##################################################################################################################################

#Is there an association between DBH and COND?

#try without plant date
survival_analysis <- dirtydata_nona %>% 
  select(Condition, DBH) %>%
  group_by(Condition) %>%
  summarise(mean_DBH = mean(DBH)) %>%
  arrange(desc(mean_DBH))

print(survival_analysis)
plot(survival_analysis)