#---
#  title: "Goodness of Fit"
#author: "Allison Humbert"
#date: "2025-09-05"
#output: html_document
#---




### install.packages("DescTools") ### run if you haven't already installed DescTools
library(DescTools)
### install.packages("dplyr") ### run if you haven't already installed dplyr
library(dplyr)

### load in your dataset (this one is from base r)
data("warpbreaks")
View(warpbreaks)

### use dplyr to select just to column of categorical data you want
Wool_vs_tension <- warpbreaks %>% select(wool, tension)

### use the table function to get your frequencies
table <- table(Wool_vs_tension)

### run the G-test
GTest(table)


### G-test Goodness-of-Fit
setwd("C:/Users/Allison/Desktop/R_Seminar")
### install.packages("DescTools") ### run if you haven't already installed DescTools
library(DescTools)
### install.packages("dplyr") ### run if you haven't already installed dplyr
library(dplyr)
### load in your dataset (this one is from base r)
data("warpbreaks")
View(warpbreaks)
### use dplyr to select just to column of categorical data you want
tension <- warpbreaks %>% select(tension)
### use the table function to get your frequencies
tension_table <- table(tension)
### run the G-test
GTest(tension_table)


install.packages("EMT") ### run if you haven't already installed EMT
library(EMT)
install.packages("XNomial") ### run if you haven't already installed XNomial
library(XNomial)

### Multinomial Exact Test


### This time, instead of going from an existing dataset in base R, let's make lists of the observed and expected frequencies manually, as shown in the textbook. Say you encounter a Beech-Maple-Birch stand that has yellow birch, American beech, sugar maple, and very few other tree species in it. You want to confirm if the community composition of this stand lines up with the literature reference you have been frequently citing throughout your thesis. Your literature reference indicates the composition of the stand should be 38% sugar maple, 34% yellow birch, 23% American beech, and 5% other species. However, you are an extremely exhausted graduate student, and didn't randomly sample and identify enough trees to meet the assumptions of Chi Square or the G-test. We can still try the multinomial exact test. You sampled and identified 12 sugar maple, 8 yellow birch, 4 American beech, and 1 lonely hop-hornbeam.

observed    = c(12, 8, 4, 1)
theoretical = c(0.38,0.34,0.23,0.05)
multinomial.test(observed,theoretical)
xmulti(observed, theoretical)
