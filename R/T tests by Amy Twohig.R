#* t tests: 1 and 2 sample by Amy Twohig 
#* continuous quant variable: test for normality
#* 
#* 
#*      
#* ONE SAMPLE:
#* check normal distribution 
#* hist() will show histogram
#*      hist(data sample name, breaks = # columns on graph)
#*      
#* t.test() for values compared to theoretical value
#*      t.test(value name, mu = expected mean, conf. int = 0.95, alternative = "two.sided")

# powerpoint example: use trees dataset from stats package
## load dataset + print first 10 entries
data(trees)
head(trees)

## histogram of tree height: testing normal distributon
hist(trees$Height)

#* t.test(observed value,mu = theoretical, conf.int = 0.95)
#* going to pretend our theoretical mean is 75

t.test(trees$Height, mu = 75, conf.level = 0.95)


#* TWO SAMPLE:
#* two sample t-test using made up height data set
#* need two variables: gender (male/female) and height
maleHeight <- c(67, 68, 72, 71, 64, 69, 74, 71, 71, 70)
femaleHeight <- c(61, 62, 66, 61, 68, 63, 62, 60, 62, 65)

# add male and female height data to single data frame, 'height'
height <- data.frame(maleHeight, femaleHeight)
print(height)

#* testing assumptions:
#* variances between both groups need to be equal (esp. with small data set):
#*  bartlett.test(y (what you measured) ~ groups tested between (i.e. species))
#*  in this case: values are independent variables so nest in list()
#*  great explanation: https://www.geeksforgeeks.org/r-language/bartletts-test-in-r-programming/

bartlett.test(list(height$maleHeight, height$femaleHeight))


#* testing normal distributions with q-q plot
#* q-q = quantile-quantile: determines if data set follows a certain probability distribution

# qqnorm plots points
qqnorm(maleHeight)
# qqline draws a line thru all points
qqline(maleHeight)

#repeat for female
qqnorm(femaleHeight)
qqline(femaleHeight)

#* two sample t-test
#* t.test(x, y, alternative = "two.sided", paired = FALSE, var.equal = TRUE, conf.level = 0.95)
t.test(height$maleHeight,height$femaleHeight, alternative = "two.sided", paired = FALSE, var.equal = TRUE, conf.level = 0.95)