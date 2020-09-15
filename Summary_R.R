#########################################################################
# PSA
########################################################################
# This document is a Summary of the R-code we learnt in Biostat,
# DO NOT edit the code in this document directly
# copy it into your Rstudio and run/edit it there,
# make sure it's working code before putting it into this document :)
# Example data used is lab1_dat from lab 1

########################################################################
# BASICS
########################################################################
# Attach Data
lab1_dat <- read.csv("data1.csv",
      header = TRUE, sep = ";", dec = ",", stringsAsFactors = TRUE)
attach(lab1_dat)
#how many females?
sum(sex == "f")

# how many people with sexes?
length(sex)

# which rows represent female samples? returns vector of indexes
which(sex == "f")

# what proportion are female? returns number
sum(sex == "f")/length(sex)
mean(sex == "f")

# how old are the females? returns vector of ages for females
age[sex == "f"]

# Compute the minimum and maximum age, separately in males and females.
tapply(age,sex, range) # tapply(to_what, by_what, what_function)

# male or female? returns number
length(sex == "f" | sex == "m")

# is everyone female? returns TRUE/FALSE
all(sex == "f")

# is there anyone who is female? returns TRUE/FALSE
any(sex == "f")

# What proportion of people are older than 65 and male?
mean(age > 65 & sex == "m")

# what proportion of males are older than 65?
mean(age[sex == "m"] > 65)

# How many people are male and smokers? Proportion?
sum(sex == "m" & smoke == 1)
mean(sex == "m" & smoke == 1)

# What is the proportion of non-smokers among males with the gene AA?
mean((smoke == 0)[gene=="AA" & sex=="m"])

# proportion of = mean(thing) among [group]


# frequentist probability

# when flipping a coin
coinflip <- sample(c("tail","head"), size=10000, replace=TRUE)
est_prob <- sum(coinflip=="head")/length(coinflip) #which is the same as
mean(coinflip == "head")
est_prob

########################################################################
# Cut & Quantile
########################################################################

# returns intervals of age cut into 3 equal sections
# based off of range of age
# e.g. age range of 10-40 splits to (10-20],(20-30],(30-40]
cut(age, 3)
# define your own cut intervals using a vector
cut(age, c(50,56,59,65))

# returns cut points for proportions of observations
# based off of probability distribution of age
# defaults to quartiles
quantile(age)
# define your own cut points using a vector
quantile(age, c(0, 0.2, 0.4, 0.6, 0.8, 1))

# combine cut and quantile to produce intervals with roughly same no. of observations
cut(age, quantile(age, c(0, 0.25, 0.5, 0.75, 1)))

########################################################################
# Conditional Probability
########################################################################

# what is the probability that a random sample smokes/has gene X
mean(smoke == 1)
mean(smoke == 0)
mean(gene == "AG")
mean(gene == "AA")
mean(gene == "GG")

# what is the probability that you are a smoker given that you have gene "AA" vs. gene "GG"
mean(smoke[gene=='AA']=='1') #which is the same as
tapply(smoke, gene == "AA", mean)
mean(smoke[gene=="GG"]=="1")

# How much does the chance of being a smoker change from general population to carrying the gene "AA"
mean(smoke[gene=='AA']=='1')/mean(gene == "AA")

# relative frequency

barplot(table(age)/length(age), ylab = "probability", xlab = "age")

########################################################################
# Tables
########################################################################

table(age)
prop.table(table(age))
xtabs(~age + gene + sex, data = lab1_dat)
summary(xtabs(~age + gene + sex, data = lab1_dat))

########################################################################
# Discrete Distribution
########################################################################

# binomial distribution

# dbinom is a probability mass function of binomial distribution.
# dbinom <- P(X=x), probability of observing value equal to x.
# pbinom is a cumulative distribution function of this distribution. 
# pbinom <- P(X<=x), probability of observing value smaller or equal than x.

# what is the probability that 2 people from the dataset are smokers when randomly selecting 5?
mean(smoke) #gives the probability of a person being a smoker, 0.5
dbinom(2, size = 5, prob = 0.5)
# what is the probability of selecting 2 or fewer smokers out of 5 random picks?
dbinom(0, size = 5, prob = 0.5)+
  dbinom(1, size = 5, prob = 0.5)+
  dbinom(2, size = 5, prob = 0.5)
# or
pbinom(2, size = 5, prob = 0.5)
# 2 or more smokers
pbinom(1, size = 5, prob = 0.5, lower = FALSE)
# roll 3 dice and let X be the number of 6s rolled. What is the distribution of X?
dist<- c(dbinom(0, size = 3, prob = 0.167),
         dbinom(1, size = 3, prob = 0.167),
         dbinom(2, size = 3, prob = 0.167),
         dbinom(3, size = 3, prob = 0.167))
nroll <- c(0,1,2,3)
plot(dist ~ nroll, xlab = "number of sixes", ylab = "probability")
min(dist)
max(dist)
# roll 2 dice and let X be number of 1s rolled. what is pmf for X?
dbinom(0, size = 2, prob = 0.167)
dbinom(1, size = 2, prob = 0.167)
dbinom(2, size = 2, prob = 0.167)

# poisson distribution

### If there are twelve cars crossing a bridge per minute on average,
### find the probability of having seventeen or more cars crossing the bridge in a particular minute.
1 -ppois(16, lambda = 12)
# or
ppois(16, lambda = 12, lower = FALSE)
# what is the probability of 13 cars passing?
dpois(13, lambda = 12)

########################################################################
# Continuous Distribution
########################################################################

# uniform 

# Draw 100 random numbers between 1 and 3
runif(100, min = 1, max = 3)
barplot(runif(100, min = 1, max = 3))

# normal distribution

# rnorm generates a normally distributed dataset
normal_dat <- rnorm(1000, mean = 0, sd = 1)
plot(density(normal_dat))
# pnorm returns the integral from -infinity to q of the pdf of the normal distribution where q is a Z-score
# given parameters for pnorm(q, mean = Î¼, sd = Ï)
# this says that about 99,8% of the data is below 3 z-scores from the mean
pnorm(3, mean = 0, sd = 1)
# this says that about 0,13% of the data is above 3 z-scores from the mean
pnorm(3, mean = 0, sd = 1, lower = F)
# 95% conf interval
pnorm(1.96, mean = 0, sd = 1) - pnorm(1.96, mean = 0, sd = 1, lower = F)

# Calculate the probability that a normal random variable with mean 10
# and standard deviation 4 is between -2 and 2 or between 14 and 18.
# That is, the probability of the union of the events -2 < X < 2 and 14 < X < 18.
pnorm(2, mean = 10, sd = 4) - pnorm(-2, mean = 10, sd = 4) +
  pnorm(18, mean = 10, sd = 4) - pnorm(14, mean = 10, sd = 4)

# what percentage of the participants are older than 60?
mean(age)
sd(age)
pnorm(60, mean = 56, sd = 11.23487, lower.tail = FALSE)
# check to see if roughly same as
mean(age > 60)

########################################################################
# Plotting
########################################################################
# line width
lwd(2)
#line character, 1 = full, 2 = dashed, 3 = dotted
lty(2)
#density plot of male/female age
plot(density(age[sex == "f"]),main = "density plot of male/female age") 
lines(density(age[sex == "m"]), lty = 2)
legend("topleft", legend = c("f","m"), lty = c(1,2))
# histogram, 1 variable, frequency  
hist(age)
# Histogram of age of females (association of age with sex)
hist(age[sex == "f"]) #which is the same as
tapply(age, sex == "f",hist)
# barplot, 1 variable,age of each person in set
barplot(age)
# barplot of table, nr. of persons that have each age in set
barplot(table(age))
# Cut age into four equally sized intervals and do a barplot of the
# proportion of smokers in the 4 age groups
age_cat <- cut(age, quantile, prob = c(0, 0.25, 0.5, 0.75, 1)
barplot(tapply(smoke, age_cat, mean),
        xlab = "age",
        ylab = "smokers")
          
# Represent graphically the association between age and smoking.
# For example, you can do a barplot of proportion of patients who smoke in each age group
barplot(tapply(smoke, age, mean),
        ylab = "smoke",
        xlab = "age")
# boxplot, plots summary
summary(age)
boxplot(age)
# multiple variable boxplots
boxplot(age ~ sex)
# uses the data of a table consisting of
table(sex,age)
# scatterplot, 2 variables
plot(age,gene)
plot(age, gene, col = c("orange", "darkgreen","blue")[gene]) # plot them all
legend("topleft", legend = c("AA", "AG","AC"), fill = c("orange", "darkgreen","blue"))

########################################################################
# Functions
########################################################################

# confidence interval: confidence(%) that mu is in interval
t.test(age, conf.level = 0.95)
t.test(age, conf.level = 0.99)
t.test(age, conf.level = 1)
qnorm(0.025, lower = F)

# 99% confidence interval without using t.test function
mean(age) + c(-1, 1) * qt(0.005, df = nrow(age) - 1, lower = F) *
  sd(age) / sqrt(nrow(age))

########################################################################
# Parametric tests (normal data or v. large n)
########################################################################
# One sample T.test
# Determines whether the sample mean (X-bar) is statistically different from known or hypothesized population mean (mu0).
# works on continuous, independent, approximately normal data. Assumes similar sample & population variance.
t.test(age, mu = 65)
# printing the p-value
t.test(age, mu = 65)$p.value
# printing the confidence interval
t.test(age, mu = 65)$conf.int
# changing the confidence interval
t.test(age, mu = 65, conf.level = 0.8)
# Two sample T.test
# Two sample T-test compares the mean of two groups to see if there is a significant difference between them.
# Assumes equal variance unless differently stated

# Two sample t.test where x and y are numeric vectors
x <- rnorm(100, mean = 2, sd = 1)
y <- rnorm(100, mean = 1.9, sd =1)
t.test(x , y)
# paired T.test
t.test(x,y, paired = TRUE)
# T.test with different variance
t.test(x,y, var.equal = FALSE)

# ANOVA
# Compares the mean of >2 groups to see if there is a significant difference between them. 
# Works on data similar to t.test data but also needs an independent categorial variable (a factor with >2 lvls)

anova(lm(age ~ gene))
# Between group values are first row, within group values second (residuals)
summary(aov(age ~ gene))

########################################################################
# Non-parametric tests (non-normal data with small n)
########################################################################
# binomial test

# Wilcox signed rank test
# One-sample t.test but non-parametric
# tests if the distribution of Xi is symmetric around a hypothetical median.
# Works on non-normal data with small n if samples are independent. Assumes equal variance.
wilcox.test(smoke, mu = 0.56)

# Wilcoxon rank-sum test
# Two-sample t.test but non-parametric
# Tests if the distributions are the same around the same median. 
wilcox.test(age[sex == "m"], age[sex == "f"] )
# or
wilcox.test(age ~ sex )

# Kruskal–Wallis rank-sum test
#Anova but for non-normal data, needs an independent categorial variable (a factor with >2 lvls)
kruskal.test(age ~ gene)
# k≥2 distributions are the same

########################################################################
# Other
########################################################################

# Chi-squared test
# Test whether factors are independent
obs_counts = xtabs(~age + sex)
summary(obs_counts)
# or
chisq.test(table(age, sex))

# Fisher's exact test
# Test whether factors are independent (2 factors i.e. 2x2 table)
# More accurate than chi-squared
fisher.test(obs_counts)

# Binomial test
# Test whether the true proportions of a factor with two levels is equal to some null hypothesis value p0
# for p = 0.5, null hypothesis assumes factors will have equal proportions (e.g. flipping coin)
binom.test(x, n, p = 0.5)

########################################################################
# Odds ratio
########################################################################

n <- sum(flip_sex_tab)
numerator <- (43 / n) / (30 / n) # P(short and f) / P(long and f)
denominator <- (19 / n) / (54 / n) # P(short and m) / P(long and m)
numerator / denominator

########################################################################
# Factors
########################################################################

# Create vector of characters for all combinations of gene and sex
# e.g. AA_m, AA_f
paste(gene, sex, sep = "_")

# Create vector of factor for all combinations of gene and sex
# e.g. AA_m, AA_f
age_sex <- factor(paste(age, sex, sep = "_"))
# Returns levels in factor age_sex
levels(age_sex)


detach(lab1_dat)
rm(list=ls())
