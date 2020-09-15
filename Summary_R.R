lab1_dat <- read.csv("C:/Users/User/Documents/KI/Biostatistics/data1.csv",
                     header = TRUE, sep = ";", stringsAsFactors = TRUE)
attach(lab1_dat)
#how many females?
sum(sex == "f")
#how many people with sexes?
length(sex)
#which are female (siffror index)?
which(sex == "f")
#what proportion are female?
sum(sex == "f")/length(sex)
mean(sex == "f")
#how old are the females?
age[sex == "f"]
# Compute the minimum and maximum age, separately in males and females.
tapply(age,sex, range) # tapply(to_what, by_what, what_function)
#male or female
length(sex == "f" | sex == "m")
#is everyone female?
all(sex == "f")


#frequentist probability

#when flipping a coin
coinflip <- sample(c("tail","head"), size=10000, replace=TRUE)
est_prob <- sum(coinflip=="head")/length(coinflip) #which is the same as
mean(coinflip == "head")
est_prob


#conditional probability
mean(smoke == 1)
mean(smoke == 0)
mean(gene == "AG")
mean(gene == "AA")
mean(gene == "GG")
#what is the probability that you are a smoker given that you have gene "AA" vs. gene "GG"
mean(smoke[gene=='AA']=='1') #which is the same as
tapply(smoke, gene == "AA", mean)
mean(smoke[gene=="GG"]=="1")
#How much does the chance of being a smoker change from general population to carrying the gene "AA"
mean(smoke[gene=='AA']=='1')/mean(gene == "AA")

#relative frequency

barplot(table(age)/length(age), ylab = "probability", xlab = "age")

#binomial distibution

#dbinom is a probability mass function of binomial distribution.
#dbinom <- P(X=x), probability of observing value equal to x.
#pbinom is a cumulative distribution function of this distribution. 
#pbinom <- P(X≤x), probability of observing value smaller or equal then x.

#what is the probability that 2 people from the dataset are smokers when randomly selecting 5?
mean(smoke) #gives the probability of a person being a smoker, 0.5
dbinom(2, size = 5, prob = 0.5)
#what is the probability of selecting 2 or fewer smokers out of 5 random picks?
dbinom(0, size = 5, prob = 0.5)+
dbinom(1, size = 5, prob = 0.5)+
dbinom(2, size = 5, prob = 0.5)
#or
pbinom(2, size = 5, prob = 0.5)
#2 or more smokers
pbinom(1, size = 5, prob = 0.5, lower = FALSE)
#roll 3 dice and let X be the number of 6s rolled. What is the distribution of X?
dist<- c(dbinom(0, size = 3, prob = 0.167),
dbinom(1, size = 3, prob = 0.167),
dbinom(2, size = 3, prob = 0.167),
dbinom(3, size = 3, prob = 0.167))
nroll <- c(0,1,2,3)
plot(dist ~ nroll, xlab = "number of sixes", ylab = "probability")
min(dist)
max(dist)
#roll 2 dice and let X be number of 1s rolled. what is pmf for X?
dbinom(0, size = 2, prob = 0.167)
dbinom(1, size = 2, prob = 0.167)
dbinom(2, size = 2, prob = 0.167)

#poisson distribution

###If there are twelve cars crossing a bridge per minute on average,
###find the probability of having seventeen or more cars crossing the bridge in a particular minute.
1 -ppois(16, lambda = 12)
#or
ppois(16, lambda = 12, lower = FALSE)
#what is the probability of 13 cars passing?
dpois(13, lambda = 12)

#Continuous uniform distribution

#Draw 100 random numbers between 1 and 3
runif(100, min = 1, max = 3)
barplot(runif(100, min = 1, max = 3))

#normal distribution

#rnorm generates a normally distributed dataset
normal_dat <- rnorm(1000, mean = 0, sd = 1)
plot(density(normal_dat))
#pnorm returns the integral from −∞ to q of the pdf of the normal distribution where q is a Z-score
#given parameters for pnorm(q, mean = μ, sd = σ)
#this says that about 99,8% of the data is below 3 z-scores from the mean
pnorm(3, mean = 0, sd = 1)
#this says that about 0,13% of the data is above 3 z-scores from the mean
pnorm(3, mean = 0, sd = 1, lower = F)
#95% conf interval
pnorm(1.96, mean = 0, sd = 1) - pnorm(1.96, mean = 0, sd = 1, lower = F)

# Calculate the probability that a normal random variable with mean 10
# and standard deviation 4 is between -2 and 2 or between 14 and 18.
# That is, the probability of the union of the events -2 < X < 2 and 14 < X < 18.
pnorm(2, mean = 10, sd = 4) - pnorm(-2, mean = 10, sd = 4) +
  pnorm(18, mean = 10, sd = 4) - pnorm(14, mean = 10, sd = 4)

#what percentage of the participants are older than 60?
mean(age)
sd(age)
pnorm(60, mean = 56, sd = 11.23487, lower.tail = FALSE)
#check to see if roughly same as
mean(age > 60)

#plotting

#histogram, 1 variable, frequency  
hist(age)
#Histogram of age of females (association of age with sex)
hist(age[sex == "f"]) #which is the same as
tapply(age, sex == "f",hist)
#barplot, 1 variable,age of each person in set
barplot(age)
#barplot of table, nr. of persons that have each age in set
barplot(table(age))
# Cut age into four equally sized intervals and do a barplot of the
# proportion of smokers in the 4 age groups
age_cat <- cut(age, 4)
barplot(tapply(smoke, age_cat, mean),
        xlab = "age",
        ylab = "smokers")

#Represent graphically the association between age and smoking.
# For example, you can do a barplot of proportion of patients who smoke in each age group
barplot(tapply(smoke, age, mean),
        ylab = "smoke",
        xlab = "age")
#boxplot, plots summary
summary(age)
boxplot(age)
#multiple variable boxplots
boxplot(age ~ sex)
#uses the data of a table consisting of
table(sex,age)
#scatterplot, 2 variables
plot(age,gene)
plot(age, gene, col = c("orange", "darkgreen","blue")[gene]) # plot them all
legend("topleft", legend = c("AA", "AG","AC"), fill = c("orange", "darkgreen","blue"))

#Functions

# confidence interval: confidence(%) that mu is in interval
t.test(age, conf.level = 0.95)
t.test(age, conf.level = 0.99)
t.test(age, conf.level = 1)
qnorm(0.025, lower = F)

#99% confidence interval without using t.test function
mean(lab4_dat$y1) + c(-1, 1) * qt(0.005, df = nrow(lab4_dat) - 1, lower = F) *
  sd(lab4_dat$y1) / sqrt(nrow(lab4_dat))

#models

#calculating using normally distributed models
var(sample)
mean(sample)

detach(lab1_dat)
rm(x)
rm(y)
rm(age_cat)
rm(b1cat)
rm(b2cat)
rm(coinflip)
rm(nroll)
rm(dist)
rm(est_prob)
