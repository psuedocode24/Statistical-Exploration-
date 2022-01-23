library(dplyr)
library(stringr)

####################################
########### Question 1 ##############
####################################

rm(list = ls())

# set.seed(50)
winnings <- seq(0, 1000, by = 1)

### Theoretical Means and SD
(theor_mean <- mean(winnings))
(theor_var <- sum((winnings - theor_mean)^2)/1000)
(theor_sd <- sqrt(theor_var))
# (theor_sd <- (1000-0)/sqrt(12))

## Now sample
sample_size <- 5
no_of_replications <- 1000

rep_spinning <- replicate(no_of_replications, {
  samp <- sample(winnings, sample_size, replace = TRUE)
})

## Store the results in a matrix
matrix_Spin_Wheel <- matrix(rep_spinning, nrow = sample_size)

matrix_Spin_Wheel <- t(matrix_Spin_Wheel)

### Find the mean for each sample. 
### Since there are 1,000 samples, there will be 1,000 means
samp_means <- rowMeans(matrix_Spin_Wheel)

### Find the mean of all 1,000 Sample Means
sm_average <- mean(samp_means)

### Std Dev of of all 1,000 Sample Means = Standard Error
sm_sd <- sd(samp_means)

### Mean of sample means approaches the theoretical mean
### If the theoretical mean is unknown, then draw samples, 
### and find the mean of sample means.
### Mean of sample means approximately equal to the theoretical mean.
### Sample mean is an unbiased estimator of the population mean. 
theor_mean
sm_average

### Plot the histogram of sample means
### And compare with the original probability distribution.
### The original probability distribution = Uniform
### The sampling distribution of sample means = Normal or Approx. Normal
### Repeated sampling converts a uniform distribution to a Normal or Approx. Normal Distribution.
hist(samp_means, probability = FALSE)

### Theoretical Std. Error
theor_se <- theor_sd/sqrt(sample_size)

### Std Error from Simulation
sm_sd

### Compare the theoretical standard error with the std. Error from Simulation.
theor_se
sm_sd

## Find the probability of > $600
sum(samp_means > 600)/1000

pnorm(600, mean = theor_mean, sd = theor_se, lower.tail = FALSE)

####################################
########### Question 2 ##############
####################################

#### Part a
#### In terms of revenue, it is possible that there is less variability within 
### a product family and more variability across different product families. 
### This is exactly when stratified sampling is a good idea.

#### Parts b and c ######

Trans <- read.csv("SupermarketTrans.csv")

### Clean the data - remove the dollar signs from revenue and coerce State or Province to character
Trans$Revenue <- as.numeric(stringr::str_replace(Trans$Revenue, "\\$", ""))
Trans$State.or.Province <- as.character(Trans$State.or.Province)

Trans %>%
  group_by(Product.Family) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(reqdsample = freq*250)

### DRINK

Trans_str_Drink <- Trans %>%
  filter(Trans$Product.Family == 'Drink') %>%
  sample_n(size = 22)

mean(Trans_str_Drink$Revenue)
sd(Trans_str_Drink$Revenue)

### FOOD

Trans_str_Food <- Trans %>%
  filter(Trans$Product.Family == 'Food') %>%
  sample_n(size = 180)

mean(Trans_str_Food$Revenue)
sd(Trans_str_Food$Revenue)

### NON-CONSUMABLE
Trans_str_NC <- Trans %>%
  filter(Trans$Product.Family == 'Non-Consumable') %>%
  sample_n(size = 47)

mean(Trans_str_NC$Revenue)
sd(Trans_str_NC$Revenue)

####################################
########### Question 3 ##############
####################################

# Importing File. 
PaymentTimes <- read.csv("PaymentTimes.csv")

population_mean <- 19.5
sigma <- 4.2
n <- 65
x <- 18.1077
x_bar <- mean(PaymentTimes$PayTime)

### Population standard deviation is known. So, use a z.
### z-critical for 90%, 95%, and 99% CLs

### For 90% confidence... 
z90 <- qnorm(0.95, mean = 0, sd = 1, lower.tail = TRUE)

### For 95% confidence... 
z95 <- qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE)

### For 99% confidence... 
z99 <- qnorm(0.995, mean = 0, sd = 1, lower.tail = TRUE)

### 90% Confidence Interval
l90 <- x_bar - (z90*sigma/sqrt(n))
r90 <- x_bar + (z90*sigma/sqrt(n))

l90_rounded <- round(l90, digits = 3)
r90_rounded <- round(r90, digits = 3)

### 95% Confidence Interval
l95 <- x_bar - (z95*sigma/sqrt(n))
r95 <- x_bar + (z95*sigma/sqrt(n))

l95_rounded <- round(l95, digits = 3)
r95_rounded <- round(r95, digits = 3)

### 99% Confidence Interval
l99 <- x_bar - (z99*sigma/sqrt(n))
r99 <- x_bar + (z99*sigma/sqrt(n))

l99_rounded <- round(l99, digits = 3)
r99_rounded <- round(r99, digits = 3)

### Display 90% CI
sprintf("90 percent Confidence Interval: (%s, %s)", l90_rounded, r90_rounded)
sprintf("INTERPRETATION: We are 90 percent confident that the interval between %s days and %s days contains the true population mean payment times.", l90_rounded, r90_rounded)

### Display 95% CI
sprintf("95 percent Confidence Interval: (%s, %s)", l95_rounded, r95_rounded)
sprintf("INTERPRETATION: We are 95 percent confident that the interval between %s days and %s days contains the true population mean payment times.", l95_rounded, r95_rounded)

### Display 99% CI
sprintf("99 percent Confidence Interval: (%s, %s)", l99_rounded, r99_rounded)
sprintf("INTERPRETATION: We are 99 percent confident that the interval between %s days and %s days contains the true population mean payment times.", l99_rounded, r99_rounded)

### Part 1 - The 95% CI is 17.087 days and 19.129 days. Since the entire interval is below 19.5 days, 
### we can be 95% confident.

### Part 2 - The 99% CI is 16.766 days and 19.45 days. Since the entire interval is below 19.5 days, 
### we can be 99% confident.

### OR
### Solution using Upper Bound only...
### Students need to show only one - either two-sided CI or one-sided CI.
### Accept both for full credit as long as the conclusion that the billing 
### system is effective.

### For 90% confidence... 
z90_ub <- qnorm(0.90, mean = 0, sd = 1, lower.tail = TRUE)

### 90% Confidence Bound
r90_ub <- x_bar + (z90_ub*sigma/sqrt(n))
r90_rounded_ub <- round(r90_ub, digits = 3)

### Display 90% UCB
sprintf("90 percent Confidence Bound: %s", r90_rounded_ub)
sprintf("INTERPRETATION: We can estimate with 90 percent confidence that the mean payment time with the new system will be less than %s days. The new billing system will be effective.", r90_rounded_ub)

### For 95% confidence... 
z95_ub <- qnorm(0.95, mean = 0, sd = 1, lower.tail = TRUE)

### 95% Confidence Bound
r95_ub <- x_bar + (z95_ub*sigma/sqrt(n))
r95_rounded_ub <- round(r95_ub, digits = 3)

### Display 95% UCB
sprintf("95 percent Confidence Bound: %s", r95_rounded_ub)
sprintf("INTERPRETATION: We can estimate with 95 percent confidence that the mean payment time with the new system will be less than %s days. The new billing system will be effective.", r95_rounded_ub)

### For 99% confidence... 
z99_ub <- qnorm(0.99, mean = 0, sd = 1, lower.tail = TRUE)

### 99% Confidence Bound
r99_ub <- x_bar + (z99_ub*sigma/sqrt(n))
r99_rounded_ub <- round(r99_ub, digits = 3)

### Display 99% UCB
sprintf("99 percent Confidence Bound: %s", r99_rounded_ub)
sprintf("INTERPRETATION: We can estimate with 99 percent confidence that the mean payment time with the new system will be less than %s days. The new billing system will be effective.", r99_rounded_ub)

### Part 3 -
std_error_of_means <- sigma/sqrt(n)

Prob <- pnorm(x, population_mean, std_error_of_means, lower.tail = TRUE)
Prob

######################
##### Question 4 #####
######################

pexp(3, 1/2.7, lower.tail = TRUE)

#####################
##### Question 5 #####
#####################

mean <- 62
sd <- 2

prob_under_60 <- pnorm(60, mean, sd, lower.tail = TRUE)

prob_under_60

### P(under 60 exactly twice)
dbinom(2, 5, prob_under_60)

#####################
##### Question 6 #####
#####################

N <- 25
S <- 5
N-S <- 20
n <- 4
x <- 1

## Part a
round(dhyper(x, S, N-S, n), 4)

## Part b
round(1 - dhyper(0, S, N-S, n), 4)

#####################
##### Question 7 #####
#####################

mean <- 3

## Part a
round(dpois(0, 3), 4)

## Part b
## Atleast one sold
round(ppois(0, 3, lower.tail = FALSE)^5, 4)

#####################
##### Question 8 #####
#####################      

lambda <- 0.5 # mean events per unit of time

## Part a
part_a <- round(qexp(0.5, rate = lambda, lower.tail = TRUE), 2)
paste("The median waiting time until the next alarm is", part_a, "mins.")

## Part b
part_b <- round(qexp(0.25, rate = lambda, lower.tail = TRUE), 2)
paste("The first quartile of waiting time until the next alarm is", part_b, "mins.")

## Part c
part_c <- round(qexp(0.30, rate = lambda, lower.tail = TRUE), 2)
paste("The 30th percentile of waiting time until the next alarm is", part_c, "mins.")

#####################
##### Question 9 #####
#####################

## Done using simulation. Student approaches may vary.

num_samp <- 500 # Any value can be set here
num_mailed <- 150
original_non_resp_rate <- 0.45
new_non_rep_rate <- 0.70
min_resp_reqd <- 110
var_success <- 0

matrix <- matrix(NA, nrow = num_samp, ncol = 3)

for (i in 1:num_samp){
  matrix[i, 1] <- rbinom(1, num_mailed, 1-original_non_resp_rate)
  matrix[i, 2] <- rbinom(1, num_mailed-matrix[i, 1], 1-new_non_rep_rate)
  matrix[i, 3] <- matrix[i, 1] + matrix[i, 2]
  
  if(matrix[i, 3] >= min_resp_reqd) var_success <- var_success + 1
}

colnames(matrix) <- c("1st returns", "2nd returns", "Total")

matrix

var_success

frac_successes <- var_success/num_samp
frac_successes

####Question 10#####

##Part a
z_95 <- qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE)

## Mean = 20,000
mean <- 20000

## Sigma
sigma <- (30000-20000)/z_95

## Part b - Stock-out probabilities

## Order quantity = 15,000
pnorm(15000, mean = mean, sd = sigma, lower.tail = FALSE)

## Order quantity = 18,000
pnorm(18000, mean = mean, sd = sigma, lower.tail = FALSE)

## Order quantity = 24,000
pnorm(24000, mean = mean, sd = sigma, lower.tail = FALSE)

## Order quantity = 28,000
pnorm(28000, mean = mean, sd = sigma, lower.tail = FALSE)

## Part c - Profit Projections shown in Excel

## Project projections for suggested order quantities are presented in the Excel file.

## If unit sales are 10,000, the optimal order quantity is 15,000 for a profit 
## of 25,000. At all other order quantities when there are only 10,000-unit 
## sales, the company experiences a loss.

## When unit sales are 20,000 the optimal order quantity among quantities considered 
## is 18,000. Profit of 144,000 is greatest at this order quantity. 
## At 24,000 and 28,000 profits are 116,000 and 72,000, respectively.

## When unit sales are 30,000, the largest profit is at an order quantity of 
## 28,000. At a 28,000-order quantity there is also the potential for the 
## largest loss in profits if unit sales are as low as 10,000 (118,000).

## Part d
qnorm(0.70, mean = mean, sd = sigma, lower.tail = TRUE)

## At an order quantity of 22,675, the highest profit is realized if 30,000 units 
## are sold (181,128). More realistically, a 20,000-unit sales results in a 
## profit of 130,575 with excess units (2,675) sold at $5 after the season.

## Recommendation:
## Based on this analysis, the suggested order quantity is between 18,000 and 20,000. 
## At 18,000 production, the worst-case scenario of 10,000 sales results in a loss of 8000, 
## but a larger profit margin of 144,000 if all 20,000 are sold.
## Order quantity of 24,000 yield only a profit of 116,000 if all units are sold.
## Quantities produced at 70% of market demand (22,675) yield only 130,575 dollars 
## in profit if 20,000 units are sold.
