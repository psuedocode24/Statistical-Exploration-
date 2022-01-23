###############################################
#### Exercise 1 - ISlim #######################
###############################################

ISlim <- read.csv("ISlim.csv")

boxplot(ISlim$Supplier1, ISlim$Supplier2, names = c("Supplier 1", "Supplier 2"), horizontal = TRUE, main = "Boxplot for Two Suppliers", xlab = "Hours")

## These show that 
## (1) the variances in the number of hours do not appear to be the same, 
## (2) the mean for supplier 1 is somewhat greater than the mean for supplier 2, and 
## (3) there are several outliers. 
## There seems to be little doubt that supplier 1's motors will last longer on average than supplier 2's - or is there? 
## A confidence interval for the mean difference allows us to see whether the differences apparent in the 
## box plots can be generalized to all motors from the two suppliers. 

## Approach 1... using R functions

x <- t.test(ISlim$Supplier1, ISlim$Supplier2, var.equal = FALSE, paired = FALSE, conf.level = 0.95)

lcl <- x$conf.int[1]
ucl <- x$conf.int[2]

### Display 95% CI
sprintf("95 percent Confidence Interval: (%.2f, %.2f)", lcl, ucl)

## Approach 2... Using principles

mean_supplier1 <- mean(ISlim$Supplier1)
mean_supplier2 <- mean(ISlim$Supplier2)
diff_means <- mean_supplier1 - mean_supplier2

var_supplier1 <- var(ISlim$Supplier1)
var_supplier2 <- var(ISlim$Supplier2)

n_supplier1 <- length(ISlim$Supplier1)
n_supplier2 <- length(ISlim$Supplier2)

diff_means <- mean_supplier1 - mean_supplier2

numer <- ((var_supplier1/n_supplier1) + (var_supplier2/n_supplier2)) ^2
denom <- (((var_supplier1/n_supplier1)^2)/(n_supplier1 - 1)) + (((var_supplier2/n_supplier2)^2)/(n_supplier2 - 1))
df <- numer/denom

t95 <- qt(0.975, df = df, lower.tail = TRUE)

se <- sqrt((var_supplier1/n_supplier1) + (var_supplier2/n_supplier2))

l95 <- diff_means - t95*se
r95 <- diff_means + t95*se

l95_rounded <- round(l95, 2)
r95_rounded <- round(r95, 2)

### Display 95% CI
sprintf("95 percent Confidence Interval: (%.2f, %.2f)", l95_rounded, r95_rounded)

### Interpretation:
### Supplier 1's motors last on average from 47 hours fewer than to 233 hours greater than those from Supplier 2.

### What should ISlim do?
## Should ISlim continue with supplier 1? 
## This depends on the trade-off between the cost of the motors and warranty costs (and any other relevant costs). 
## Because the warranty probably depends on whether a motor lasts a certain amount of time, 
## warranty costs probably depend on a proportion (the proportion that fail before 500 hours, say) 
## rather than a mean. 

###############################################
#### Exercise 2 - Coupon Effectiveness ########
###############################################

## Read the data into R
CouponEffectivess <- read.csv("CouponEffectiveness.csv")

## How many of them received coupon?
n_received_coupon <- sum(CouponEffectivess$Received_coupon == "Yes")

## How many of them have no received coupon?
n_not_received_coupon <- sum(CouponEffectivess$Received_coupon == "No")

## How many of those who received the coupon purchased?
n_received_coupon_purchase <- sum(CouponEffectivess$Received_coupon == "Yes" & 
                                    CouponEffectivess$Purchased == "Yes")

## How many of those who did not receive the coupon yet purchased?
n_not_received_coupon_but_purchase <- sum(CouponEffectivess$Received_coupon == "No" & 
                                            CouponEffectivess$Purchased == "Yes")

## Sample proportion 1 = Num of people received coupon and purchased/num of people received coupon
p_1_hat <- n_received_coupon_purchase/n_received_coupon

## Sample proportion 2 = Num of people did not receive coupon but purchased/num of people not receive coupon
p_2_hat <- n_not_received_coupon_but_purchase/n_not_received_coupon

## Check large sample size conditions
n_received_coupon*p_1_hat
n_received_coupon*(1-p_1_hat)

n_not_received_coupon*p_2_hat
n_not_received_coupon*(1-p_2_hat)

## Run the test
prop.test(c(n_received_coupon_purchase, n_not_received_coupon_but_purchase), c(n_received_coupon, n_not_received_coupon), alternative = "two.sided", conf.level = 0.95, correct = TRUE)

####################################################################
#### Exercise 2 - Coupon Effectiveness ALTERNATIVE SOLUTION ########
####################################################################

z_critical_95 <- qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE)

margin_of_error <- z_critical_95*sqrt((p_1_hat*(1-p_1_hat)/n_received_coupon) + (p_2_hat*(1-p_2_hat)/n_not_received_coupon))

lcl <- (p_1_hat - p_2_hat) - margin_of_error
ucl <- (p_1_hat - p_2_hat) + margin_of_error

### Display 95% CI
sprintf("95 percent Confidence Interval: (%.2f, %.2f)", lcl, ucl)

### INTERPRETATION
### We see that 36.67% of customers who received a coupon purchased something, as opposed to only 23.33% of those who didn't receive a coupon. 
### The difference, 36.67% - 23.33% = 13.33% (or 0.1333), is the quantity of interest. 
### Specifically, the sample difference is 13.33%, and the objective is to find a confidence interval for this difference for the entire population.
### The confidence interval for the difference between proportions extends from 0.031 to 0.236 (all positive).
### So there is good reason to conclude that the proportion who purchase is larger for those who receive a coupon. 

###############################################
#### Exercise 3 - Presentation Ratings #########
###############################################
Ratings <- read.csv("PresentationRatings.csv")

y <- t.test(Ratings$Husband.rating, Ratings$Wife.rating, paired = TRUE, conf.level = 0.95)

lcl <- y$conf.int[1]
ucl <- y$conf.int[2]

### Display 95% CI
sprintf("95 percent Confidence Interval: (%.2f, %.2f)", lcl, ucl)

### Interpretation
### The 95% confidence interval for the difference between the ratings between husbands and wives
### extends from 1.06 to 2.20. Husbands rate the presentations 1.06 to 2.2 points higher than the wives.