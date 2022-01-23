##### Example 1 ######

Example1 <- read.csv("Example1.csv")

var.test(Example1$Wendys, Example1$McDonalds, ratio = 1, alternative = "two.sided")

### Variances are equal, so run Equal Variance t-test ###
t.test(Example1$Wendys, Example1$McDonalds, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE)

##### Example 2 #####

### tsum.test is found in PASWR2 package and can be used when not dataset but summaries for two samples are given. 

install.packages("PASWR2")
library(PASWR2)

tsum.test(mean.x = 42.0, s.x = 2.3, n.x = 10, mean.y = 45.5, s.y = 1.8, n.y = 10, alternative = "less", var.equal = TRUE)

##### Example 3 #####

Example3 <- read.csv("Example3.csv")

t.test(Example3$Company1, Example3$Company2, alternative = "greater", mu = 0, paired = TRUE)

#### Example 4 #####

Example4 <- read.csv("Example4.csv")

var.test(Example4$Portfolio2, Example4$Portfolio1, ratio = 1, alternative = "greater")

#### OR ####

var.test(Example4$Portfolio1, Example4$Portfolio2, ratio = 1, alternative = "less")

#### Example 5 #####

Example5 <- read.csv("Example5.csv")

### Create a cross-tab table first to determine frequencies.
t <- table(Example5$Group, Example5$BuySpX)

### Add margin totals to the crosstab 
addmargins(t)

### Run a two-sample prop.test. 
### Supply two vectors.
### One vector is the number of successes. Success being defined as being health conscious and buy for the first sample.
### and No Health Conscious and Buy for the second sample.
### Second vector is the total no. of health conscious for the first sample and total no. of no health conscious for the second sample.
### Make sure you pay attention to the sign in the alternative hypothesis. 
prop.test(c(32, 56), c(231, 619), alternative = "greater", correct = FALSE)

### OR
buy.healthy <- length(which(Example5$Group == 1 & Example5$BuySpX == 2))
buy.nothealthy <- length(which(Example5$Group == 2 & Example5$BuySpX == 2))

num_buy <- c(buy.healthy, buy.nothealthy)

healthy.total <- length(which(Example5$Group == 1))
nothealthy.total <- length(which(Example5$Group == 2))

group_total <- c(healthy.total, nothealthy.total)

prop.test(num_buy, group_total, alternative = "greater", correct = FALSE)
