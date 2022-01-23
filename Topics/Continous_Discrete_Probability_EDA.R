## Question 1
df_Carriers <- read.csv("Carriers.csv")

D_Speed <- df_Carriers$Download.Speed
U_Speed <- df_Carriers$Upload.Speed
n_rows <- nrow(df_Carriers)

## For download speeds
(d_mean <- mean(D_Speed)) # Mean
(d_median <- median(D_Speed)) # Median
(d_sd_pop <- sqrt((n_rows-1)/n_rows) * sd(D_Speed)) # Population SD
(d_sd_samp <- sd(D_Speed)) # Sample SD

## For upload speeds
(u_mean <- mean(U_Speed)) # Mean
(u_median <- median(U_Speed)) # Median
(u_sd_pop <- sqrt((n_rows-1)/n_rows) * sd(U_Speed)) # Population SD
(u_sd_samp <- sd(U_Speed)) # Sample SD

## CV
(d_sd_samp/d_mean)
(u_sd_samp/u_mean)

## The standard deviation of upload speeds is lower; however, the CV of download
## speeds is lower. So download speeds have lower relative variation. The
## conflict is due to the difference in the means. The mean for the download 
## speed is roughly 3 times the mean for the upload speed.

## CV (upload speeds) = 51.83%
## CV (download speeds) = 39.68%

## SD (upload speeds) = 5.79 Mbps
## SD(download speeds) = 12.84 Mbps

####################################
########### Question 2 ##############
####################################

### Read the file into a dataframe
Dow <- read.csv("Dow.csv")

### Clean the data
Dow$Closing.Value <- as.numeric(stringr::str_replace(Dow$Closing.Value, ",", ""))

### The average return is over a period of time and it compounds, so we use Geometric
### Mean and not the Arithmetic Mean. The first step is to convert the closing prices to
### percentage returns. Then under the pysch package, use the geometric.mean function.

### Covert the variable to a time series object
timeseries <- ts(Dow$Closing.Value, frequency = 12, start = c(1950, 1))
timeseries # Just displaying it


### Find the Returns from the Closing prices
Returns <- (timeseries/stats::lag(timeseries, -1))-1

### Calculate 1 + Returns 
Growth_Factors <- 1 + Returns
Growth_Factors ## just displaying

### Calculate the Geometric Mean
Geometric_Mean_Rate <- round(100*(psych::geometric.mean(Growth_Factors)-1), digits = 2)

Geometric_Mean_Rate ## Just displaying

sprintf("The geometric mean return is %.2f%%.", Geometric_Mean_Rate)

hist(Returns) ## Plot a histogram of returns

## The histogram looks approximately normal maybe a little left-skewed. 

### Assess normality using QQPlot
qqnorm(Returns)
qqline(Returns)

## The normal probability plot shows slight departures from the stright line towards the
## beginning and the end.

###### Empirical Rule ########

mean <- mean(Returns)

stddev <- sd(Returns)

TotalSampleSize <- length(Returns)

One_SD_Range_Lower <- mean - 1*stddev
One_SD_Range_Upper <- mean + 1*stddev

Num_Within_One_SD <- length(which(Returns >= One_SD_Range_Lower & Returns <= One_SD_Range_Upper))
Percent_Within_One_SD <- 100*(Num_Within_One_SD/TotalSampleSize)

Two_SD_Range_Lower <- mean - 2*stddev
Two_SD_Range_Upper <- mean + 2*stddev

Num_Within_Two_SD <- length(which(Returns >= Two_SD_Range_Lower & Returns <= Two_SD_Range_Upper))
Percent_Within_Two_SD <- 100*(Num_Within_Two_SD/TotalSampleSize)

Three_SD_Range_Lower <- mean - 3*stddev
Three_SD_Range_Upper <- mean + 3*stddev

Num_Within_Three_SD <- length(which(Returns >= Three_SD_Range_Lower & Returns <= Three_SD_Range_Upper))
Percent_Within_Three_SD <- 100*(Num_Within_Three_SD/TotalSampleSize)

sprintf("%.2f%% of the observations lie within 1s from the mean.", Percent_Within_One_SD)
sprintf("%.2f%% of the observations lie within 2s from the mean.", Percent_Within_Two_SD)
sprintf("%.2f%% of the observations lie within 3s from the mean.", Percent_Within_Three_SD)

## Data adhere to the Empirical Rule.

### Create a boxplot
boxplot(Returns, horizontal = TRUE)$out

### Compute quartiles
Q1 <- quantile(Returns, 0.25)
Q2 <- quantile(Returns, 0.50)
Q3 <- quantile(Returns, 0.75)
IQR <- IQR(Returns, na.rm = TRUE)

### Interpretation of Quartiles:
sprintf("25%% of the observations are below %.2f%% and 75%% are above %.2f%%.", Q1*100, Q1*100)
sprintf("50%% of the observations are below %.2f%% and 50%% are above %.2f%%.", Q2*100, Q2*100)
sprintf("75%% of the observations are below %.2f%% and 25%% are above %.2f%%.", Q3*100, Q3*100)

### Getting ready to create a new dataframe containing time periods and Returns
newMonths <- Dow$Month[-1]
newMonthsdf <- as.data.frame(newMonths)

newMonthsdf$Returns <- Returns
str(newMonthsdf)

head(newMonthsdf)

### Creating the outlier vector which will hold outliers
outliervector <- with(newMonthsdf, newMonthsdf$Outlier <- ifelse(newMonthsdf$Returns >= Q3 + 1.5*IQR | newMonthsdf$Returns <= Q1 - 1.5*IQR, "Yes", "No"))

### Add the vector to the dataframe
newMonthsdf["Outlier"] <- outliervector

newMonthsdf ## Just displating

### Creating a vector for Extreme Outliers
extremeoutliervector <- with(newMonthsdf, newMonthsdf$Outlier <- ifelse(newMonthsdf$Returns >= Q3+ 3*IQR | newMonthsdf$Returns <= Q1 - 3*IQR, "Yes", "No"))

### Adding it to the dataframe
newMonthsdf["ExtremeOutlier"] <- extremeoutliervector

### Creating a vector for Mild Outliers
mildoutliervector <- with(newMonthsdf, newMonthsdf$Outlier <- ifelse(newMonthsdf$Outlier == "Yes" & newMonthsdf$ExtremeOutlier == "No", "Yes", "No"))

### Adding it to the dataframe
newMonthsdf["MildOutlier"] <- mildoutliervector

### View the final data frame
View_Data_FINAL <- newMonthsdf

### Order the information
View_Data_FINAL[order(newMonthsdf$Outlier, decreasing = TRUE), ]

####################################
########### Question 3 ##############
####################################

SchoolPerf <- read.csv("TestScores.csv")

## Remove the % signs
SchoolPerf$Pct.taking.SAT <- as.numeric(stringr::str_replace(SchoolPerf$Pct.taking.SAT, "%", ""))
SchoolPerf$Pct.taking.ACT <- as.numeric(stringr::str_replace(SchoolPerf$Pct.taking.ACT, "%", ""))

## Part 1
plot(SchoolPerf$Pct.taking.SAT, SchoolPerf$SAT.Total, main="SAT Correlation",
     xlab="Percent taking SAT", ylab="SAT Total", pch=19)
abline(lm(SchoolPerf$SAT.Total~SchoolPerf$Pct.taking.SAT), col="red")

## Correlation
cor(SchoolPerf$Pct.taking.SAT, SchoolPerf$SAT.Total)

## Part 1 Interpretation:
## The higher the percentage taking the exam, the lower the combined score.

## Part 2
plot(SchoolPerf$SAT.ERW, SchoolPerf$SAT.Math, main="MATH, ERW Correlation",
     xlab="ERW", ylab="Math", pch=19)
abline(lm(SchoolPerf$SAT.Math~SchoolPerf$SAT.ERW), col="red")

## Correlation
cor(SchoolPerf$SAT.ERW, SchoolPerf$SAT.Math)

## Part 2 Interpretation:
## Very high correlation (across states) between math and verbal scores.

## Part 3
plot(SchoolPerf$Pct.taking.ACT, SchoolPerf$ACT.Composite, main="ACT Correlation",
     xlab="Percent taking ACT", ylab="ACT Composite", pch=19)
abline(lm(SchoolPerf$ACT.Composite~SchoolPerf$Pct.taking.ACT), col="red")

## Correlation
cor(SchoolPerf$Pct.taking.ACT, SchoolPerf$ACT.Composite)

## Part 3 Interpretation:
## Again, the higher the percentage taking the exam, the lower the combined score.

## Part 4
df <- SchoolPerf[ ,7:10]

## Create a correlation matrix
round(cor(df), 3)

## Part 4 Interpretation:
## Very high correlations (across states) between English, Math, Reading, and Science scores.

####################################
########### Question 4 ##############
####################################

MortgageRates <- read.csv("MortgageRates.csv")

Date <- lubridate::mdy(MortgageRates$Date)
Mtg_30_Year <- MortgageRates$X30.year.Rate
Mtg_15_Year <- MortgageRates$X15.year.Rate

df_MortgageRates <- data.frame(Date, Mtg_30_Year, Mtg_15_Year)

## Using ggplot

df_long <- df_MortgageRates %>%
  select(Date, Mtg_30_Year, Mtg_15_Year) %>%
  pivot_longer(-Date, names_to = "variable", values_to = "value")

# Multiple line plot
ggplot(df_long, aes(x = Date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal() +
  xlab("Date") +
  ylab("Mortgage Rates %") +
  ggtitle("Mortgage Rates US (1991 - 2017)") + 
  theme(plot.title = element_text(hjust = 0.5))

## Using Base R
plot(Mtg_15_Year~Date, type="l" , col = "red", xlab = "Date", ylab = "Mortgage Rates")
lines(Mtg_30_Year~Date, type="l", col = "blue")
title(main = "Mortgage Rates US (1991 - 2017)")
legend("topright", legend = c("15-year mortgage", "30-year mortgage"),
       col = c("red", "blue"),
       lty = 1)

## Interpretation:
## The rates have declined gradually during this period. 
## The two rates vary together, with the 15-year rate slightly below the 30-year rate.

####################################
########### Question 5 ##############
####################################

Trans <- read.csv("SuperMarketTransactions.csv")

### Clean the data - remove the dollar signs from revenue and coerce State or Province to character
Trans$Revenue <- as.numeric(stringr::str_replace(Trans$Revenue, "\\$", ""))
Trans$State.or.Province <- as.character(Trans$State.or.Province)

## Part a
boxplot(Trans$Revenue~Trans$State.or.Province, main="Distribution of Revenue", 
        xlab="State or Province", ylab="Revenue")

## Part b
USData <- subset(Trans, Trans$Country == 'USA')

boxplot(USData$Revenue~USData$State.or.Province, main="Distribution of Revenue (USA only)", 
        xlab="State", ylab="Revenue")

## Part c
Trans %>%
  group_by(State.or.Province) %>%
  summarise(Total_Rev = sum(Revenue)) %>%
  arrange(desc(Total_Rev))

# OR

# Using apply function
(md <- tapply(Trans$Revenue, Trans$State.or.Province, sum))

x <- as.matrix(md, byrow = TRUE)
y <- as.matrix(x[order(x[ ,1], decreasing = TRUE), ])
colnames(y) <- "Total Revenue"
y

## Part d
Trans %>%
  group_by(Product.Category) %>%
  summarise(Total_Rev = sum(Revenue)) %>%
  arrange(desc(Total_Rev)) %>%
  print(n = Inf)

# OR

# Using apply function
(md1 <- tapply(Trans$Revenue, Trans$Product.Category, sum))

# Display the results in a matrix... NOT REQUIRED.
x1 <- as.matrix(md1, byrow = TRUE)
y1 <- as.matrix(x1[order(x1[ ,1], decreasing = TRUE), ])
colnames(y1) <- "Total Revenue"
y1

## Part e
Trans %>%
  group_by(State.or.Province) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq))

## Part f
total_row_count <- nrow(Trans) # Storing total row count for calculations in the future parts.

Trans %>%
  filter(Children > 1) %>%
  summarise(count = n(), pct = round(100*n()/total_row_count, 2))

# OR
(length(which(Trans$Children > 1))/total_row_count*100)

## Part g
Trans %>%
  mutate(PurchaseDate1 = as.Date(Purchase.Date, format = "%m/%d/%Y")) %>%
  mutate(month = strftime(PurchaseDate1, "%m")) %>%
  mutate(year = strftime(PurchaseDate1, "%Y")) %>%
  filter((month == "01" & year == "2018") | (month == "02" & year == "2018")) %>%
  group_by(month, year) %>%
  summarise("TotalRevenue" = sum(Revenue))

## Part h
table(Trans$Product.Family, Trans$Gender)

## Part i
(sum(Trans$Homeowner == "Y" & Trans$Marital.Status == "S")/total_row_count)

# OR

Trans %>%
  filter(Homeowner == "Y" & Marital.Status == "S") %>%
  summarise(count = n(), pct = 100*n()/total_row_count)

## Part j
Trans %>%
  group_by(Gender) %>%
  summarise(count = n(), pct = 100*n()/total_row_count) %>%
  arrange(desc(Gender))

####################################
########### Question 6 ##############
####################################

df_CellPhoneMarket <- read.csv("CellphoneMarket.csv")

# Create a correlation matrix (ignore the qualitative variables)
df <- df_CellPhoneMarket[ ,c(2, 5:18)]

## Create a correlation matrix
round(cor(df), 3)

## Observations from the correlation matrix
## The first thing to note is that all of these correlations are really small 
## except for the 1's in Day Charge and Day Minutes, Evening Charge and Evening Minutes,
## Night Charge and Night Minutes, International Charge and International Minutes. 
## Each pair of Charge/Minutes variables are perfectly correlated. 
## This indicates that each charge is per minute. 
## So if you want to see what is related to churn, you don't need to look at 
## the Charge variables; it suffices to look only at the Minutes variables.

## Create a histogram of quantitative variables to examine the distributions

## Account Length
hist(df_CellPhoneMarket$Account.Length)

## Voice Mail Messages
hist(df_CellPhoneMarket$Voice.Mail.Messages)

## Day Minutes
hist(df_CellPhoneMarket$Day.Minutes)

## Day Calls
hist(df_CellPhoneMarket$Day.Calls)

## Evening Minutes
hist(df_CellPhoneMarket$Evening.Minutes)

## Evening Calls
hist(df_CellPhoneMarket$Evening.Calls)

## Night Minutes
hist(df_CellPhoneMarket$Night.Minutes)

## Night Calls
hist(df_CellPhoneMarket$Night.Calls)

## International Minutes
hist(df_CellPhoneMarket$International.Minutes)

## International Calls
hist(df_CellPhoneMarket$International.Calls)

## Customer Service Calls
hist(df_CellPhoneMarket$Customer.Service.Calls)

## Bar plot for International Plan
counts_Int_Plan <- table(df_CellPhoneMarket$International.Plan)
barplot(counts_Int_Plan, main="International Plan",
        xlab="")

## Bar plot for Voicemail Plan
counts_VM_Plan <- table(df_CellPhoneMarket$Voice.Mail.Plan)
barplot(counts_VM_Plan, main="Voicemail Plan",
        xlab="")

## Bar plot for Churn
counts_Churn <- table(df_CellPhoneMarket$Churn.)
barplot(counts_Churn, main="Churn",
        xlab="")

## Find the proportion of those who chured
counts_Churn/length(df_CellPhoneMarket$Churn.)

## About 14% of customers churned.

## Observations from Histograms/Bar plots
## All of the histograms for continuous variables are fairly symmetric and 
## bell-shaped except for the Voice Mail Messages (big spike at 0 because a lot 
## of customers don't have voice mail), International Calls (skewed to the 
## right), and Customer Service Calls (skewed to the right). 
## But they are all pretty well behaved -- no wild outliers.

## To see which variables seem to affect the churn, for the solutions, 
## you can create two-way tables of each variable against churn and 
## examined the row %.
## Here are the insights.

## 1. Account Length has virtually no effect.
## 2. Customers with an international plan are almost four times as likely to 
##    churn than those without such a plan.
## 3. Customers with a voice mail plan are about half as likely to churn as 
##    those without such a plan.
## 4. Voice Mail Message has a small effect. It's more important to know who 
##    has a voice mail plan.
## 5. Customers with quite a few day minutes are considerably more likely to churn.
## 6. Day calls doesn't seem to have much effect.
## 7. Customers with more evening minutes are more likely to churn.
## 8. Evening calls doesn't seem to have much effect.
## 9. Customers with more night minutes are slightly more likely to churn.
## 10. Night calls doesn't seem to have much effect.
## 11. Customers with more international minutes are more likely to churn.
## 12. International calls doesn't seem to have much effect.
## 13. Customers with a lot of customer service calls are much more likely to churn.

## In short, customers who use a lot of minutes or have an international plan 
## are more likely to churn. These are evidently the customers who shop around 
## for good deals. Customers with a lot of customer service calls are evidently 
## unhappy with the service. If it is financially feasible, the company could 
## sweeten the deal for heavy/international users in some way. The company 
## could also try to improve its customer service.

####################################
########### Question 7 ##############
####################################

# Create numeric vectors for the random variable and their probabilities
value <- c(-8000, 0, 15000)
prob_vector <- c(0.273, 0.545, 0.182)

# Create a probability distribution
df_prob_distr <- data.frame(value, prob_vector)
names(df_prob_distr) <- c("Value", "Probability")
df_prob_distr # Just displaying to check

# Find the expected value
Expected_Value <- round(sum(value*prob_vector), digits = 3)

Expected_Value # Just displaying

# Find the variance
var1 <- (value - Expected_Value)^2
Variance <- round(sum(var1 * prob_vector), digits = 3)

Variance # Just displaying

# Determine the standard deviation
Std_Dev <- round(sqrt(Variance), digits = 3)

Std_Dev # Display

####################################
########### Question 8 ##############
####################################

## Part a
## Let X be the number of technicians called to repair the machine.
## Since the number of visits needed to fix the machine can be at most 4,
## X takes on the values x = 1, x = 2, x = 3, x = 4, x >= 5.

prob_fix <- 0.27
prob_fail <- 1-0.27

prob_first_fix <- prob_fix
prob_second_fix <- prob_fail_fix*prob_fix # P(1st failed)*P(second fixed)
prob_third_fix <- prob_fail*prob_fail*prob_fix # P(1st failed)*P(second failed)*P(third fixed)
prob_fourth_fix <- prob_fail*prob_fail*prob_fail*prob_fix # P(1st failed)*P(second failed)*P(third failed)*P(fourth fixed)
prob_greater_than_equal_5 <- 1 - (prob_first_fix + prob_second_fix + prob_third_fix + prob_fourth_fix)

## Probability Model
no_of_tech <- c(1, 2, 3, 4)
prob_vector <- c(prob_first_fix, prob_second_fix, prob_third_fix, prob_fourth_fix)

# Create a probability distribution
df_prob_distr <- data.frame(no_of_tech, prob_vector)
names(df_prob_distr) <- c("No_of_Tech", "Probability")
df_prob_distr # Just displaying to check

# Find the expected value
Expected_Value <- round(sum(no_of_tech*prob_vector), digits = 2)
Expected_Value # Just displaying

# Find the expected amount spent on the machine if a technician can fix the 
# problem. Then find the expected amount spend on the machine if a technician 
# does not fix the problem. Add the results to determine the expected amount 
# spent on the machine.
(Expected_Value*500) + (prob_greater_than_equal_5*7500)

####################################
########### Question 9 ##############
####################################

## Part a
dbinom(10, 20, 0.40)

## Part b
pbinom(10, 20, 0.40)

## Part c
1 - pbinom(14, 20, 0.40)

####################################
########### Question 10 ##############
####################################

## Part a
## Av. no. of calls in 1 hour
av_hour <- 400/16

## Av. no. of calls in 30 min
av_30_min <- av_hour/2

## Av. no. of calls in 15 min
av_15_min <- av_30_min/2

## Part b
dpois(6, 6.25)

## Part c
dpois(0, 6.25)

## Part d
1 - ppois(1, 6.25)

####################################
########### Question 11 ##############
####################################

## Original Sampling Plan

lot_size <- 1000
n <- 50
max_defective <- 4

defect_rates <- seq(0.02, 0.18, by = 0.02)
max_defective <- 4

names(defect_rates) = seq(0.02, 0.18, by = 0.02)
names(max_defective) = c("Prob_Acceptance")

Func_Prob_Accept <- function(defect_rates, max_defective){
  round(pbinom(max_defective, n, defect_rates, lower.tail = TRUE), digits = 4)
}

x <- outer(defect_rates, max_defective, Func_Prob_Accept)

y <- as.data.frame(x)

barplot(y$Prob_Acceptance, names.arg = c(seq(0.02, 0.18, by = 0.02)), ylim = c(0, 1.0), xlab = "Fraction Defective", ylab = "Probability of acceptance", main = "Sensitivity Analysis")

### Revised Sampling Plan

lot_size <- 1000
n <- 50
max_defective <- 5

defect_rates <- seq(0.02, 0.18, by = 0.02)
max_defective <- 5

names(defect_rates) = seq(0.02, 0.18, by = 0.02)
names(max_defective) = c("Prob_Acceptance")

Func_Prob_Accept <- function(defect_rates, max_defective){
  round(pbinom(max_defective, n, defect_rates, lower.tail = TRUE), digits = 4)
}

x <- outer(defect_rates, max_defective, Func_Prob_Accept)

y <- as.data.frame(x)
yv <- as.vector((y))

barplot(yv$Prob_Acceptance, names.arg = c(seq(0.02, 0.18, by = 0.02)), ylim = c(0, 1.0), xlab = "Fraction Defective", ylab = "Probability of acceptance", main = "Sensitivity Analysis")

## Observations: 
## 1. Probability of acceptance decreases as the fraction defective increases.
## 2. With the acceptance criteria increased from four or fewer to five or fewer.
##    the probability of acceptance increases for every fraction defective.

####################################
########### Question 12 ##############
####################################

num_years <- 47
num_mtrs_day <- 3000
total_mtrs <- 365*num_years*num_mtrs_day

prob_hitting <- 2/1000000000
mean <- total_mtrs*prob_hitting

prob_at_least_one <- 1 - dpois(0, mean)

