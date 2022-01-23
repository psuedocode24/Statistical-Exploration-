# Read the file. Independent samples.
Two_Samp_Indep <- read.csv("Comparison_Indep.csv")

# Check if the variances are equal.
var.test(Two_Samp_Indep$Finance, Two_Samp_Indep$Marketing, ratio = 1, alternative = "two.sided")

# Use equal variance t-test
t.test(Two_Samp_Indep$Finance, Two_Samp_Indep$Marketing, alternative = "greater", mu = 0, paired = FALSE)

# Sample size Finance
n_F <- length(Two_Samp_Indep$Finance)

# Sample size Marketing
n_M <- length(Two_Samp_Indep$Marketing)

# Numerator of the t-statistic
diff_means_indep_samp <- mean(Two_Samp_Indep$Finance) - mean(Two_Samp_Indep$Marketing)

# Variance in Finance
var_F <- var(Two_Samp_Indep$Finance)

# Variance in Marketing
var_M <- var(Two_Samp_Indep$Marketing)

# Pooled std dev
s_p <- sqrt((((n_F - 1)*var_F) +  ((n_M - 1)*var_M))/(n_F + n_M - 2))

format(s_p, big.mark = ",")

# Std Error
std_error_indep_samp <- sqrt(s_p^2*((1/n_M) + (1/n_F)))

# Test statistic
diff_means_indep_samp/std_error_indep_samp

# Read the file. Dependent (Paired) samples.
Two_Samp_Dep <- read.csv("Comparison_Dependent.csv")

# No. of pairs
n_pairs <- length(Two_Samp_Dep$Group)

# Calculate differences
Two_Samp_Dep$Difference <- Two_Samp_Dep$Finance - Two_Samp_Dep$Marketing

# Use paired t-test
t.test(Two_Samp_Dep$Finance, Two_Samp_Dep$Marketing, alternative = "greater", mu = 0, paired = TRUE)

# Mean and Variance of differences
diff_means_dep_samp <- mean(Two_Samp_Dep$Difference)

sd_dep_samp <- sd(Two_Samp_Dep$Difference)

format(sd(Two_Samp_Dep$Difference), big.mark = ",")

# Std Error
std_error_dep_samp <- sd_dep_samp/sqrt(n_pairs)

# Test statistic
diff_means_dep_samp/std_error_dep_samp
