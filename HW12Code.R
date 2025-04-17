################################################################################
# HW 12
# Avery Johnson
################################################################################

################################################################################
# Question 1
################################################################################
library(tidyverse)
library(VGAM)
library(effectsize)

# (a) t-val for statistically discernible support for t20
(val_t20 <- qt(0.95, df=19))

# (b) t-val for statistically discernible support for t30
(val_t30 <- qt(0.95, 29))

# (c) simulation to estimate type 1 error
simulations <- 10000
alpha <- 0.05

type_1_error_count <- 0

for (i in 1:simulations) {
  # generate data from Laplace
  data <- rlaplace(30, location=0, scale=4)
  
  # perform t test
  t20_result <- t.test(data[1:20], mu=0)
  t20 <- t20_result$statistic
  
  t30_result <- t.test(data, mu=0)
  t30 <- t30_result$statistic
  
  # check if we would reject the null at month 20
  if (t20 > val_t20 | t30 > val_t30) {
    type_1_error_count <- type_1_error_count + 1
  }
}

# estimate type 1 error rate
(type_1_error_rate <- type_1_error_count / simulations)

################################################################################
# Question 2
################################################################################

# (a) proportion of time we make a Type 1 error for left-tailed
# (b) proportion of time we make a Type 1 error for right-tailed
# (c) proportion of time we make a Type 1 error for two-tailed
# (d) how does skewness effect Type 1 error

