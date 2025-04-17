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
# gives t val at 95th percentile 
(val_t20 <- qt(0.95, df=19))

# (b) t-val for statistically discernible support for t30
# gives t val at 95th percentile 
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
  # do the t-stats exceed the corresponding critical values?
    # if they do, reject the null
  if (t20 > val_t20) {
    type_1_error_count <- type_1_error_count + 1
    # if this doesnt reject, then we check t30
  } else if (t30 > val_t30) {
    type_1_error_count <- type_1_error_count + 1
  }
}

# estimate type 1 error rate
(type_1_error_rate <- type_1_error_count / simulations)

################################################################################
# Question 2
################################################################################

simulations <- 10000
alpha <- 0.05
n <- 15

true_means <- c(10 / (10+2), 2 / (2+10), 10/(10+10))

# initialize counters
type_1_error_left <- 0
type_1_error_right<- 0
type_1_error_two_tailed <- 0

for (i in 1:simulations){
  # generate samples from Beta distributions
  data_beta1 <- rbeta(n, 10, 2)
  data_beta2 <- rbeta(n, 2, 10)
  data_beta3 <- rbeta(n, 10, 10)
  
  # conduct left tailed t test
  t_left_1 <- t.test(data_beta1, mu=true_means[1], alternative="less")
  t_left_2 <- t.test(data_beta2, mu=true_means[2], alternative="less")
  t_left_3 <- t.test(data_beta3, mu=true_means[3], alternative="less")
  
  # conduct right tailed t test
  t_right_1 <- t.test(data_beta1, mu=true_means[1], alternative="greater")
  t_right_2 <- t.test(data_beta2, mu=true_means[2], alternative="greater")
  t_right_3 <- t.test(data_beta3, mu=true_means[3], alternative="greater")
  
  # conduct two tailed t test
  t_two_1 <- t.test(data_beta1, mu=true_means[1], alternative="two.sided")
  t_two_2 <- t.test(data_beta2, mu=true_means[2], alternative="two.sided")
  t_two_3 <- t.test(data_beta3, mu=true_means[3], alternative="two.sided")
  
  # count type one errors
  type_1_error_left <- type_1_error_left + (t_left_1$p.value < alpha) +
                                          (t_left_2$p.value < alpha) +
                                          (t_left_3$p.value < alpha)
  
  type_1_error_right <- type_1_error_right + (t_right_1$p.value < alpha) +
                                             (t_right_2$p.value < alpha) +
                                             (t_right_3$p.value < alpha)
  
  type_1_error_two_tailed <- type_1_error_two_tailed + (t_two_1$p.value < alpha) +
                                                      (t_two_2$p.value < alpha)+ 
                                                      (t_two_3$p.value < alpha)
  
}

type_1_error_rate_left <- type_1_error_left / (simulations * 3)
type_1_error_rate_right <- type_1_error_right / (simulations * 3)
type_1_error_rate_two_tailed <- type_1_error_two_tailed / (simulations * 3)

# (a) proportion of time we make a Type 1 error for left-tailed
(type_1_error_rate_left <- type_1_error_left / (simulations * 3))

# (b) proportion of time we make a Type 1 error for right-tailed
(type_1_error_rate_right <- type_1_error_right / (simulations * 3))

# (c) proportion of time we make a Type 1 error for two-tailed
(type_1_error_rate_two_tailed <- type_1_error_two_tailed / (simulations * 3))

# (d) how does skewness effect Type 1 error
library(e1071)
skewness(rbeta(10000, 10, 2)) # should be left-skewed
skewness(rbeta(10000, 2, 10))  # right-skewed
skewness(rbeta(10000, 10, 10)) # symmetric


