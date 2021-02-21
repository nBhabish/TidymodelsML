# Loading Configurations
library(tidymodels)

# Loading dataset
auto <- ISLR::Auto

linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg ~ cylinders, data = auto) %>%
  tidy() # gives us a nice tibble of the output

# Look at the statistic value and interpret it
# We can interpret it as the sample slope is 24.4 standard errors below the the slope of zero
# How do we know this?
# the t-stat is the ratio of the estimate (i.e. beta coefficient) and std.error

# How do we know the statistic of 24.4 is meaningful?

linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg ~ cylinders, data = auto) %>%
  tidy(conf.int = TRUE)

# How are these statistics distributed under null hypothesis?

# null hypothesis says that the slope is zero

# n-p-1 degrees of freedom

# The distribution of test statistic we would expect given the null hypothesis is true
# , beta1 = 0, is t-distribution with n-p-1 degrees of freedom. p = # of predictors

# we will compare the t-statistic that we observed with the t-distribution under the null
# and see how likely it is to see the statistic that we saw

# p-value
# The probability of getting a statistic as extreme or more extreme than the observed test statistic
# given the null hypothesis is true


# We actually look at both sides of the p-value
# We will see how likely it is to see the right or left of the line


# probability of being left to the statistic that we observed

pt(1.5, df = 18)

pt(1.5, df = 18, lower.tail = FALSE) # proportion of area greater than 1.5 or less than 1.5 on one side

# proportion of area greater than 1.5 or less than 1.5 on both sides

pt(1.5, df = 18, lower.tail = FALSE) * 2

# 15% of the mass are as extreme or more extreme than this 1.5 value

# p-value of 0.15 means the probability of getting a statistic as extreme or more extreme than the
# 1.5 (observed test statistic) given the null hypothesis is true


# Confidence Interval
# if we use the same sampling method to select different samples and computed an interval estimate for each sample,
# we would expect the true population parameter (beta1) to fall within the interval estimates 95% of the time

# residual sum of squares: observed y value - predicted y value
# total sum of squares: observed y value - average of y value

# Assessing model fit
# 1. Residual sum of errors (RSE)
# 2. R^2 (R-squared) - the fraction of the variance explained


# what could be used to determine if atleast one predictor is useful?
# - we can use what's known as f-statistic


lm_fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg ~ wt, data = mtcars)


# all of statistic can be observed using this function below

glance(lm_fit$fit)











# Mtcars dataframe mpg and wt


mtcars <- datasets::mtcars

linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg ~ wt, data = mtcars) %>%
  tidy(conf.int = TRUE)

# Interpretation: This basically tells us that the sample slope is 6.49 standard errors below the slope of zero

# Interpretation of p-value: The p-value of 1.29 x 10^-10 means the probability of getting a test statistic
# as extreme or more extreme than -9.56 (observed test statistic) given the null hypothesis is true
# Since p-value is less than 0.05 we reject the null value
