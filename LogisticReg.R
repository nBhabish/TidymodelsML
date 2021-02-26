library(tidymodels)
default <- ISLR::Default

logistic_reg() %>% 
  set_engine("glm") %>% # generalized linear model
  fit(default ~ student+balance+income,
      data = default) %>% 
tidy()


# What if the customer is not a student?
# your x  = 0
