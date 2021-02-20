# Validation Set

# Randomly divide the available set up samples into two parts: a training set and a validation set
# Fit the model on the training set and estimate the prediction error in the validation set

# For quantitative predictor, we use Mean Squared Error as our metric to calculate error
# For qualitative predictor, we use Misclassification Rate as our metric to calculate error

# k-fold cross-validation
library(tidymodels)
library(tidyverse)

# Step 1: Specify the model
# Pick the model
# Set the engine

linear_reg() %>%  #Instead of linear_reg you can use any models that you want to use
  set_engine("lm")


# Other examples of how you can change engine and models
linear_reg() %>%
  set_engine("glmnet")


decision_tree() %>%
  set_engine("ranger")

linear_reg() %>%
  set_engine("spark")



# Linear reg model specification
lm_spec <- linear_reg() %>%
  set_engine("lm")

lm_spec


# Fit the data

library(ISLR)

auto <- ISLR::Auto
auto %>%
  glimpse()

lm_fit <- fit(lm_spec,
              mpg ~ horsepower,
              data = auto)

lm_fit

# intercept = 39.9359 and beta coefficient = -0.1578

lm(mpg ~ horsepower, data = auto)


# Get Predictions

mpg_pred <- lm_fit %>%
  predict(new_data = auto) %>%
  bind_cols(auto) # Binding the predicted data to my original dataset and comparing with mpg data

# Look at the actual miles per gallon and predicted miles per gallon and compare

# Calculate the error

mpg_pred %>%
  rmse(truth = mpg, estimate = .pred)

# Whatever the estimate we got, this was training error. It wasn't the testing error


# Cross-validation
# Splitting the dataset into training and testing data

auto_split <- initial_split(auto, prop = 0.5) # Here we are doing a 50-50 split
auto_split


auto_train <- training(auto_split)
auto_test <- testing(auto_split)

# Estimate the error in the testing dataset
set.seed(100)

auto_fit <- lm_spec %>% 
  fit(mpg~horsepower, data = auto_train)

mpg_pred <- auto_fit %>% 
  predict(new_data = auto_test) %>% 
  bind_cols(auto_test)

mpg_pred %>% 
  rmse(truth = mpg, estimate = .pred)


# A faster way to deal with this

auto_split <- initial_split(data = auto, prop = 0.5) 
lm_fit <- last_fit(lm_spec, 
                   mpg~horsepower,
                   split = auto_split)

# Here the main thing is last_fit, if you specify the split, this will automatically train the data in the training data


lm_fit %>% 
  collect_metrics()

# collect_metrics() will automatically calculate the error from test data 

# What about the cross-validation?

auto_cv <- vfold_cv(auto, v = 5)

results <- fit_resamples(lm_spec,
              mpg~horsepower,
              resamples = auto_cv) 

# How do we get the metrics out
results %>% 
  collect_metrics()



