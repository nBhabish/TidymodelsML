
# Loading configurations --------------------------------------------------

pacman::p_load(tidymodels,
               tidyverse)

# Reading in data ---------------------------------------------------------

auto <- ISLR::Auto
auto


# Splitting data ----------------------------------------------------------

auto_split <- initial_split(auto, strata = mpg)

auto_train <- training(auto_split)
auto_test <- testing(auto_split)


# Fitting model ------------------------------------------------------

auto_lm <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(mpg~horsepower + cylinders, data = auto_train)


# training data rmse ------------------------------------------------------

auto_train %>% 
  add_column(predict(auto_lm, new_data = auto_train)) %>% 
  yardstick::rmse(truth = mpg, estimate = .pred)



# testing data rmse -------------------------------------------------------

auto_test %>% 
  add_column(predict(auto_lm, new_data = auto_test)) %>% 
  yardstick::rmse(truth = mpg, estimate = .pred)


# k-Cross-validation --------------------------------------------------------

auto_cv <- vfold_cv(auto, v = 5, strata = mpg)



# Recipe ------------------------------------------------------------------

auto_recipe <- recipe(mpg~horsepower + cylinders, data = auto_train)
auto_recipe

# setting up model --------------------------------------------------------

auto_reg <- linear_reg() %>% 
  set_engine("lm")
auto_reg

# Setting up workflow -----------------------------------------------------

auto_wf <- workflow() %>% 
  add_recipe(auto_recipe) %>% 
  add_model(auto_reg)

# Run the cv --------------------------------------------------------------

auto_rsamples <- fit_resamples(auto_wf,
                          resamples = auto_cv)


# Collect metrics ---------------------------------------------------------

auto_rsamples %>% 
  collect_metrics()


# Checking each fold  -----------------------------------------------------

auto_rsamples$.metrics[[1]]
auto_rsamples$.metrics[[2]]
auto_rsamples$.metrics[[3]]
auto_rsamples$.metrics[[4]]
auto_rsamples$.metrics[[5]]
