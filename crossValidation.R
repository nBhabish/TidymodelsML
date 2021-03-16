
# Loading configurations --------------------------------------------------

pacman::p_load(tidymodels,
               tidyverse,
               discrim)



# setting seed ------------------------------------------------------------

set.seed(2021)

# Reading in data ---------------------------------------------------------

election_dat <- read_csv("~/Desktop/Repositories/Election Analysis/40StatesElectionAndProductionData.csv")

election_dat <- as.tibble(election_dat)

election_dat <- election_dat %>% 
  drop_na(Soybean.2019)


# Splitting data ----------------------------------------------------------

election_split <- initial_split(election_dat, strata = Soybean.2019)

election_split %>% 
  glimpse()

election_train <- training(election_split)
election_test <- testing(election_split)

election_lm <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(Soybean.2019~Corn.2019 + Median.Household.Income, data = election_train)
 


# training data rmse --------------------------------------------------------------


election_train %>% 
  add_column(predict(election_lm, new_data = election_train)) %>% 
  yardstick::rmse(truth = Soybean.2019, estimate = .pred)


# testing data rmse ---------------------------------------------------------------

election_test %>% 
  add_column(predict(election_lm, new_data = election_test)) %>%
  yardstick::rmse(truth = Soybean.2019, estimate = .pred)
  
  
###Validation estimate of test error rate can be highly variable


# Cross-validation --------------------------------------------------------

election_cv <- vfold_cv(election_dat, v = 5, strata = Soybean.2019)
election_cv


# Creating recipe ---------------------------------------------------------

election_recipe <- recipe(Soybean.2019 ~ Corn.2019 + Median.Household.Income, data = election_train)
election_recipe

# Setting up model --------------------------------------------------------

election_model <- linear_reg() %>% set_engine("lm")
election_model


# Setting up a workflow ---------------------------------------------------

election_wf <- workflow() %>% 
  add_recipe(election_recipe) %>% 
  add_model(election_model)
election_wf



# Perform cross-validation ------------------------------------------------

election_rsamples <- fit_resamples(
  election_wf,
  resamples = election_cv
)



# Get results -------------------------------------------------------------

election_rsamples %>% 
  collect_metrics()


# Check -------------------------------------------------------------------

election_rsamples$.metrics[[1]]


# Hand-code for check -----------------------------------------------------

train_1 <- analysis(election_cv$splits[[1]])
test_1 <- assessment(election_cv$splits[[1]])


model_1 <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(Soybean.2019 ~Corn.2019 + Median.Household.Income, data = train_1)


test_1 %>% 
  add_column(predict(model_1, new_data = test_1)) %>% 
  yardstick::rmse(truth = Soybean.2019, estimate = .pred)
