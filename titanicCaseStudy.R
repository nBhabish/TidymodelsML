# Load Config -------------------------------------------------------------

pacman::p_load(tidymodels,
               tidyverse)


# Reading in data ---------------------------------------------------------

data("titanic_train", package = "titanic")
titanic <- as_tibble(titanic_train)
titanic


# Cleaning colnames -------------------------------------------------------

titanic <- titanic %>% 
  janitor::clean_names()


# Coverting cols to factor -------------------------------------------------

titanic <- titanic %>% 
  mutate_all(factor) 

titanic



# Splitting dataset -------------------------------------------------------

titanic_split <- initial_split(titanic, strata = survived)
titanic_train <- training(titanic_split)
titanic_test <- testing(titanic_split)

titanic_test
titanic_train


# Get CV of training data -------------------------------------------------

titanic_cv <- vfold_cv(titanic_train, v = 5, strata = survived)

titanic_cv



# Setting up workflow -----------------------------------------------------

titanic_recipe <- recipe(survived ~ sex + pclass, data = titanic_train) %>%
  step_dummy(all_predictors()) %>% 
  step_interact(terms = ~starts_with("sex"):starts_with("pclass"))



# Quick gander ------------------------------------------------------------

titanic_recipe %>% prep %>% juice



# Setting up model -----------------------------------------------------------

titanic_model <- logistic_reg() %>% 
  set_engine("glm")


# Workflow ----------------------------------------------------------------

titanic_wf <- workflow() %>% 
  add_recipe(titanic_recipe) %>% 
  add_model(titanic_model)
titanic_wf


# Fitting the model in data -----------------------------------------------

titanic_fit <- fit(titanic_wf, titanic_train)
titanic_fit %>% 
  tidy()



# Fit CV ------------------------------------------------------------------

titanic_rsamples <- fit_resamples(
  titanic_wf,
  resamples = titanic_cv,
  control = control_resamples(save_pred = TRUE))

titanic_rsamples


# Get metrics -------------------------------------------------------------

titanic_rsamples %>% collect_metrics()


# ROC curves --------------------------------------------------------------

CV_ROC <- titanic_rsamples %>% 
  collect_predictions() %>% 
  group_by(id) %>% 
  roc_curve(truth = survived, estimate = .pred_1, event_level = "second")

CV_ROC %>% autoplot()
