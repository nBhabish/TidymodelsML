
# Load Libraries ----------------------------------------------------------

pacman::p_load(
  tidyverse, tidymodels,
  here, vip, janitor, 
  visdat, naniar, corrplot)


# Reading in the data -----------------------------------------------------

data(credit_data, package = "modeldata")

# Convert to tibble -------------------------------------------------------

credit_data <- as_tibble(credit_data)


# Clean Names -------------------------------------------------------------

credit_data <- credit_data %>% 
  janitor::clean_names()

credit_data

# Quick Gander ------------------------------------------------------------

credit_data %>% 
  skimr::skim()


# Preprocessing -----------------------------------------------------------

### Consists of three steps : recipe-prep-bake/juice

### 1. Setup variables

credit_recipe <- recipe(status ~ ., data = credit_data)
credit_recipe
summary(credit_recipe)


# Add steps ---------------------------------------------------------------

### Center and scale
credit_recipe <- credit_recipe %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric())

credit_recipe


# Skewness ----------------------------------------------------------------

get_skewness <- function(x){
  #Remove missing 
  x <- x[!is.na(x)]
  #Get mean 
  x_bar <- mean(x)
  #Get number of observations
  n <- length(x)
  #Calculate v
  v <- sum((x - x_bar)^2)/(n-1)
  # Calculate skewness
  skewness <- sum((x-x_bar)^3)/((n-1)*v^(3/2))
  return(skewness)
}


get_skewness(rnorm(100)) # symmetric data

get_skewness(rexp(100)) # exponential data


# Skewness on Credit data -------------------------------------------------

credit_data %>% 
  summarise(
    across(where(is.numeric), get_skewness)
  )
  

# Box-Cox Transformation for skewness -------------------------------------

credit_recipe <- 
  credit_recipe %>% 
  step_BoxCox(all_numeric())

credit_recipe


# Missing Data ------------------------------------------------------------


vis_miss(credit_data)


gg_miss_upset(credit_data)



# Impute for missing data -------------------------------------------------

credit_recipe <- credit_recipe %>% 
  step_modeimpute(all_nominal()) %>% 
  step_meanimpute(all_numeric())

credit_recipe



# Near zero ---------------------------------------------------------------

credit_recipe <- credit_recipe %>% 
  step_nzv(all_predictors())
credit_recipe



# Collinearity ------------------------------------------------------------

credit_data %>% 
  select_if(is.numeric) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  corrplot()

credit_recipe <- credit_recipe %>% 
  step_corr(all_numeric()) 
  
credit_recipe



# Dummy -------------------------------------------------------------------

credit_recipe <- credit_recipe %>% 
  step_dummy(all_nominal(), -status)

credit_recipe


# Order of our steps ------------------------------------------------------
tidy(credit_recipe)
tidy(credit_recipe, n = 1)


# Prep recipe -------------------------------------------------------------

credit_recipe <- credit_recipe %>% 
  prep(data = credit_data)
credit_recipe

tidy(credit_recipe)

tidy(credit_recipe, n = 3)


# Get data ----------------------------------------------------------------

### Juice

credit_recipe %>% 
  juice()

credit_recipe %>% 
  bake(credit_data)



