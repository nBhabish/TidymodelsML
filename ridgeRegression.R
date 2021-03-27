
# The Office - Ridge Regression -------------------------------------------
# Objective: Predict imdb rating by how many line each character has --------
# Loading configurations ----------------------------------------------------------

pacman::p_load(tidymodels,
               tidyverse, 
               glmnet,
               schrute,
               plotly,
               vip,
               paletteer,
               tidytuesdayR)



# Read in data ------------------------------------------------------------


data("theoffice", package = "schrute")
theoffice <- theoffice
theoffice



# Characters --------------------------------------------------------------

theoffice %>% 
  distinct(character) %>% 
  nrow()

# Collapsing down

theoffice <- theoffice %>% 
  mutate(character = fct_lump_n(character, 20))

theoffice %>% 
  distinct(character) %>% head(20)


# Cleaning ----------------------------------------------------------------

## first count the number of lines for each character ----------------------

theoffice_long <- theoffice %>% 
  group_by(season, episode, character) %>% 
  summarise(
    n_lines = n(),
    rating = mean(imdb_rating)
  )


## Convert the long data into wide -----------------------------------------


theoffice_wide <- theoffice_long %>% 
  pivot_wider(names_from = character, values_from = n_lines, values_fill = 0)

theoffice_wide


# Split -------------------------------------------------------------------

theoffice_split <- initial_split(theoffice_wide, strata = rating)

theoffice_train <- training(theoffice_split)
theoffice_test <- testing(theoffice_split)

# Preprocessing -----------------------------------------------------------

theoffice_recipe <- recipe(
  rating ~ ., data = theoffice_train
)

theoffice_recipe

# Set ID columns ----------------------------------------------------------

theoffice_recipe <- theoffice_recipe %>% 
  update_role(season, episode, new_role = "ID")
theoffice_recipe


# Scale and center --------------------------------------------------------

theoffice_recipe <- theoffice_recipe %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())

theoffice_recipe



# Check -------------------------------------------------------------------

theoffice_recipe %>% 
  prep() %>% 
  juice()

theoffice_recipe %>% 
  prep() %>% 
  juice() %>% 
  select(rating)


# Fit ridge regression ----------------------------------------------------

theoffice_model <- linear_reg(mixture = 0, penalty = 0.1) %>% 
  set_engine("glmnet")

theoffice_model

## Creating workflow -------------------------------------------------------

theoffice_wf <- 
  workflow() %>% 
  add_recipe(theoffice_recipe) %>% 
  add_model(theoffice_model)


## Fit workflow ------------------------------------------------------------

theoffice_ridge <- 
  theoffice_wf %>% 
  fit(data = theoffice_train)

theoffice_ridge %>% 
  yardstick::tidy() 

p <- theoffice_ridge %>% 
  pull_workflow_fit() %>%
  pluck("fit") %>% 
  tidy() %>% 
  filter(term != "(Intercept)",
         lambda < 100) %>% 
  ggplot(aes(lambda, estimate, col = term))+
  geom_line()


ggplotly(p)


# Predict -----------------------------------------------------------------
## Train RMSE --------------------------------------------------------------

theoffice_train %>% 
  add_column(predict(theoffice_ridge, new_data = theoffice_train)) %>% 
  ungroup() %>% 
  rmse(rating, .pred)



## Test RMSE ---------------------------------------------------------------
theoffice_test %>% 
  add_column(predict(theoffice_ridge, new_data = theoffice_test)) %>% 
  ungroup() %>% 
  rmse(rating, .pred)

## Comment: We would expect mean square error for training to be less than mean square error for testing


# VI plot -----------------------------------------------------------------

theoffice_ridge %>% 
  pull_workflow_fit() %>% 
  vi() %>% 
  ggplot(aes(Importance, fct_reorder(Variable, Importance), fill = Sign))+
  geom_col()+
  labs(y = "Character")+
  scale_fill_paletteer_d("dutchmasters::milkmaid")


# Comment from the VI plot: The more Michael delivers the line, the higher the ratings. On the other end, the more Nelie talks, the more likely for the ratings to drop.



