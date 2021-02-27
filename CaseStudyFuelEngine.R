
# Loading Libraries -------------------------------------------------------

pacman::p_load(
  tidyverse,
  tidymodels,
  AppliedPredictiveModeling,
  skimr,
  janitor,
  corrplot,
  vip
)


# Loading in Data ---------------------------------------------------------

data(FuelEconomy)
dim(cars2010)


# Convert to tibble -------------------------------------------------------


cars2010 <- as_tibble(cars2010) 

cars2010 <- cars2010 %>% 
  janitor::clean_names()

cars2010 <- cars2010 %>% 
  select(eng_displ,
         num_cyl,
         FE = fe,
         num_gears,
         drive_desc)

cars2010



# Recode the drive predictor ----------------------------------------------

cars2010 %>% 
  count(drive_desc)

cars2010 <- cars2010 %>% 
  mutate(drive = case_when(
    str_detect(drive_desc, "Front") ~ "front",
    str_detect(drive_desc, "Rear") ~ "rear",
    TRUE ~ "4WD"
  ))

cars2010 %>% 
  count(drive_desc, drive)

cars2010 <- cars2010 %>% 
  select(-drive_desc)

cars2010 <- cars2010 %>% 
  mutate_if(is.character, factor)
# if there's any column that's character we convert that to factor


# Split into TR and TE sets -----------------------------------------------

set.seed(2020)

car_split <- initial_split(cars2010, strata = FE) # Split in a way that they have the same amount of fuel efficiency

car_train <- training(car_split)
car_test <- testing(car_split)


# Exploratory Data Analysis -----------------------------------------------


# Overview  ---------------------------------------------------------------

skim(car_train)


# Look at Fuel Efficiency -------------------------------------------------

car_train %>% 
  ggplot(aes(FE))+
  geom_histogram(col = "black", fill = "orange")


# Bivariate Relationships -------------------------------------------------

car_train %>% 
  ggplot(aes(eng_displ, FE))+
  geom_point()


car_train %>% 
  ggplot(aes(factor(num_cyl), FE))+
  geom_boxplot()


car_train %>% 
  ggplot(aes(factor(num_gears), FE))+
  geom_boxplot()


car_train %>% 
  ggplot(aes(drive, FE))+
  geom_boxplot()



# Relationships between predictors ----------------------------------------

car_train %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot()



# Pre-processing with recipes ---------------------------------------------

car_recipe <- 
  recipe(FE ~ ., data = car_train) %>% 
  step_poly(eng_displ, degree = 2, options = list(raw = TRUE))
# Polynomial term added to engine displacement of degree 2, extra column
# engine displacement squared

# Taking your data and manipulating data and columns to get it in the 
# right form 

car_recipe  

car_recipe <- car_recipe %>% prep()

car_recipe

car_train <- car_recipe %>% juice()

car_test <- car_recipe %>% bake(car_test)

car_test



# Fit the model -----------------------------------------------------------


car_lm <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(FE ~ ., data = car_train)



# Use the model -----------------------------------------------------------



car_train %>% 
  add_column(
    predict(car_lm, new_data = car_train)
  ) %>% 
  yardstick::rmse(truth = FE, estimate = .pred)


car_test %>% 
  add_column(
    predict(car_lm, new_data = car_test)
  ) %>% 
  yardstick::rmse(truth = FE, estimate = .pred)



# Check assumptions -------------------------------------------------------

car_res <- augment(car_lm$fit)


car_res %>% 
  ggplot(aes(.fitted, .resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  labs(x = "Fitted",
       y = "Residual")


car_res %>% 
  ggplot(aes(sample = .resid))+
  stat_qq()+
  stat_qq_line()


car_res %>% 
  ggplot(aes(.hat, .std.resid))+
  geom_vline(size = 2, color = "white", xintercept = 0)+
  geom_hline(size = 2, color = "white", yintercept = 0)+
  geom_point(aes(size = .cooksd))+
  geom_smooth(se = FALSE)



# Variable Importance -----------------------------------------------------

vip(car_lm)



# Inference ---------------------------------------------------------------

tidy(car_lm$fit, conf.int = TRUE) %>% 
  arrange(p.value)

tidy(car_lm$fit, conf.int = TRUE) 




# Prediction --------------------------------------------------------------


# New data

new_data <- tibble(
  num_cyl = 4,
  num_gears = 6,
  eng_displ = 5,
  drive = "front"
)

new_data <- bake(car_recipe, new_data = new_data)

predict(car_lm, new_data = new_data)

predict(car_lm, new_data = new_data, type = "pred_int")

