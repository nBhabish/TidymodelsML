# Loading configuartions --------------------------------------------------

pacman::p_load(tidyverse,
               tidymodels,
               mlbench)


# Setting seed ------------------------------------------------------------

set.seed(2021)

# Reading in data ---------------------------------------------------------

data(HouseVotes84, package = "mlbench")
head(HouseVotes84)


# Fitting model -----------------------------------------------------------

housevotes84_NB <- 
  naive_Bayes() %>% 
  set_engine("naivebayes") %>% 
  fit(Class ~ V5 + V9, data = HouseVotes84)
  
housevotes84_NB$fit$prior





# checking ----------------------------------------------------------------

HouseVotes84 %>% 
  count(Class) %>% 
  mutate(p = n/ sum(n))



housevotes84_NB$fit$tables$V5


HouseVotes84 %>% 
  count(Class, V5) %>% 
  group_by(Class) %>% 
  mutate(N = sum(n),
         p = n/ N)

 

# Laplace Smoothing -------------------------------------------------------



df <- tibble(class = factor(rep(LETTERS[1:2], each = 4)),
             x1 = factor(c("N", "N", "Y", "Y", "N", "N", "N", "N")),
             x2 = factor(c("a", "b", "b", "b", "b", "b", "a", "a")))


naive_Bayes() %>% 
  set_engine(naivebayes) %>% 
  fit(class ~ ., data = df) %>% 
  pluck("fit", "tables", "x1")


df %>% 
  group_by(class, x1) %>% 
  summarise(n = n()) %>% 
  complete(x1, fill = list(n = 0)) %>% 
  group_by(class) %>% 
  mutate(N = sum(n), 
         p = n/N)


# adding pseudocount with Laplace -----------------------------------------

naive_Bayes(Laplace = 1) %>% 
  set_engine("naivebayes") %>% 
  fit(class~., data = df) %>% 
  pluck("fit", "tables", "x1")

  
df %>% 
  group_by(class, x1) %>% 
  summarise(n = n()) %>% 
  complete(x1, fill = list(n = 0)) %>% 
  mutate(n = n+1) %>% 
  group_by(class) %>% 
  mutate(N = sum(n), 
         p = n/N)
