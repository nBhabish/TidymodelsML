
# Loading config ----------------------------------------------------------

pacman::p_load(tidymodels,
               tidyverse)


# Reading in data ---------------------------------------------------------
df <- ISLR::Auto

df %>% 
  glimpse()

# bootstrapping -----------------------------------------------------------

df_bs <- bootstraps(df, times = 1000)

df_bs


# Creating function -------------------------------------------------------

get_median <- function(split) {
  df <- analysis(split)
  return(tibble(
    term = "Median",
    estimate = median(df$mpg),
    std.error = NA
  ))
}


# Apply to each sample ----------------------------------------------------

df_bs <- df_bs %>% 
  mutate(median = map(splits, get_median))


# Getting medians ---------------------------------------------------------


T_obs <- median(df$mpg)
T_strap <- df_bs %>% unnest(median) %>% pull(estimate)
T_strap[1:10]


# Percentile --------------------------------------------------------------

int_pctl(df_bs, median)

