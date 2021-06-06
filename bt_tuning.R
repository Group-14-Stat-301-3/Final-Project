# loading packages ----
library(tidyverse)
library(tidymodels)

# loading data ----
load("bt_setup.rda")

# tune/fit ----
bt_tuned <- bt_workflow %>% 
  tune_grid(
    resamples = mlb_folds,
    grid = bt_grid,
    control = ctrl_grid
  )

# write out ----
save(bt_tuned, file = "bt_tuning.rda")