# loading packages
library(tidyverse)
library(tidymodels)
library(readr)
library(yardstick)
# loading training data
mlb_train <- read_csv('training_data_mlb.csv')
# setting seed
set.seed(2025)
# folding data and creating recipe
mlb_folds <- vfold_cv(data = mlb_train, v = 5, repeats = 3, strata = ops)
mlb_recipe <- recipe(ops ~ ., data = mlb_train) %>%
  step_rm(z_swing_miss_pct, oz_swing_miss_pct) %>%
  step_log(b_bb_pct, barrel_batted_rate, launch_angle_avg, oz_contact_pct) %>%
  step_normalize(all_predictors())