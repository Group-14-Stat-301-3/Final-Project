# svm rbf tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(stacks)

# loading training data
mlb_train <- read_csv('training_data_mlb.csv')
# setting seed
set.seed(2025)
# folding data and creating recipe
mlb_folds <- vfold_cv(data = mlb_train, v = 5, repeats = 3, strata = ops)

mlb_recipe <- recipe(ops ~ ., data = mlb_train) %>%
  step_rm(z_swing_miss_pct, oz_swing_miss_pct) %>%
  step_log(b_bb_pct, barrel_batted_rate, oz_contact_pct, offset = 1e-16) %>%
  step_normalize(all_predictors())

# Define model ----
svm_model <- svm_rbf(
  mode = "regression",
  cost = tune(),
  rbf_sigma = tune()
) %>%
  set_engine("kernlab")

# # check tuning parameters
# parameters(svm_model)

# set-up tuning grid ----
svm_params <- parameters(svm_model)

# define grid
svm_grid <- grid_regular(svm_params, levels = 5)

# workflow ----
svm_workflow <- workflow() %>%
  add_model(svm_model) %>%
  add_recipe(mlb_recipe)

# Tuning/fitting ----
svm_res <- svm_workflow %>%
  tune_grid(
    resamples = mlb_folds,
    grid = svm_grid,
    metrics = metric_set(rmse, mae),
    control = control_stack_grid()
  )

# Write out results & workflow
save(svm_res, file = "svm_res.rda")
