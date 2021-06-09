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
  step_log(b_bb_pct, barrel_batted_rate, oz_contact_pct, offset = 1e-16) %>%
  step_normalize(all_predictors())

# loading stacks and setting control grid
library(stacks)
ctrl_grid <- control_stack_grid()

# define model
rf_model <- rand_forest(mode = "regression",
                        mtry = tune(),
                        min_n = tune()) %>%
  set_engine("ranger", importance = "impurity")

# set up and define tuning grid
rf_params <-  parameters(rf_model) %>%
  update(mtry = mtry(range = c(2, 10)))
rf_grid <- grid_regular(rf_params, levels = 5)

# creating workflow
rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(mlb_recipe)

# tuning model
rf_tuned <- rf_workflow %>%
  tune_grid(resamples = mlb_folds,
            grid = rf_grid,
            control = ctrl_grid)

# saving results and workflow
save(rf_tuned, rf_workflow, file = "rf_tuning.rda")

load("rf_tuning.rda")

select_best(rf_tuned, metric = "rmse")
autoplot(rf_tuned, metric = "rmse")
rf_metrics <- collect_metrics(rf_tuned)

mlb_test <- read_csv('testing_data_mlb.csv')



