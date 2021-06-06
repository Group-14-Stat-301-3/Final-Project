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
bt_model <- boost_tree(mode = "regression",
                       mtry = tune(),
                       min_n = tune(),
                       learn_rate = tune()) %>% 
  set_engine("xgboost", importance = "impurity")

# set up and define tuning grid
bt_params <-  parameters(bt_model) %>%
  update(mtry = mtry(range = c(2, 10))) %>% 
  update(learn_rate = learn_rate(range = c(-5, -.2)))
bt_grid <- grid_regular(bt_params, levels = 5)

# creating workflow
bt_workflow <- workflow() %>%
  add_model(bt_model) %>%
  add_recipe(mlb_recipe)

save(mlb_folds, bt_workflow, bt_grid, ctrl_grid, file = "bt_setup.rda")

load("bt_tuning.rda")

show_best(bt_tuned, metric = "rmse")
autoplot(bt_tuned, metric = "rmse")
bt_metrics <- collect_metrics(bt_tuned)
