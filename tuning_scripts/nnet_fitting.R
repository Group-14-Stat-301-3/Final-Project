# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(stacks)
library(discrim)

# Handle common conflicts
tidymodels_prefer()

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


#nnet model
nnet_model <- mlp(hidden_units = tune(),
                  penalty = tune()) %>%
  set_engine("nnet", trace = 0) %>%
  set_mode("regression")

# neural net workflow
nnet_workflow <- workflow() %>%
  add_model(nnet_model) %>%
  add_recipe(mlb_recipe)

# define possible values for neural net
nnet_grid <- grid_regular(
  hidden_units(),
  penalty(),
  levels = 5
)

# Tuning/fitting ----
nnet_tuned <- nnet_workflow %>% 
  tune_grid(
    resamples = mlb_folds,
    grid = nnet_grid,
    metrics = metric_set(rmse, mae),
    control = control_stack_grid()
  )
write_rds(nnet_tuned, "nnet_tuned.rds", "xz")



