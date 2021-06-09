# Load package(s) ----
library(tidymodels)
library(tidyverse)
library(stacks)

# Handle common conflicts
tidymodels_prefer()

# Load candidate model info ----
load("rf_tuning.rda")
load("svm_res.rda")
load("bt_tuning.rda")

# Load split data object & get testing data
train_mlb <- read_csv("training_data_mlb.csv")

test_mlb <- read_csv("testing_data_mlb.csv")

# Create data stack ----
mlb_stack <- stacks() %>%
  add_candidates(rf_tuned) %>%
  add_candidates(svm_res) %>%
  add_candidates(bt_tuned) 

# Fit the stack ----
# penalty values for blending (set penalty argument when blending)
blend_penalty <- c(10^(-6:-1), 0.5, 1, 1.5, 2, 2.5, 3)

# Blend predictions using penalty defined above (tuning step, set seed)
set.seed(9876)
mlb_model_stacked <- mlb_stack %>%
  blend_predictions(penalty = blend_penalty)

# Save blended model stack for reproducibility & easy reference (Rmd report)
save(mlb_model_stacked, file = "mlb_model_stacked" )

# Explore the blended model stack
autoplot(mlb_model_stacked)
autoplot(mlb_model_stacked, type = "members")
autoplot(mlb_model_stacked, type = "weights")

# fit to ensemble to entire training set ----
mlb_model_stacked_fitted <- mlb_model_stacked %>%
  fit_members()

# Save trained ensemble model for reproducibility & easy reference (Rmd report)
save(mlb_model_stacked_fitted, file = "mlb_model_stacked_fitted" )


# Explore and test trained ensemble model
test_mlb <- test_mlb %>%
  bind_cols(predict(mlb_model_stacked_fitted, .))

# plot predicted vs observed
ggplot(test_mlb, aes(x = ops, y = .pred)) + geom_point() + theme_minimal()

# get predictions based on member models
member_preds <- 
  test_mlb %>%
  select(ops) %>%
  bind_cols(predict(mlb_model_stacked_fitted, test_mlb, members = TRUE))

# display results
map_dfr(member_preds, rmse, truth = ops, data = member_preds) %>%
  mutate(member = colnames(member_preds))