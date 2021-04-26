library(tidyverse)
library(tidymodels)
library(psych)

mlb_train <- read_csv("training_data_mlb.csv")

# Univariate investigation of potential important predictors

# First - exit velocity. We'd probably expect the harder a player hits,
# the more likely they are to 1. get on base and 2. hit for extra bases, 
# which would increase their OPS

mlb_train %>% 
  ggplot(aes(exit_velocity_avg)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  labs(x = "Average Exit Velocity", y = "Count")

# Here we see a distribution that's slightly left skewed, 
# with a mean and median likely just under 90. We can check
# those:

mlb_train %>% select(exit_velocity_avg) %>% 
  summary()

describe(mlb_train$exit_velocity_avg)

# Another potential important predictor - launch angle
# Knowing kinematics and projectile motion, in a vacuum we'd
# expect 45 degrees to be the optimal launch angle. But due to 
# wind/air resistance (and some domain knowledge we have), the 
# optimal launch angle is likely closer to 20-25 degrees.

mlb_train %>% 
  ggplot(aes(launch_angle_avg)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  labs(x = "Average Launch Angle", y = "Count")

mlb_train %>% select(launch_angle_avg) %>% 
  summary()

describe(mlb_train$launch_angle_avg)

# Something that initially surprised me: average launch angles 
# at < 0 degrees. But I realized that ground balls have a 
# launch angle < 0, so the handful of batters below that line
# are more likely to hit ground balls.

# Something less surprising, but probably more informative: 
# very few players average over 20 degree launch angles. 

# Notes - we should look at comparisons of both of these across age,
# as we may see some interesting trends

# Final important predictor - hard hit percentage
# We'd expect a player that hits hard more often to have a higher
# OPS

mlb_train %>% 
  ggplot(aes(hard_hit_pct)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  labs(x = "Hard Ht Percentage", y = "Count")

mlb_train %>% select(hard_hit_pct) %>% 
  summary()

describe(mlb_train$hard_hit_pct)

# Some potentially extraneous predictors, but also might
# have a strong impact - in zone contact, solid contact, barrels

mlb_train %>% select(iz_contact_pct) %>% 
  summary()

mlb_train %>% select(solidcontact_pct) %>% 
  summary()

mlb_train %>% select(barrel_batted_rate) %>% 
  summary()
