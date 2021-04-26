# loading packages
library(tidyverse)
library(readr)
library(ggplot2)
library(psych)
# reading in training data
mlb_training <- read.csv(file = "training_data_mlb.csv")

# initial overview of data
This dataset consists of batting statistics of MLB player seasons of players with at least 100 plate appearances, in the 2019 and  2020 seasons.
The data was acquired from the mlb.com site 'Baseball Savant' and its custom leaderboard feature, and then certain features were removed for the purpose of modeling, such as player names and statistics that are used in formulas for other features.
The total dataset consists of 761 observations or player seasons, and the training dataset consists of 573 player seasons.
There are 14 features and one response variable, ops or On Base Plus Slugging Percentage.
Luckily, there is no missingness in this dataset.

# univariate investigation of ops, the response variable
summary(select(mlb_training, ops))
describe(mlb_training$ops)
ggplot(data = mlb_training) +
  geom_histogram(mapping = aes(x = ops), binwidth = 0.5)
ggplot(data = mlb_training, mapping = aes(x = ops)) +
  geom_boxplot() +
  coord_flip()
ggplot(mlb_training, aes(x = ops)) + 
  geom_density(fill = "grey60")
