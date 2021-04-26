library(tidyverse)
library(cowplot)

training_data_mlb <- read_csv('training_data_mlb.csv')

# plot all variables by life expectancy, note any irregularities 
training_data_mlb %>%
  gather(-c(ops), key = "var", value = "value") %>%
  ggplot(aes(x = value, y = ops)) + geom_point() + geom_smooth() + facet_wrap(~var, scales = "free")

training_data_mlb %>%
  gather(-c(ops), key = "var", value = "value") %>%
  ggplot(aes(x = value)) + geom_histogram() + facet_wrap(~var, scales = "free")

knitr::kable(cor(training_data_mlb %>% select(-c(ops))))

