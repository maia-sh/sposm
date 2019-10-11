# assignment
# Calculate weekly mean return and weekly return variance

# load packages
library(tidyr)
library(dplyr)
library(readr)
library(tidylog)


stocks <- read_csv("raw_data/stock_price.csv")

dates <- tibble()


returns <-
  stocks %>%
  mutate(return = (adj_prc - lag(adj_prc))/lag(adj_prc)) %>%
  summarise(mn_return = sum(return, na.rm = TRUE)/(nrow(.) + 20))

,
            var_return = sd(return, na.rm = TRUE)^2)

summarise(mn_return = mean(return, na.rm = TRUE),
          var_return = sd(return, na.rm = TRUE)^2)