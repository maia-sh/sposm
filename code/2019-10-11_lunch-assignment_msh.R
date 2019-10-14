# Assignment
# Calculate weekly mean return and weekly return variance
# From topic 6 slide 9

library(tidyr)
library(dplyr)
library(readr)
library(tidylog)

# Buggy code
read_csv("raw_data/stock_price.csv") %>%
  mutate(return = (adj_prc - lag(adj_prc))/lag(adj_prc)) %>% 
  summarise(mn_return = mean(return, na.rm = TRUE),
            var_return = sd(return, na.rm = TRUE)^2)

# Fixed code
stocks <- read_csv("raw_data/stock_price.csv")

dates <- tibble(date = seq(min(stocks$date), max(stocks$date), by = 'weeks'))

stocks_full <- dates %>% left_join(stocks, by = "date")

stocks_full %>%
  mutate(return = (adj_prc - lag(adj_prc))/lag(adj_prc)) %>%
  summarise(mn_return = mean(return, na.rm = TRUE),
            var_return = sd(return, na.rm = TRUE)^2)