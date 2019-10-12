# Assignment
# Use the 2019Q2 SEC data to extract current quarterly revenues of U.S. based firms. Try to replicate the following methodology. We use the SEC Financial Statement Dataset to obtain the most current quarterly revenues of U.S. based SEC registrants. To distill total revenue from reported XBRL data, we take for each filing the maximum value of the three tags
# "Revenues"
# "RevenueFromContractWithCustomerExcludingAssessedTax"
# "RevenueFromContractWithCustomerIncludingAssessedTax"
# Produce a clean sample and report the sample size, the sample’s mean and the sample’s standard deviation.
# For extra credit: Prepare a map that shows where in the U.S. the corporate revenues are located.
# Data dictionary from: https://www.sec.gov/files/aqfs.pdf

library(tidyr)
library(dplyr)
library(readr)
library(tidylog)
library(stringr)

num_df <- read_csv("data/num.csv")
pre_df <- read_csv("data/pre.csv")
sub_df <- read_csv("data/sub.csv")
tag_df <- read_csv("data/tag.csv")  

# Extract current quarterly revenues of U.S. based firms
revenue <-
  sub_df %>%
  filter(
    countryba == "US",
    str_detect(fp, "Q")
  ) %>%
  select(adsh, cik, name, sic, fp, period, fye) %>%
  group_by(cik) %>%
  top_n(1, wt = period) %>%
  left_join(pre_df) %>%
  filter(
    stmt == "IS",
    tag == "Revenues" |
    tag == "RevenueFromContractWithCustomerExcludingAssessedTax" |
    tag == "RevenueFromContractWithCustomerIncludingAssessedTax"
  ) %>%
  distinct(adsh, tag, version, .keep_all = TRUE) %>%
  left_join(num_df) %>%
  filter(
    ddate == period,
    qtrs == 1L,
    is.na(coreg),
    uom == "USD",
    !is.na(value)
  ) %>%
  mutate(total_revenue = value)  %>%
  select(adsh, cik, name, sic, fp, fye, ddate, total_revenue) %>%
  group_by(adsh, cik, name, sic, fp, fye, ddate) %>%
  top_n(1, wt = total_revenue) %>%
  ungroup() %>%
  distinct(cik, name, sic, fp, fye, ddate, .keep_all = TRUE) %>%
  arrange(cik)

# Explore current quarterly revenues of U.S. based firms
nrow(revenue)

length(unique(revenue$cik)) == nrow(revenue)
table(revenue$ddate)

revenue_descriptives <- 
  revenue %>%
  select(-ddate, -cik) %>% 
  ExPanDaR::prepare_descriptive_table()

revenue_descriptives$df

revenue %>% arrange(-total_revenue)