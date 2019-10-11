# assignment
# Use the 2019Q2 SEC data to extract current quarterly revenues of U.S. based firms. Try to replicate the following methodology. We use the SEC Financial Statement Dataset to obtain the most current quarterly revenues of U.S. based SEC registrants. To distill total revenue from reported XBRL data, we take for each filing the maximum value of the three tags
# "Revenues"
# "RevenueFromContractWithCustomerExcludingAssessedTax"
# "RevenueFromContractWithCustomerIncludingAssessedTax"
# Produce a clean sample and report the sample size, the sample’s mean and the sample’s standard deviation.
# For extra credit: Prepare a map that shows where in the U.S. the corporate revenues are located.

# load packages
library(tidyr)
library(dplyr)
library(readr)
library(tidylog)
library(stringr)

# read in csvs 
num_df <- read_csv("data/num.csv")
pre_df <- read_csv("data/pre.csv")
sub_df <- read_csv("data/sub.csv")
tag_df <- read_csv("data/tag.csv")  

# filter for variables of interest
sub_df %>% 
  filter(countryba == "US") %>%
  filter(str_detect(fp, "Q")) %>% 


filter(
  tag == "Revenues" |
    tag == "RevenueFromContractWithCustomerExcludingAssessedTax" | 
    tag == "RevenueFromContractWithCustomerIncludingAssessedTax"
)


# create joined dataframe
# join by columns taken from: https://www.sec.gov/files/aqfs.pdf
sec_df <-
  num_df %>% 
  left_join(pre_df, by = "adsh") %>%
  # left_join(tag_df, by = "version") %>% 
  left_join(pre_df, by = "adsh")

# select revenue column
sec_df %>% 
  select(value)
