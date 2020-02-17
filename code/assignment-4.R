# title: "Assignment #4"
# author: "Maia Salholz-Hillel"
# date: "`r Sys.Date()`"


# Assignment --------------------------------------------------------------

# Collect (scrape) and tidy Wikipedia data. Collect whatever data you find to be interesting and can get in a standardized way for a reasonable set of pages. The tidy datasets should be stored in the “data” directory. If you feel like it, you can also prepare an informative visual based on your scraped data. Possible data includes:
# The info in the top right infobox
# The length of the Wikipedia article
# Some info on its revision history


# My Research Question ----------------------------------------------------

# BACKGROUND
# Women are underepresented on Wikipedia (https://wikimediafoundation.org/news/2018/10/18/wikipedia-mirror-world-gender-biases/). This gender bias and underrepresentation includes women in science (https://www.nature.com/articles/d41586-018-05947-8) and women in history (http://theconversation.com/why-wikipedia-often-overlooks-stories-of-women-in-history-92555). During my bachelors in neuroscience and romance languages, I experienced this issue during my research on 18th century French women scientists. For this assignment, I decided to explore the following research questions:
# How many 18th century French women scientists are represented on Wikipedia?
# What categories are 18th century French women scientists represented in?
# What basic information (i.e., Wikipedia infobox) is provided for 18th century French women scientists?

# METHOD
# I first looked for a category for "18th century French women scientists" but did not find any. Instead I used the intersection of "18th-century French scientists" and "18th-century women scientists." I collected a list of all the pages in each of these two categories (dataframe 1: pgs_fr_or_w) and selected the pages that appeared in both categories (dataframe 2: pgs_fr_and_w), which represent "18th century French women scientists". For each page on an "18th century French women scientist", I collected the categories mentioned for each page (dataframe 3: cats_fr_and_w) and the information in the infobox, when an infobox was available (dataframe 4: info_fr_and_w).

# FINDINGS
# I found 32 pages on "18th-century French scientists" and 50 pages on "18th-century women scientists" (dataframe 1). Of these, 8 pages were on "18th century French women scientists" (dataframe 2). These "18th century French women scientists" are mentioned in 93 unique categories. Only one of these "18th century French women scientists" has an infobox.

# CONCLUSION
# There is still a lot of work needed to reduce the underrepresentation of 18th century French women scientists, and likely other women (scientists), on Wikipedia. The Berlin Institute of Health promotes this with iniatives such as the Diversithon (https://www.bihealth.org/en/institute/equal-opportunity/career/berlin-diversithon/).

# SOURCES
# In addition to help documentation for the packages used, I consulted the following sources:
# https://medium.com/@kyleake/wikipedia-data-scraping-with-r-rvest-in-action-3c419db9af2d

# Install and load packages -----------------------------------------------

# NOTE: I tried to update the dockerfile, but I couldn't figure out how to make the pull request for a single commit/file.
cran_pkgs <- c("dplyr", "tidyr", "rvest", "janitor", "stringr","purrr", "WikipediR")
to_install <- cran_pkgs[!cran_pkgs %in% installed.packages()]

if (length(to_install) > 0) {
  install.packages(to_install, deps = TRUE, repos = "https://cran.r-project.org")
}

invisible(lapply(c(cran_pkgs), library, character.only = TRUE))

# Get pages in categories -------------------------------------------------

get_pages_in_category <- function(category){
  
    pages_in_category("en", "wikipedia", 
                    categories = category,
                    type = "page",
                    clean_response = T
    ) %>% 
    mutate(category_title = category)
}

cats_fr_w <- c("18th-century French scientists", 
              "18th-century women scientists")

pgs_fr_or_w <- 
  map_dfr(cats_fr_w, 
          get_pages_in_category) %>% 
  dplyr::rename(page_title = title)

n_pgs_fr <-
  pgs_fr_or_w %>% 
  filter(category_title == "18th-century French scientists") %>%
  nrow()

n_pgs_w <-
  pgs_fr_or_w %>% 
  filter(category_title == "18th-century women scientists") %>%
  nrow()

# pgs_fr_or_w %>% distinct(pageid, .keep_all = TRUE)

pgs_fr_and_w <- pgs_fr_or_w %>% janitor::get_dupes(pageid)

names_fr_and_w <- 
  pgs_fr_and_w %>% 
  distinct(pageid, .keep_all = TRUE) %>% 
  pull(page_title)

n_pgs_fr_and_w <-
  length(names_fr_and_w)
# Get categories for pages ------------------------------------------------

get_categories_in_page <- function(page) {
  
  categories_raw <- 
    categories_in_page("en", "wikipedia",
                       pages = page,
                       clean_response = TRUE) %>%
    unlist(recursive = FALSE)
  
  categories_clean <- 
    categories_raw$categories %>% 
    mutate(page_id = categories_raw$pageid, 
           page_title = categories_raw$title
    )
}

cats_fr_and_w <-
  names_fr_and_w %>% 
  map_dfr(get_categories_in_page) %>% 
  mutate(category_title = str_replace(title, "Category:", ""))

n_cats_fr_and_w <-
 nrow(cats_fr_and_w)
  
# Get infoboxes -----------------------------------------------------------

get_infobox <- function(url) {
  
  infobox_raw <-
    url %>%
    read_html() %>%
    html_node("table.infobox")
  
  if (length(infobox_raw) == 0) {
    return(NULL)
  }
  
  infobox_raw %>% 
    html_table(header = FALSE)
  
  df_infobox_raw <-
    infobox_raw %>%
    html_table(header = FALSE)
  
  info_names <- 
    df_infobox_raw %>% 
    pull(1)
  
  infobox <- 
    df_infobox_raw %>% 
    pull(2) %>% 
    t() %>% 
    as.data.frame()
  
  colnames(infobox) <- info_names
  
  colnames(infobox)[1] <- "page_title"
  
  infobox %>% 
    janitor::clean_names() %>%
    select(page_title, born, died, nationality, known_for, 
           spouse_s, partner_s, children, fields, influences
    )
}

# create urls for 18th century French women scientists
urls_fr_and_w <-
  names_fr_and_w %>% 
  str_replace_all(" ", "_") %>% 
  str_replace("^", "https://en.wikipedia.org/wiki/")

info_fr_and_w <-
  map_dfr(urls_fr_and_w, get_infobox)