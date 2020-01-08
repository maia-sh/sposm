# title: "Assignment #3"
# author: "Maia Salholz-Hillel"
# date: "`r Sys.Date()`"

# Collect (scrape) and tidy Wikipedia data. Collect whatever data you find to be interesting and can get in a standardized way for a reasonable set of pages. The tidy datasets should be stored in the “data” directory. If you feel like it, you can also prepare an informative visual based on your scraped data. Possible data includes:
# The info in the top right infobox
# The length of the Wikipedia article
# Some info on its revision history


# analysis plan -----------------------------------------------------------
# find all pages in categories of "18th-century French scientists" and "18th-century women scientists"
#   df with col for category (rep of 2) and each page
# filter for page not unique (i.e. appears in both categories)
# find all categories all non-unique pages (or unique, then find for all pages if additional visuals)
# Questions/graphics:
# How many "18th-century French scientists" are women?
# How many "18th-century women scientists" are French?
# Of "18th-century French women scientists" (in both categories), what other categories are they in?
# Maybe:
# Of "18th-century French scientists" who are not women, what other categories are they in?
# Of "18th-century women scientists" who are not French, what other categories are they in?

# install and load packages -----------------------------------------------

cran_pkgs <- c("dplyr", "tidyr", "janitor", "lubridate", "purrr", "stringr", "here", "broom", "knitr", "ggplot2", "WikipediR")
to_install <- cran_pkgs[!cran_pkgs %in% installed.packages()]

if (length(to_install) > 0) {
  install.packages(to_install, deps = TRUE, repos = "https://cran.r-project.org")
}

invisible(lapply(c(cran_pkgs), library, character.only = TRUE))


# find categories in page -------------------------------------------------

cinp <- categories_in_page("en", "wikipedia", 
                           pages = "Geneviève Thiroux d'Arconville", 
                           clean_response = TRUE)
cinp_two <- categories_in_page("en", "wikipedia",
                           pages = c("Geneviève Thiroux d'Arconville",
                                     "Émilie Du Châtelet"),
                           clean_response = TRUE)
cinp_ec <- categories_in_page("en", "wikipedia",
                           pages = "Émilie du Châtelet",
                           clean_response = TRUE)

# Émilie Du Châtelet

cinp2 <- cinp %>% unlist(recursive = FALSE)
categs <- 
  cinp2$categories %>% 
  mutate(
    page_id = cinp2$pageid, page_title = cinp2$title,
    category_title = str_replace(title, "Category:", "")
)

categories_in_page("en", "wikipedia", 
                   pages = "Nicole-Reine Lepaute", 
                   clean_response = TRUE)

# find pages in a category ------------------------------------------------

# both <- pages_in_category("en", "wikipedia", 
#                           categories = c("18th-century French scientists", 
#                                          "18th-century women scientists"), 
#                           clean_response = F)

cat_fr_raw <- pages_in_category("en", "wikipedia", 
                  categories = "18th-century French scientists",
                  type = "page",
                  clean_response = T)

cat_fr <-
  cat_fr_raw %>% 
  mutate(category_title = "18th-century French scientists",
         page_title = title)

cat_w_raw <- pages_in_category("en", "wikipedia", 
                                 categories = "18th-century women scientists",
                                 type = "page",
                                 clean_response = T)

cat_w <-
  cat_w_raw %>% 
  mutate(category_title = "18th-century women scientists",
         page_title = title)

cat_comb <- bind_rows(cat_fr, cat_w)

# cat_comb %>% distinct(pageid, .keep_all = TRUE)

pgs_in_both <- cat_comb %>% janitor::get_dupes(pageid)

fr_w <- 
  pgs_in_both %>% 
  distinct(pageid, .keep_all = TRUE) %>% 
  pull(page_title)

get_page_categories <- function(page) {
  
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

cat_fr_w <-
  fr_w %>% 
  map_dfr(get_page_categories) %>% 
  mutate(category_title = str_replace(title, "Category:", ""))

