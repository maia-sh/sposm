# https://en.wikipedia.org/wiki/Category:Scientists_by_nationality
# get df of subcategories (number of countries)
# for each country, get subcategories and get pages

# 3 df:
# countries of scientists (row per country)
# subcategories (row per sub per country with country col and subcat col)
# pages (row per scientist with country col)

# install and load packages -----------------------------------------------

cran_pkgs <- c("dplyr", "tidyr", "janitor", "lubridate", "purrr", "stringr", "here", "broom", "knitr", "ggplot2", "WikipediR")
to_install <- cran_pkgs[!cran_pkgs %in% installed.packages()]

if (length(to_install) > 0) {
  install.packages(to_install, deps = TRUE, repos = "https://cran.r-project.org")
}

invisible(lapply(c(cran_pkgs), library, character.only = TRUE))


# scientists by nationality -----------------------------------------------

natl_subcat <- 
  pages_in_category("en", "wikipedia", 
                    categories = "Scientists by nationality",
                    type = "subcat",
                    clean_response = T
  ) %>% 
  mutate(category_title = str_replace(title, "Category:", "")) %>% 
  filter(str_detect(category_title, "scientists$"))

natl_subcat_vector <-
  natl_subcat %>% 
  pull(category_title)

# find pages in a category ------------------------------------------------

get_pages_in_category <- function(category){
  
  try(
    pages_in_category("en", "wikipedia", 
                    categories = category,
                    type = "page",
                    clean_response = T
  )
   %>% 
    mutate(category_title = category)
  )
}

pg_sci <- 

  map_dfr(natl_subcat_vector[1:8], get_pages_in_category)

%>% 
  rename(page_title = title)

fr_w_cat <- c("18th-century French scientists", 
              "18th-century women scientists")