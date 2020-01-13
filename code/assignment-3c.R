# https://steviep42.github.io/webscraping/book/

# install and load packages -----------------------------------------------

cran_pkgs <- c("dplyr", "tidyr", "rvest", "reshape", "janitor", "lubridate", "purrr", "stringr", "here", "broom", "knitr", "ggplot2", "WikipediR")
to_install <- cran_pkgs[!cran_pkgs %in% installed.packages()]

if (length(to_install) > 0) {
  install.packages(to_install, deps = TRUE, repos = "https://cran.r-project.org")
}

invisible(lapply(c(cran_pkgs), library, character.only = TRUE))

# hiii --------------------------------------------------------------------

# create urls for 18th century French women scientists
urls_fr_and_w <-
  names_fr_and_w %>% 
  str_replace_all(" ", "_") %>% 
  str_replace("^", "https://en.wikipedia.org/wiki/")

map_dfr(urls_fr_and_w, get_infobox)
# get_infobox(fr_w_url[1])

# url_plain = "https://en.wikipedia.org/wiki/Émilie_du_Châtelet"
# url_plain = "https://en.wikipedia.org/wiki/Marie_Marguerite_Bihéron"


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
    select(page_title, born, died, nationality, known_for, spouse_s, partner_s, children, fields, influences)
}

get_infobox(url)


# url = "https://en.wikipedia.org/wiki/Johann_Karl_Burckhardt"

# infobox <-
#   url %>% 
#   read_html() %>% 
#   html_nodes("table.infobox")
# 
# name <-
#   infobox %>% 
#   html_node("div.fn") %>% 
#   html_text()
# 
# birthdate <-
#   infobox %>% 
#   html_node("span.bday") %>% 
#   html_text()
# 
# birthplace <-
#   infobox %>% 
#   html_nodes("div.birthplace a") %>% 
#   html_attr("title")
# 
# # deathdate
# 
# deathplace <-
#   infobox %>% 
#   html_nodes("div.deathplace a") %>% 
#   html_attr("title")
# 
# # nationality