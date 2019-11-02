library(rvest)

lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

rating <- lego_movie %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
rating

lego_movie %>%
  html_nodes("td a") %>%
  xml_find_all(".//tt_cl_t")
  xml_find_all(“//a[contains(@href, 'ref_=tt_cl_t')]”)
  html_nodes("ref_=tt_cl_t") %>% 
  html_text()
