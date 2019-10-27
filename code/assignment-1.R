#' ---
#' title: "Assignment #1"
#' author: "Maia Salholz-Hillel"
#' date: ""
#' always_allow_html: yes
#' output_dir: "output"
#' output:
#'   html_document:
#'     df_print: kable
#' ---

#+ setup, include = FALSE

cran_pkgs <- c("dplyr", "tidyr", "lubridate", "stringr", "here", "broom", "knitr", "ggplot2", "WDI", "maps", "gganimate", "gifski", "transformr")
to_install <- cran_pkgs[!cran_pkgs %in% installed.packages()]

if (length(to_install) > 0) {
  install.packages(to_install, deps = TRUE, repos = "https://cran.r-project.org")
}

options(scipen=999)

# provides page and section breaks for MS Word documents
# if (!"officedown" %in% installed.packages()) {
#   devtools::install_github("davidgohel/officedown")
# }

invisible(lapply(c(cran_pkgs), library, character.only = TRUE))

#+ download data
# https://data.worldbank.org/indicator
# We are interested in indicators for "Scientific and technical journal articles, "Researchers in R&D (per million people)," and "Population, total"
article_indicator <- WDIsearch("Scientific and technical journal articles")
researcher_indicator <- WDIsearch("Researchers in R&D")
population_indicator <- WDIsearch("Population, total")

wb <-
  WDI(
    indicator = c("article" = article_indicator[["indicator"]],
                  "researcher_per_million" = researcher_indicator[["indicator"]],
                  "population" = population_indicator[["indicator"]]),
    country   = c("US", "DE"),
    start     = 2008,
    end       = 2016
  ) %>% 
  mutate(population_in_millions = population/1000000,
         researcher = researcher_per_million * population_in_millions,
         articles_per_researcher = article/researcher,
         researchers_per_article = researcher/article,
         articles_in_thousands = article/1000
  )

attr(wb[["researcher"]], "label") <- "Researchers in R&D, total"
attr(wb[["articles_per_researcher"]], "label") <- "Articles per researcher"

wb %>% 
  ggplot(aes(population_in_millions, researcher_per_million, color = country, size = articles_in_thousands)) + geom_point()

wb %>% 
  # filter(year == 2016) %>%
  ggplot(aes(researcher_per_million, articles_in_thousands, alpha = 0.05, color = country)) + 
  geom_point(aes(size = articles_per_researcher)) +
  # geom_label(nudge_y = 20, aes(label = round(researchers_per_article, digits = 1))) + 
  transition_time(year) + labs(title = "Year: {frame_time}", subtitle = "Number of researchers per article")
  # "Number of researchers per article: {round(researchers_per_article, digits = 1)}"

wb %>% 
  ggplot(aes(researcher_per_million, articles_per_researcher, color = country)) + 
  geom_point(aes(size = format(article, scientific = FALSE)) +
  geom_smooth(data = select(wb, -year), se =  FALSE) +
  transition_time(year) + 
  shadow_mark(alpha = 0.3) +
  labs(
    title = "OMG",
    subtitle = "Year: {frame_time}",
    caption = "Data: https://data.worldbank.org/indicator",
    x = "Researchers in R&D (per million people)",
    y = "Rate of Articles per Researcher",
    color = "Country",
    size = "Number of S&T\n Journal Articles")

# Researcher Efficiency; Too many researchers! Researcher efficiency plummets as number of researchers increase
# Pressure to Publish or Perish; More researchers promotes publication quality over quantity! Researchers experience less publication pressure as number of researchers increase

  wb %>% 
  # filter(year == 2016) %>%
  ggplot(aes(researcher_per_million, articles_per_researcher, color = country)) +
  geom_point() + geom_smooth(se =  FALSE)

wb %>% 
  # filter(year == 2016) %>%
  ggplot(aes(population, researcher, size = articles_per_researcher, color = country)) +
  geom_point() 

wb %>% 
  # filter(year == 2016) %>%
  ggplot(aes(researcher_per_million, articles_per_researcher, color = country)) +
  geom_point() + transition_time(year) + labs(title = "Year: {frame_time}")

article_researcher_plot <- wb %>% 
  ggplot(aes(year, articles_per_researcher, color = country)) + geom_line() + 
  xlab("Year") + ylab("Scientific and technical journal articles per researcher in R&D")


article_plot <- wb %>% 
  ggplot(aes(year, article, color = country)) + geom_line() + 
  xlab("Year") + ylab("Number of Scientific and technical journal articles")

# article_plot + transition_time(year) + labs(title = "Year: {frame_time}")
article_plot + transition_reveal(year) + labs(title = "Year: {frame_time}")


wb %>% 
  ggplot(aes(year, researcher, color = country)) + geom_line() + 
  xlab("Year") + ylab("Researchers in R&D (per million people)")

wb %>% 
  ggplot(aes(researcher, article, color = country)) + geom_line() + 
  xlab("Researchers in R&D (per million people)") + ylab("Number of Scientific and technical journal articles")

wb %>% 
  ggplot() + borders("world", colour="gray50", fill="gray50")

ggplot() + map_data("USA")