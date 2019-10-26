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

cran_pkgs <- c("dplyr", "tidyr", "lubridate", "stringr", "here", "broom", "knitr", "ggplot2", "WDI", "maps", "gganimate", "gifski")
to_install <- cran_pkgs[!cran_pkgs %in% installed.packages()]

if (length(to_install) > 0) {
  install.packages(to_install, deps = TRUE, repos = "https://cran.r-project.org")
}

# provides page and section breaks for MS Word documents
# if (!"officedown" %in% installed.packages()) {
#   devtools::install_github("davidgohel/officedown")
# }

invisible(lapply(c(cran_pkgs), library, character.only = TRUE))

#+ download data
# https://data.worldbank.org/indicator
# We are interested in indicators for "Scientific and technical journal articles" and "Researchers in R&D (per million people)"
article_indicator <- WDIsearch("Scientific and technical journal articles")
researcher_indicator <- WDIsearch("Researchers in R&D")

wb = WDI(
  indicator = c("article" = article_indicator[["indicator"]],
                "researcher" = researcher_indicator[["indicator"]]),
  country   = c("US", "DE"),
  start     = 2008,
  end       = 2016)

wb %>% 
  mutate(article_by_researcher = article/researcher) %>% View()

article_plot <- wb %>% 
  ggplot(aes(year, article, color = country)) + geom_line() + 
  xlab("Year") + ylab("Number of Scientific and technical journal articles")

article_plot + transition_time(year) + labs(title = "Year: {frame_time}")
article_plot + transition_reveal(year)

wb %>% 
  ggplot(aes(year, researcher, color = country)) + geom_line() + 
  xlab("Year") + ylab("Researchers in R&D (per million people)")

wb %>% 
  ggplot(aes(researcher, article, color = country)) + geom_line() + 
  xlab("Researchers in R&D (per million people)") + ylab("Number of Scientific and technical journal articles")

wb %>% 
  ggplot() + borders("world", colour="gray50", fill="gray50")

ggplot() + map_data("USA")


