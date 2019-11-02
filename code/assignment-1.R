#' ---
#' title: "Assignment #1"
#' author: "Maia Salholz-Hillel"
#' date: "`r Sys.Date()`"
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

invisible(lapply(c(cran_pkgs), library, character.only = TRUE))

#+ set up data frames
# https://data.worldbank.org/indicator
article_indicator <- WDIsearch("Scientific and technical journal articles")
researcher_indicator <- WDIsearch("Researchers in R&D")
population_indicator <- WDIsearch("Population, total")

start_year <- 2008
end_year <- 2016

wb <-
  WDI(
    indicator = c("article" = article_indicator[["indicator"]],
                  "researcher_per_million" = researcher_indicator[["indicator"]],
                  "population" = population_indicator[["indicator"]]),
    country   = c("US", "DE"),
    start     = start_year,
    end       = end_year
  ) %>% 
  as_tibble() %>% 
  mutate(population_in_millions = population/1000000,
         researcher = researcher_per_million * population_in_millions,
         articles_per_researcher = article/researcher,
         researchers_per_article = researcher/article,
         articles_in_thousands = article/1000
  )
  
#+ set up tweets (neutral, negative spin, positive spin)
tweet_neutral <- 
  wb %>% 
  ggplot(aes(researcher_per_million, articles_per_researcher, color = country)) + 
  geom_point(aes(size = articles_in_thousands)) +
  geom_smooth(data = select(wb, -year), se =  FALSE) +
  transition_time(year) + 
  shadow_mark(alpha = 0.3) +
  theme_minimal() +
  theme(
    legend.justification = c(1,1), 
    legend.position = c(1,1), 
    legend.background = element_blank()
  ) +
  scale_color_viridis_d() +
  guides(
    color = guide_legend(order = 1),
    size = guide_legend(order = 2)
  ) +
  labs(
    title = paste0("Researchers and Publications in Germany and USA (", start_year, " - ", end_year, ")"),
    subtitle = "Year: {frame_time}",
    caption = "Data: https://data.worldbank.org/indicator",
    x = "Researchers in R&D (per million people)",
    y = "Rate of Articles per Researcher",
    color = "Country",
    size = "Number of S&T\n Journal Articles\n (in thousands)"
  )

tweet_negative_spin <- 
  tweet_neutral +
  labs(
    title = "Too many researchers!\nResearcher efficiency plummets as number of researchers increase",
    y = "Researcher Efficiency\nRate of Articles per Researcher"
  )
  
tweet_positive_spin <-
  tweet_neutral +
  labs(
    title = "More researchers promotes publication quality over quantity!\nPublication pressures decrease as number of researchers increase",
    y = "Pressure to Publish or Perish\nRate of Articles per Researcher"
  )

#+ save tweets
anim_save(filename = "tweet_neutral.gif", animation = tweet_neutral, path = "output")
anim_save(filename = "tweet_negative_spin.gif", animation = tweet_negative_spin, path = "output")
anim_save(filename = "tweet_positive_spin.gif", animation = tweet_positive_spin, path = "output")