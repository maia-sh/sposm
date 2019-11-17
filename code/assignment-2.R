# a latex table (in plain text),
# a list of number pairs containing row and column indices to identify cells, and
# a list of colors
# 
# https://en.wikibooks.org/wiki/LaTeX/Tables#Colors_of_individual_cells

library(stringr)
library(readr)

# for each color
# get mapping (and the remove from mapping)
# go to row
# go to column
# add color

my_map <- list(c(1, 1), c(1,2), c(2,2))

for c in color {
  col <- c
  map <- mapping[[1]]
  mapping <- mapping[2:length(map)]
  
}

colors <- c("red", "orange", "yellow", 
            "green", "blue", "purple", 
            "white", "grey", "black")
latex_colors <- paste0("\\cellcolor{", colors, "}")

# read in basic latex table excluding first and last line
# latex_table <- read_file("raw_data/latex_table.txt")
latex_table <- read_lines("raw_data/latex_table.txt", skip = 1)
latex_table <- latex_table[1: length(latex_table) - 1]


latex_table %>% 
  str_split_fixed("&",n = length(latex_table)) %>% 
  str_glue(sep = "&")

  str_replace_all("^|& ", "& ahoy") %>% 
  str_replace("& ", "")

  str_replace_all(latex_table, "^|& ", add_color)

str_c(latex_table, "blah")
str_locate(latex_table, "&")
str_locate_all(latex_table, "^|& ")
str_replace_all(latex_table, "& ", "& ahoy")
str_sub(latex_table, 65, 68)

add_color <- function(rpl_txt){
  latex_colors
}

color_latex_table <- function(table, mapping, colors) {
  
  # check parameters
  # table must be a table in latex
  str_detect(table,"^\begin{tabular}") & str_detect(table, "\end{tabular}$")
  
  # mapping and colors must be list
  is.list(mapping)
  is.list(colors)
  
  
  
  #add to latex file for colors
  # \usepackage[table]{xcolor}
}
  
  
\begin{tabular}{ l c r }
1 & 2 & 3 \\
4 & 5 & 6 \\
7 & 8 & \cellcolor{red}9 \\
\end{tabular}

