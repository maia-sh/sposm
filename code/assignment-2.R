# a latex table (in plain text),
# a list of number pairs containing row and column indices to identify cells, and
# a list of colors
# 
# https://en.wikibooks.org/wiki/LaTeX/Tables#Colors_of_individual_cells

library(stringr)
library(readr)
library(dplyr)

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

mapping_rc <- 
  list(c(2,1), c(1,2), c(1,3),
       c(1,1), c(2,2), c(2,3),
       c(3,1), c(3,2), c(3,3)
  )
2, 3, 4, 
3, 4, 5, 
4, 5, 6
5, 6, 7,
6, 7, 8

1, 2, 3,
4, 5, 6
7, 8, 9,
10, 11, 12,
13, 14, 15

-1, 1, 3, 5, 7

2, 3, 4, 5,
3, 4, 5, 6,
4, 5, 6, 7

1, 2, 3, 4,
5, 6, 7, 8,
9, 10, 11, 12

-1, 2, 5
n_row-1

lapply(sum)

# find all pairs with row=1 and label 1 to n
#   find col 1 to y
# find all pairs with row=2 and label n+1 to m

mapping_rc_array <- unlist(mapping_rc)
map_rows <- mapping_rc_array[c(TRUE , FALSE)]
map_cols <- mapping_rc_array[c(FALSE , TRUE)]
n_row <- max(mapping_rc_array[c(TRUE , FALSE)])
n_col <- max(mapping_rc_array[c(FALSE , TRUE)])  

for (i in 9) {
  
  
  
}

# convert a list of row-columns into list of numbers

mapping





color_latex_table <- function(tbl_txt, color_tbl_txt, colors, mapping) {
  
  # read in basic latex table, saving first and last line for later
  latex_table <- read_lines(tbl_txt)
  first_line <- latex_table[1]
  last_line <- latex_table[length(latex_table)]
  latex_table <- latex_table[2: (length(latex_table) - 1)]
  
  # reorder colors
  reordered_colors <- colors[mapping]
  
  # format colors for latex cell colors
  latex_colors <- paste0("\\cellcolor{", reordered_colors, "}")
  
  # add colors to table
  latex_table %>% 
    str_split("& ",n = length(latex_table)) %>% 
    unlist() %>% 
    str_c(latex_colors, .) %>% 
    str_replace_all(" $", " & ") %>% 
    str_replace_all("\\\\\\\\", "\\\\\\\\\n") %>% 
    paste0(collapse = "") %>% 
    str_c(first_line, "\n", ., last_line) %>% 
    write_file(path = color_tbl_txt)
}

color_latex_table(
  tbl_txt = "raw_data/latex_table.txt",
  color_tbl_txt = "output/color_latex_table.txt",
  colors = c("red", "orange", "yellow", 
             "green", "blue", "purple", 
             "white", "grey", "black"),
  mapping = sample(1:9)
)

# check parameters
# table must be a table in latex
str_detect(table,"^\begin{tabular}") & str_detect(table, "\end{tabular}$")

# mapping and colors must be list
is.list(mapping)
is.list(colors)