# title: "Assignment #2"
# author: "Maia Salholz-Hillel"
# date: "`r Sys.Date()`"

library(stringr)
library(readr)
library(dplyr)

# create function
color_latex_table <- function(tbl_txt, color_tbl_txt, colors, mapping) {
  
  # read in basic latex table, saving first and last line for later
  latex_table <- read_lines(tbl_txt)
  first_line <- latex_table[1]
  last_line <- latex_table[length(latex_table)]
  latex_table <- latex_table[2:(length(latex_table) - 1)]
  
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

# color table
color_latex_table(
  tbl_txt = "raw_data/latex_table.txt",
  color_tbl_txt = "output/color_latex_table.txt",
  colors = c("red", "orange", "yellow", 
             "green", "blue", "purple", 
             "white", "grey", "black"),
  mapping = sample(1:9)
)