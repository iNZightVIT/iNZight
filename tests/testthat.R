## Test file order (alphabetical prefixes):
##   A foundation | B data I/O | C wrangling | D plots/code | E analysis | F surveys | G modules
library(testthat)
library(iNZight)

options(readr.show_progress = FALSE)

test_check("iNZight")
