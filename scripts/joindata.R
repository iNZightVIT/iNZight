library(devtools)
library(tidyverse)
document("~/iNZightTools")

load_all("~/iNZightTools")
load_all("~/iNZight")


data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin.csv")


try(dispose(kk$win), TRUE)
load_all("~/iNZight")
kk = iNZGUI$new()
kk$initializeGui(data)


# help
# preview window size use head function

# No common column found. Automatically mtach has common column with no common value DONE
# Only show suffix window when there is duplicated column
# Test on larger dataset DONE
# testthat

# head() show only top 10 rows DONE

devtools::test