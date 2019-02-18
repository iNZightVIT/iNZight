library(devtools)
library(tidyverse)
document("~/iNZightTools")

load_all("~/iNZightTools")
load_all("~/iNZight")


data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\join.csv")


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


data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin.csv") #original
data2 = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin2.csv") #has the same column "Instagram photo" with completely different values
data3 = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin3.csv") #contains a column named "sdasdsds" which has the same value as "Instagram user"
data4 = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin4.csv") #contains the same column "Instagram photo" with one value that is the same to the original one
