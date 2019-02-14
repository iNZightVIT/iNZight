library(devtools)
library(tidyverse)
document("~/iNZightTools")

load_all("~/iNZightTools")
load_all("~/iNZight")


data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\Join.csv")


try(dispose(kk$win), TRUE)
load_all("~/iNZight")
kk = iNZGUI$new()
kk$initializeGui(data)


# help
# preview window size use head function
# Logical values not working


## join method has to change in order to get join_method but the default is given in fun??  ----- problem with columns

## fix suffix

## check for things that cannot be joined

## check button works