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

# col names (suffix)
# help