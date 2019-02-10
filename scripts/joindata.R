library(devtools)

load_all("~/iNZightTools")
load_all("~/iNZight")

document("~/iNZightTools")

data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\Join.csv")

try(dispose(kk$win), TRUE)
load_all("~/iNZight")
kk = iNZGUI$new()
kk$initializeGui(data)


