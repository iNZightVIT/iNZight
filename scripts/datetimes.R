#install_deps("~/iNZightTools")
#install_deps("~/iNZight", dependencies = TRUE)

library(devtools)

load_all("~/iNZightTools")
load_all("~/iNZight")

document("~/iNZightTools")

data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\iNZight\\scripts\\paris1.csv")

## Convert function
try(dispose(kk$win), TRUE)
load_all("~/iNZight")
kk = iNZGUI$new()
kk$initializeGui(data)


## Extract function
data$`Time stamp` = lubridate::parse_date_time(data$`Time stamp`, "%m%d%y%H%M%p")
try(dispose(kk$win), TRUE)
load_all("~/iNZight")
kk = iNZGUI$new()
kk$initializeGui(data)


## addhandlerchanged to gtree (change double click to single click)
## default value of a gtree
## Extract hours decimal 
## Window sizing?
## Convert function trycatch
## Timezone