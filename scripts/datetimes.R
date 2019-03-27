install_deps("~/iNZightTools", dependencies = TRUE)
install_deps("~/iNZight", dependencies = TRUE)
install_deps("~/iNZightPlots", dependencies = TRUE)

install.packages("glue")

library(devtools)

# install_deps("~/iNZightPlots")
load_all("~/iNZightTools")
load_all("~/iNZightPlots")
load_all("~/iNZight")

document("~/iNZightTools")

data = readr::read_csv("C:\\Users\\30576\\Documents\\iNZight\\scripts\\paris1.csv")

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


## addhandlerchanged to gtree (change double click to single click) done
## default value of a gtree done
## Extract hours decimal done
## Window sizing? done
## Convert function trycatch
## Timezone