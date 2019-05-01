install_deps("~/iNZightTools", dependencies = TRUE)
install_deps("~/iNZight", dependencies = TRUE)
# install_deps("~/iNZightPlots", dependencies = TRUE)


library(devtools)
load_all("~/iNZightTools")
load_all("~/iNZightPlots")
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

## Aggregation of time
data = readr::read_csv("C:\\Users\\30576\\Documents\\iNZight\\scripts\\paris1.csv")
data$`Time stamp` = lubridate::parse_date_time(data$`Time stamp`, "%m%d%y%H%M%p")
data = data %>% tibble::add_column("Weeks" = paste0("2017W", rep(1:52, length.out=nrow(data))))
data = data %>% tibble::add_column("Months" = paste0("2006M", rep(1:12, length.out=nrow(data))))
data = data %>% tibble::add_column("Quarters" = paste0("2006Q", rep(1:4, length.out=nrow(data))))

data$dateonly <- as.Date(data$`Time stamp`)
data
