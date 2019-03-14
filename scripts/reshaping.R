library(devtools)

load_all("~/iNZightTools")
load_all("~/iNZight")

document("~/iNZightTools")

# data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\iNZight\\scripts\\paris1.csv")
data = data.frame("Country" = c("A", "B", "C"), "v1999" = c("0.7K", "37K", "212K"), "v2000" = c("2K", "80K", "213K"))

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

?gather
