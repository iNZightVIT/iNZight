library(devtools)

load_all("~/iNZightTools")
load_all("~/iNZight")

document("~/iNZightTools")

# data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\iNZight\\scripts\\paris1.csv")
data = data.frame("Country" = c("A", "B", "C"), "v1999" = c("0.7K", "37K", "212K"), "v2000" = c("2K", "80K", "213K"))
data = data.frame("Country" = c("Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", "Brazil", "Brazil", "Brazil", "Brazil", "China", "China", "China", "China"),
               "Year" = c(1999,1999,2000,2000,1999,1999,2000,2000,1999,1999,2000,2000),
               "Type" = c("cases", "population", "cases", "population", "cases", "population", "cases", "population", "cases", "population", "cases", "population"),
               "Count" = c(745,19987071,2666,20595360,37737,172006362,80488,174504898,212258,1272915272,213766,1280428583))

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
data
