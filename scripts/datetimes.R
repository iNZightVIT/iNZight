#install_deps("~/iNZightTools")
#install_deps("~/iNZight", dependencies = TRUE)

library(devtools)

load_all("~/iNZightTools")
load_all("~/iNZight")

document("~/iNZightTools")

data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\iNZight\\scripts\\paris1.csv")
data
?convert_to_datetime
?extract_part

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

?str_c

## Code history to be written
## put inserted column after the selected column extract and convert

## addhandlerchanged to gtree (change double click to single click)

## Check week of the year
## U means "Week of the year as decimal number (00?C53) using Sunday as the first day 1 of the week 
## and typically with the first Sunday of the year as day 1 of week 1). The US convention.
## Using format.POSIXct to check (U, V and W)
## Day of week
## default value of a gtree

## Time zone


## Code history
## Convert
## Extract