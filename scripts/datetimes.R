#install_deps("~/iNZightTools")
#install_deps("~/iNZight", dependencies = TRUE)

library(devtools)

document("~/iNZightTools")

load_all("~/iNZightTools")
load_all("~/iNZight")

data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\iNZight\\scripts\\paris1.csv")
data
?convert_to_datetime
?extract_part

## Convert function
try(dispose(kk$win), TRUE)
load_all("~/iNZight")
kk = iNZGUI$new()
kk$initializeGui(data)

is.numeric(data$`#days`)

## Extract function
data$`Time stamp` = lubridate::parse_date_time(data$`Time stamp`, "%m%d%y%H%M%p")
try(dispose(kk$win), TRUE)
load_all("~/iNZight")
kk = iNZGUI$new()
kk$initializeGui(data)


## Convert
## Add UNIX timestamp to dropdown DONE
## Add in check box to let the user choose between dropdown and multiple field DONE

## Extract
## Change extract drop down to a compressed version
## Add in hour/minute/second DONE but start from Sunday or Monday?

## Tests to be written
## Code history to be written

## Time zone