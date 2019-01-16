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

##transform var

var.dt = c(lubridate::ymd(19970113), lubridate::ymd(19871105), lubridate::ymd(20190103))
x = lubridate::ymd(var.dt)
var.dt = chron::times(strftime(x, "%H:%M:%S", tz = "UTC"))
var.dt = as.character(var.dt)
if (all(var.dt == "00:00:00")) {
  print("hi")
}
var.dt
all(var.dt == "00:00:00")

data
lubridate::month(data$`Time stamp`)
lubridate::hours(data$`Time stamp`)

var.dt = chron::times(strftime(data$`Time stamp`, "%H:%M:%S", tz = "UTC"))
var.dt

x = as.character(var.dt)

sapply(strsplit(x,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+(x[2]+x[3]*60)/60
       }
)
