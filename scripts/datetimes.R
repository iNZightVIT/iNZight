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

## for columns that end with ".missing", if it contains all zero, removes it
data = readr::read_csv("C:\\Users\\30576\\Documents\\quakes_play.csv")
data[2,2] <- NA
data = iNZightTools::extract_part(data, "origintime", "Year Month", "Monthly")
data = iNZightTools::aggregateData(data, "Monthly", "mean")
colname = subset(colnames(data), grepl("[:.:]missing$",colnames(data)))
for (i in 1:length(colname)) {
  if (all(data[[colname[i]]] == 0)) {
    data[, colname[i]] <- NULL
  }
}
data


all(data[["longitude.missing"]] == 0)

for (i in x) {
  print(i)
  print(i == as.numeri0)
}
ifelse()

all(x) == as.numeric()

aa = c(0,0,0,0,0,0,1)
all(aa == 0)

drops <- list()
sapply(colname, function(x) if(all(data[[x]]) == 0) drops = append(drops, x))
# sapply(colname, function(x) data[,])
data[, sapply(colname, function(x) all(data[[x]]) != 0)]

data[, sapply(data, function(col) grepl("[:.:]missing$",col) & all(col) == 0)]
data[, !sapply(colname, function(x) all(data[[x]] == 0))]

sep_data <- iNZightTools::separate(data, col, "left", "right", key, "Column")
df = iNZightTools::aggregatedt(newdata, format, key, format)
df = iNZightTools::aggregateData(df, format, method)