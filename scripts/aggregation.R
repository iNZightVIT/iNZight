ts = data.frame("ts" = c("2017M05", "2011M12", "1997M02"))


ts1 = c("2017M05", "2011M12", "1997M02")
for (i in 1:length(ts1)) {
  ts1[i] = paste0(strsplit(ts1[i], "M")[[1]][1], "Q", ceiling(as.numeric(strsplit(ts1[i], "M")[[1]][2])/3))
}
ts1