library(devtools)
# install_github("iNZightVIT/iNZightPlots@dev")
# install_github("iNZightVIT/iNZightTools@dev")

document("~/iNZightTools")

load_all("~/iNZightTools")
load_all("~/iNZight")


data2 = readr::read_csv("scripts/Join2.csv")
data

try(dispose(kk$win), TRUE)
load_all("~/iNZight")
kk = iNZGUI$new()
kk$initializeGui(data)




data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin.csv") #original
data2 = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin2.csv") #has the same column "Instagram photo" with completely different values
data3 = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin3.csv") #contains a column named "sdasdsds" which has the same value as "Instagram user"
data4 = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin4.csv") #contains the same column "Instagram photo" with one value that is the same to the original one


data <- data.frame("x1" = c("A", "B", "D"))


colnames(data)[which(names(data) == colname)] <- paste0(colname, class(data[[colname]]))



cols_in_common <- intersect(colnames(data), colnames(data2))
orig_type <- sapply(data[, cols_in_common, drop = FALSE], class)
new_type <- sapply(data2[, cols_in_common, drop = FALSE], class)
orig_type == new_type
orig_type
new_type
