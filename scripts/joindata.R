library(devtools)
# install_github("iNZightVIT/iNZightPlots@dev")
# install_github("iNZightVIT/iNZightTools@dev")

document("~/iNZightTools")

load_all("~/iNZightTools")
load_all("~/iNZight")


data = readr::read_csv("scripts/Join2.csv")
data

try(dispose(kk$win), TRUE)
load_all("~/iNZight")
kk = iNZGUI$new()
kk$initializeGui(data)




data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin.csv") #original
data2 = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin2.csv") #has the same column "Instagram photo" with completely different values
data3 = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin3.csv") #contains a column named "sdasdsds" which has the same value as "Instagram user"
data4 = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin4.csv") #contains the same column "Instagram photo" with one value that is the same to the original one
