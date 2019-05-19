library(devtools)
# install_github("iNZightVIT/iNZightPlots@dev")
# install_github("iNZightVIT/iNZightTools@dev")

document("~/iNZightTools")

load_all("~/iNZightTools")
load_all("~/iNZight")



data = readr::read_csv("scripts/Join.xlsx")
data = readr::read_csv("C:\\Users\\30576\\Documents\\quakes_play.csv")
data = data.frame("x1" = c("1", "2", "3"))


try(dispose(kk$win), TRUE)
load_all("~/iNZight")
kk = iNZGUI$new()
kk$initializeGui(data)

class(data$x4)

data = data.frame("x1" = c(1, 2, 3), "x2" = c("A", "B", "C"))

data = readr::read_csv("C:\\Users\\Yiwen\\Documents\\join.csv") #original
data2 = readr::read_csv("C:\\Users\\Yiwen\\Documents\\join2.csv") #has the same column "Instagram photo" with completely different values
data3 = readr::read_csv("C:\\Users\\Yiwen\\Documents\\parisjoin3.csv") #contains a column named "sdasdsds" which has the same value as "Instagram user"
data4 = readr::read_csv("C:\\Users\\Yiwen\\Documents\\join4.csv") #contains the same column "Instagram photo" with one value that is the same to the original one

?gtree

xx = gwindow()
aa = ggroup(cont = xx)
table = gtable(c("Sum", "Mean", "Median"), cont = aa)
addHandlerSelectionChanged(table, handler = function(h, ...) {
  print(svalue(table))
})
addHandlerColumnRightclicked()
addhandler