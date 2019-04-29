library(devtools)

load_all("~/iNZightTools")
load_all("~/iNZight")

document("~/iNZightTools")

data1 = data.frame("Country" = c("A", "B", "C"), "v1999" = c("0.7K", "37K", "212K"), "v2000" = c("2K", "80K", "213K"))
data1 = data.frame("Country" = c("Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", "Brazil", "Brazil", "Brazil", "Brazil", "China", "China", "China", "China"),
               "Year" = c(1999,1999,2000,2000,1999,1999,2000,2000,1999,1999,2000,2000),
               "Type" = c("cases", "population", "cases", "population", "cases", "population", "cases", "population", "cases", "population", "cases", "population"),
               "Count" = c(745,19987071,2666,20595360,37737,172006362,80488,174504898,212258,1272915272,213766,1280428583))
data = tidyr::unite(data.frame("Country" = c("A", "B", "C"), "v1999" = c("0.7K", "37K", ""), "v2000" = c("2K", "80K", "213K")), col = "newcol", sep = "_", remove = FALSE) 

data = data.frame("B" = c("hi", "hello", "bye"), "A" = c("A-0.7K-2K/D-0.2K-3K", "B_37K", "C"))

try(dispose(kk$win), TRUE)
load_all("~/iNZight")
kk = iNZGUI$new()
kk$initializeGui(data)


## Do append column function P.S. two datasets must have the same number of rows

data1 = data.frame("col1" = 1:3, "col2" = "hi", col3 = TRUE, col4 = "bye", "xx" = 1:3)
data1
sum(grepl("^col[1-9]+$", names(data1)))
grepl("^col[1-9]+$", names(data1))
?grepl


gg = gwindow()
a = ggroup(cont = gg, horizontal = FALSE)
edit = gedit(cont = a)
addHandlerKeystroke(edit, function(h, ...) {
  print(svalue(edit))
})


new.name <- ifelse(svalue(box[i, 2]) == "", paste0("col", i), svalue(box[i, 2]))

var2 <- gedit("", cont = mainGroup)
addHandlerKeystroke(var2, function(h, ...) {
  name <<- ifelse(svalue(var2) == "", "newcol", svalue(var2))
  updateView()
})


update_name <- function(i, name) {
  force(i)
  force(name)
  
  function(h, ...) {
    new.name <- ifelse(svalue(box[i, 2]) == "", paste0("col", i), svalue(box[i, 2]))
    colnames(dtpreview)[which(names(dtpreview) == name)] <<- new.name
    newview$set_items(dtpreview)
    namelist <<- append(namelist, c(paste0("col", i), new.name))
    print(namelist)
  }
}

for (i in 1:numcol) {
  name <- paste0("col", i)
  box[i, 1] <<- glabel(paste0("Column ", i))
  box[i, 2] <<-gedit("")
  addHandlerKeystroke(box[i, 2], function(h, ...) {
    update_name(i, name)
  })
}