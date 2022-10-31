cat("\n=== checking if slider crashes things ...\n")

library(gWidgets2)

cat("\n- window\n")
w <- gwindow()
cat("\n- group\n")
g <- ggroup(container = w)
cat("\n- slider\n")
s <- gslider(container = g)
cat("\n- complete ...\n")
