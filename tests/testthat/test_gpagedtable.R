skip('in development')

# devtools::load_all()
try(dispose(w))
w <- gwindow()
g <- gvbox(container = w, fill = TRUE, expand = TRUE)
t <- gpagedtable(census.at.school.500, container = g, fill = TRUE, expand = TRUE)
