## we're using these libraries for the GUI
library(gWidgets2)
library(gWidgets2RGtk2)


## iNZight essentially creates a window, and passes the data to it,
win <- gwindow(title = "Time conversion window")
## inside is a "group", which keeps the contents together
g <- gvbox(container = win)

## so first, load some data and call it "dat"
dat <- data.frame(x = 1, ts = Sys.time())


## Now we just add "widgets" to the window group, `g`
title <- glabel("Convert to date time", container = g)
font(title) <- list(size = 14, weight = "bold")

## spacing is easy ...
addSpace(g, 5)


### choose a variable 
## but first, a label!
## we want it go be left aligned, so set anchor to -1 (-1 = left, 0 = center, 1 = right)
## (anchor takes two values - c(left/right, top/bottom)
lab.var <- glabel("Select a variable to convert", container = g,
                  anchor = c(-1, 0))

## now add a drop down to display the options
var <- gcombobox(items = names(dat), container = g)

addSpace(g, 5)


### then you'll need another label and dropdown for the possible formats
dt.formats <- c("some format", "another", "...")

## label

## dropdown



### and finally an OK button
okbtn <- gbutton("Convert", container = g, handler = function(h, ...) {
    ## here we write code that get excuted when the button gets clicked

    ## the NAME of the chosen variable is
    varname <- svalue(var)

    ## the actual DATA is obtained by subsetting
    ## (this is the same as dat$x, but lets you use a variable instead)
    varx <- dat[[varname]]

    ## now you can grab the format value from the dropdown you created
    fmt <- "some format"
    
    ## and call your conversion function
    yourfun <- function(x, format) x ## this is a dummy so the code works
    var.dt <- yourfun(varx, format = fmt)

    ## and then update the dataset - but iNZight has its own ways of doing this,
    ## which we will worry about
    message("You just converted ", varname, " to a date time.")

    print(var.dt)

    ## we done? close the window
    dispose(win)
})

