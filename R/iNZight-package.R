##' @import methods grid RGtk2 gdata gWidgets2RGtk2 cairoDevice iNZightPlots reshape2 plyr survey
##' @importFrom iNZightTools newdevice
##' @importClassesFrom gWidgets2RGtk2 GToolBar
NULL


##' iNZight GUI
##'
##' Starts iNZight
##' @title iNZight GUI
##' @param data a data frame
##' @param disposeR logical, if \code{TRUE}, R will quit when iNZight is closed.
##' @return NULL
##' @author Marco Kuper
##' @export
iNZight <- function(data = NULL, disposeR = FALSE) {
    gui <- iNZGUI$new()
    gui$initializeGui(data, disposeR)
}
