#' iNZight GUI
#'
#' Starts iNZight
#' @title iNZight GUI
#' @param data a data frame
#' @param disposeR logical, if \code{TRUE}, R will quit when iNZight is closed.
#' @return NULL
#' @author Marco Kuper
#' @export
iNZight <- function(data = NULL, disposeR = FALSE) {
    gui <- iNZGUI$new()
    gui$initializeGui(data, disposeR)
}
