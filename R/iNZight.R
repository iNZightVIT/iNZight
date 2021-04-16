#' iNZight GUI
#'
#' Starts iNZight, optionally with data and a dispose function.
#' For example, a script called to open iNZight can automatically
#' exit R once the iNZight session is complete using `q()` (see examples).
#' @param data a data frame
#' @param dispose_fun called when the iNZight GUI is closed
#' @param ... arguments passed to `dispose_fun`
#' @return NULL
#' @author Marco Kuper, Tom Elliott
#' @md
#' @export
#' @examples
#' \dontrun{
#' # to have iNZight quit R without saving after the session is over,
#' # you may use the following:
#' iNZight(dispose_fun = q, save = "n")
#' }
iNZight <- function(data = NULL, dispose_fun = NULL, ...) {
    gui <- iNZGUI$new()
    gui$initializeGui(data, dispose_fun, ...)
}
