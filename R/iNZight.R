#' iNZight GUI
#'
#' Starts iNZight, optionally with data and a dispose function.
#' For example, a script called to open iNZight can automatically
#' exit R once the iNZight session is complete using `q()` (see examples).
#'
#' @section Addon modules:
#'
#' By default, iNZight uses the iNZightVIT install path to locate the modules directory ('modules'). However, you can override this by specifying the `addonDir` argument:
#'
#' ```
#' iNZight(addonDir = '/path/to/modules')
#' ```
#'
#' @param data a data frame
#' @param dispose_fun called when the iNZight GUI is closed
#' @param ... arguments passed to `dispose_fun`
#' @return invisibly returns the `iNZGUI` object
#' @author Marco Kuper, Tom Elliott
#' @md
#' @export
#' @examples
#' \dontrun{
#' # to have iNZight quit R without saving after the session is over,
#' # you may use the following:
#' iNZight(dispose_fun = q, save = "n")
#' }
iNZight <- function(data = NULL, dispose_fun = NULL, ...,
                    ui_env = parent.frame()) {
    gui <- iNZGUI$new()
    if (!is.null(data))
        attr(data, "name") <- deparse(substitute(data))
    gui$initializeGui(data, dispose_fun, ..., ui_env = ui_env)
    invisible(gui)
}
