.onLoad <- function(libname, pkgname) {
    opts <- options()
    inzight_opts <- list(
        inzight.disable.bootstraps = FALSE,
        inzight.lock.packages = FALSE,
        inzighttools.comment = "#"
    )
    toset <- !(names(inzight_opts) %in% names(opts))
    if (any(toset)) options(inzight_opts[toset])

    # # inzdf class
    # setClass("inzdf", contains = "structure",
    #     prototype = prototype(NULL)
    # )
}

setOldClass(c("inzdf_tbl_df", "inzdf", "tbl_df", "tbl", "data.frame"))
setOldClass(c("inzdf_sqlite", "inzdf_db", "inzdf"))

.onAttach <- function(libname, pkgname) {
    lwd <- getOption("width")
    ind <- paste(rep(" ", floor(0.05 * lwd)), collapse = "")
    header <- paste(rep("=", lwd), collapse = "")
    parwrap <- function(txt, indent = "")
        paste(strwrap(txt, prefix = ind), collapse = "\n")

    ## Ensure we're using RGtk2
    options("guiToolkit" = "RGtk2")

    packageStartupMessage(header)
    packageStartupMessage("")
    packageStartupMessage(parwrap("You have successfully loaded the iNZight package!"))
    packageStartupMessage("")
    packageStartupMessage(parwrap("To get started with iNZight simply run the following command:"))
    packageStartupMessage("")
    packageStartupMessage(parwrap("iNZight()"))
    packageStartupMessage("")
    packageStartupMessage(header)
}
