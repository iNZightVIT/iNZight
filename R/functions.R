

#' Export not-in operator
#' @importFrom iNZightTools "%notin%"
#' @name %notin%
#' @rdname notin-operator
#' @export
NULL

iNZSaveFile <- function(theFile, ext, ...) {
    ###################################
    ### generic function to save an object
    ## theFile: file name to be saved, possible with path
    ## ext: the desired extension of the file
    ## which: from which device to save (only for plots)
    ## data: data.frame to save (only for datasets)
    ##       one of which/data needs to be present
    ## ... further arguments like fileType to distinguish
    ##     between different .txt files
    ###################################
    args = list(...)
    if (is.null(args$which) && is.null(args$data))
        return(list(msg = "What to save not specified"))
    ## device number 1 is null device (empty)
    if (!is.null(args$which) && args$which == 1)
        return(list(msg = "There is no plot to save"))
    ## Determine whether a user has specified a file just by name
    ## rather than using the file browser
    dirsep <- if (.Platform$OS.type == "windows") "[\\]" else "/"
    ## In the case that a user has given a filename rather than
    ## a file path, set the save location to the current working dir
    if (length(strsplit(theFile, dirsep)[[1]]) == 1)
        theFile <- paste(getwd(), theFile, sep = .Platform$file.sep)
    tmp <- unlist(strsplit(basename(theFile), split="\\.")) # split on dots
    ext.tmp <- tmp[length(tmp)] # take the string after last dot
    if (length(ext) == 0)
        list(msg = "Invalid extension")
    else if (ext.tmp != ext)
        ## if the specified ext is not attached to thefile, attach it
        theFile <- paste(theFile, ext, sep = ".")

    ## change the class of theFile and then use S3 to get correct fn
    class(theFile) <- ext
    .iNZSaveFile(theFile, ext, ...)
}

.iNZSaveFile <- function(theFile, ext, ...)
    UseMethod('.iNZSaveFile')
## create a function for every file extension
.iNZSaveFile.default <- function(theFile, ext, ...) {
    ## do some default behaviour
}

.iNZSaveFile.pdf <- function(theFile, ext, ...) {
    which <- list(...)$which
    curDev <- dev.cur()
    dev.set(which)
    dev.copy2pdf(file = theFile)
    dev.set(curDev)
    TRUE
}

.iNZSaveFile.png <- function(theFile, ext, ...) {
    which <- list(...)$which
    devwidth <- 640
    devheight <- 640
    curDev <- dev.cur()
    dev.set(which)
    dev.copy(png, file = theFile, width = devwidth, height = devheight)
    tmp <- dev.off()
    dev.set(curDev)
    TRUE
}

.iNZSaveFile.bmp <- function(theFile, ext, ...) {
    which <- list(...)$which
    devwidth <- 640
    devheight <- 640
    curDev <- dev.cur()
    dev.set(which)
    dev.copy(bmp, file = theFile, width = devwidth, height = devheight)
    tmp <- dev.off()
    dev.set(curDev)
    TRUE
}

.iNZSaveFile.tiff <- function(theFile, ext, ...) {
    which <- list(...)$which
    devwidth <- 640
    devheight <- 640
    curDev <- dev.cur()
    dev.set(which)
    dev.copy(tiff, file = theFile, width = devwidth, height = devheight)
    tmp <- dev.off()
    dev.set(curDev)
    TRUE
}

.iNZSaveFile.jpg <- function(theFile, ext, ...) {
    which <- list(...)$which
    devwidth <- 640
    devheight <- 640
    curDev <- dev.cur()
    dev.set(which)
    dev.copy(jpeg, file = theFile, width = devwidth, height = devheight)
    tmp <- dev.off()
    dev.set(curDev)
    TRUE
}

.iNZSaveFile.jpeg <- function(theFile, ext, ...) {
    which <- list(...)$which
    devwidth <- 640
    devheight <- 640
    curDev <- dev.cur()
    dev.set(which)
    dev.copy(jpeg, file = theFile, width = devwidth, height = devheight)
    tmp <- dev.off()
    dev.set(curDev)
    TRUE
}

.iNZSaveFile.csv <- function(theFile, ext, ...) {
    data <- list(...)$data
    write.csv(data, file = theFile, row.names = FALSE)
    TRUE
}

.iNZSaveFile.txt <- function(theFile, ext, ...) {
    data <- list(...)$data
    filetype <- list(...)$fileType
    if (filetype == 4)
        sep = " "
    else
        sep = "\t"
    write.table(data, file = theFile, sep = sep, row.names = FALSE)
    TRUE
}

.iNZSaveFile.rda <- function(theFile, ext, data, dataname, ...) {
    assign(dataname, data)
    save(list = dataname, file = theFile)
    TRUE
}

construct_call <- function(settings, model, vartypes,
                           data = quote(.dataset),
                           design = quote(.design),
                           what = c("plot", "summary", "inference")) {
    if (is.null(model$dataDesign)) design <- NULL
    iNZightPlots:::construct_call(settings, vartypes, data, design, what)
}

# a very roundabout way to get the code correct ...
mend_call <- function(call, gui) {
    iNZightPlots:::mend_call(
        call,
        gui$getActiveData(lazy = TRUE),
        gui$getActiveDoc()$getModel()$dataDesignName,
        gui$curPlot
    )
}


.base_url <- "https://inzight.nz/"
help_page <- function(path)
    browseURL(paste0(.base_url, path))


spec_char <- function(code) {
    win <- grepl("Windows", R.Version()$os)
    switch(code,
        "lte" = if (win) "<=" else "\U2264",
        "gte" = if (win) ">=" else "\U2265",
        ""
    )
}

center_window <- function(w) {
    window <- w$widget$window
    window_size <- size(w)
    window_screen <- gtkWindowGetScreen(w$widget)
    monitor <- gdkScreenGetMonitorAtWindow(window_screen, window)
    monitor_dim <- unlist(
        gdkScreenGetMonitorGeometry(window_screen, monitor)$dest[c("width", "height")]
    )

    win_pos <- monitor_dim / 2 - window_size / 2L
    gtkWindowMove(w$widget, win_pos[1], win_pos[2])
}

`%||%` <- function(a, b)
    if (is.null(a)) b else a
