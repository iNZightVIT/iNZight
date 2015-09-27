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

    ## try to load extension packages
    for (lib in c("iNZightModules", "iNZightTS", "iNZightMR")) {
        if (lib %in% installed.packages())
            eval(parse(text = paste0("require(", lib, ", quietly = TRUE)")))
    }
}



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


####################################
## modifyList is defined again here
## because R 3.0.1 does not support the
## keep.null argument. R 3.0.2 does, so
## this can be deleted once the R version
## of the release is updated accordingly
####################################
modifyList <- function (x, val, keep.null = FALSE) 
{
    stopifnot(is.list(x), is.list(val))
    xnames <- names(x)
    vnames <- names(val)
    vnames <- vnames[vnames != ""]
    if (keep.null) {
        for (v in vnames) {
            x[v] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) 
                list(modifyList(x[[v]], val[[v]], keep.null = keep.null))
            else val[v]
        }
    }
    else {
        for (v in vnames) {
            x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && 
                is.list(val[[v]])) 
                modifyList(x[[v]], val[[v]], keep.null = keep.null)
            else val[[v]]
        }
    }
    x
}
