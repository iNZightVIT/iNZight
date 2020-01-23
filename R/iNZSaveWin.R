iNZSaveWin <- setRefClass(
    "iNZSaveWin",
    fields = list(
        GUI = "ANY",
        saveFileWin = "ANY"
        ),
    methods = list(
        initialize = function(gui, type = c("plot", "data"), ...) {
            initFields(GUI = gui)
            if(!(type %in% c("plot", "data")))
                gmessage("Can't save this type of object")
            else {
                saveFileWin <<- gwindow("File Browser", parent = GUI$win)
                fileMainGp <- ggroup(container = saveFileWin,
                                     horizontal = FALSE)
                filetbl <- glayout(container = fileMainGp)

                l <- list(plot = list(), data = list())
                l$plot[[gettext("Bitmap Image (BMP)")]] <- "bmp"
                l$plot[[gettext("JPEG Image (JPG)")]] <- "jpg"
                l$plot[[gettext("Portable Document Format (PDF)")]] <- "pdf"
                l$plot[[gettext("PNG Image (PNG)")]] <- "png"
                l$plot[[gettext("TIFF Image (TIFF)")]] <- "tiff"
                l$data[[gettext("Comma Separated Values (CSV)")]] <- "csv"
                l$data[[gettext("Tab Separated Values (TXT)")]] <- "txt"
                l$data[[gettext("Space Separated Values (TXT)")]] <- "txt"
                l$data[[gettext("RData (RDA)")]] <- "rda"

                fileExtensions <- l[[type]]
                pop <- function(x) x[-length(x)]
                popchar <- function(str) paste(pop(unlist(strsplit(str, ""))),
                                               collapse = "")

                filterList <- lapply(fileExtensions, function(i)
                                     list(patterns = paste("*.", i, sep = "")))

                ll = list()
                ll$"All files " <- list(patterns = "*")
                filterList <- c(ll, filterList)

                filetbl[2, 2] <- glabel("Local file")
                filetbl[2, 3] <- (filebrowse <- gfilebrowse(
                    text = "Specify a file",
                    action = invisible,
                    type = "save",
                    container = filetbl,
                    filter = filterList,
                    quote = FALSE))
                filetbl[3, 2:3] <- gseparator(container = filetbl)
                filetbl[4, 2] = gettext("File type is")
                filetbl[4, 3] <- (filetype = gcombobox(
                                      c("<use file extension to determine>",
                                        names(filterList[!filterList %in% ll])),
                                      container = filetbl))
                buttonGp <- ggroup(container = fileMainGp)
                addSpring(buttonGp)
                extra.args <- list(...)
                okButton <- gbutton("OK",
                                    handler = function(h, ...) {
                                        l1 <- list(
                                            type = type,
                                            fileBrowse = filebrowse,
                                            filetype = filetype,
                                            fileExtensions = fileExtensions,
                                            dataname = attr(gui$getActiveData(), "name")
                                            )
                                        l2 <- modifyList(l1, extra.args)
                                        do.call(.self$okButtonHandler, l2)
                                    })
                cancelButton <- gbutton("Cancel",
                                        handler = function(h,...)  cancelButtonHandler())
                add(buttonGp, okButton)
                add(buttonGp, cancelButton)
            }
        },
        okButtonHandler = function(type, fileBrowse, filetype,
            fileExtensions, ...) {
            theFile <- svalue(fileBrowse)
            ext <- NULL ## the extension, figure out
            ## list of possible extensions for each type
            poss.ext <- list(plot = c("pdf", "png", "jpg", "jpeg",
                                 "tiff", "bmp"),
                             data = c("csv", "txt"))
            ## list of default extensions for each type
            def.ext <- list(plot = "png", data = "csv")
            if (theFile != "Specify a file") {
                fileType <- svalue(filetype)
                if (fileType != "<use file extension to determine>") {
                    ext <- fileExtensions[[fileType]][1]
                } else if (is.null(ext)) {
                    tmp <- unlist(strsplit(basename(theFile), split="\\."))
                    ext <- tolower(tmp[length(tmp)])
                    ## In the case where we aren't able to assign a usable
                    ## file extension, assume default
                    if (! ext %in% poss.ext[[type]])
                        ext <- def.ext[[type]]
                }
                result <- iNZSaveFile(theFile, ext,
                                      fileType = svalue(filetype, index=TRUE),
                                      ...)
                if(is.logical(result) && result)
                    dispose(saveFileWin)
                else {
                    if (is.list(result))
                        msg <- result$msg
                    else
                        msg <- "Could not save file"
                    gmessage(msg, icon="error")
                }
            }
        },
        cancelButtonHandler = function() {
            dispose(saveFileWin)
        })
    )
