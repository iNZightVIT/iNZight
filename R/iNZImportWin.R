
iNZImportWin <- setRefClass("iNZImportWin",
                            fields = list(
                                GUI = "ANY",
                                importFileWin = "ANY"
                                ),
                            methods = list(
                                initialize = function(GUI) {
                                    initFields(GUI = GUI)
                                    ## set up main import window
                                    importFileWin <<- gwindow("File Browser",
                                                              parent = GUI$win)
                                    fileMainGp = ggroup(container = importFileWin, horizontal = FALSE)
                                    filetbl = glayout(container = fileMainGp)
                                    ## create list of possible file extensions
                                    l = list()
                                    l[[gettext("Tab-delimited Text files")]] = c("txt")
                                    l[[gettext("CSV files")]] = c("csv")
                                    l[[gettext("2007 Excel files")]] = c("xlsx")
                                    l[[gettext("97-2003 Excel files")]] = c("xls")
                                    l[[gettext("Infoshare CSV files")]] = c("infosharecsv")
                                    fileExtensions = l
                                    ## set up filebrowse and dropdown
                                    filterList = lapply(fileExtensions, function(i)
                                        list(patterns = paste("*.",i,sep="")))
                                    ll = list()
                                    ll$"All files " = list(patterns=c("*"))
                                    filterList = c(ll,filterList)
                                    filetbl[2,2] = glabel("Local file")
                                    filetbl[2,3] <- (filebrowse = gfilebrowse(text="Specify a file",
                                                         initial.dir = file.path(".", "data"),
                                                         action=invisible,
                                                         container=filetbl, filter=filterList, quote=FALSE))
                                    filetbl[3,2:3] <- gseparator(container = filetbl)
                                    filetbl[4,2] = gettext("File type is")
                                    filetbl[4,3] <- (filetype =
                                                     gcombobox(c("<use file extension to determine>",
                                                                 sapply(
                                                                     names(filterList[!filterList %in% ll]),
                                                                     .self$popchar)),
                                                               container = filetbl))
                                    visible(filetbl) <- TRUE
                                    ## set up the buttons
                                    buttonGp = ggroup(container = fileMainGp)
                                    addSpring(buttonGp)
                                    okButton = gbutton("OK",
                                        handler = function(h, ...) {
                                            .self$okButtonHandler(h,
                                                                  fileBrowse = filebrowse,
                                                                  fileType = filetype,
                                                                  fileExtensions = fileExtensions, ...)
                                            })
                                    cancelButton = gbutton("Cancel",
                                        handler = function(h, ...) dispose(importFileWin))
                                    add(buttonGp, okButton)
                                    add(buttonGp, cancelButton)
                                    add(fileMainGp,
                                        #glabel("Space for extra options : to define NA string, header presence etc."))
                                        glabel(""))
                                    },
                                    pop = function(x) {
                                        x[-length(x)]
                                    },
                                    popchar = function(str) {
                                        paste(.self$pop(unlist(strsplit(str,""))),collapse="")
                                    },
                                    okButtonHandler = function(h, fileBrowse, fileType,
                                        fileExtensions,...) {
                                        theFile = svalue(fileBrowse)
                                        ext = NULL ## the extension, figure out
                                        if(theFile == "Specify a file" || !file.exists(theFile)) {
                                            ## raise error
                                        }else{
                                            ## file is now theFile
                                            ## get extension type from droplist
                                            fileType = svalue(fileType)
                                            if(fileType != "<use file extension to determine>") {
                                                ## use filterList to get
                                                fileType = paste(fileType,"s", sep="", collapse="") ## append s back
                                                ext = fileExtensions[[fileType]][1]
                                                sprintf("Set extension to %s \n",ext)
                                            } else if(is.null(ext)) {
                                                tmp = unlist(strsplit(basename(theFile), split="\\."))
                                                ext = tmp[length(tmp)]
                                            }
                                            dataImportObj <- iNZDataImportExport$new()
                                            dataImportObj$importData(theFile, ext)
                                            ## if an error occured during the file import, raise a msg
                                            ## otherwise set the document in the GUI object
                                            if(dataImportObj$error$cur)
                                                gmessage(title = dataImportObj$error$title,
                                                         msg = dataImportObj$error$msg,
                                                         icon = dataImportObj$error$icon,
                                                         parent = importFileWin)
                                            else {
                                                GUI$setDocument(iNZDocument$new(data = dataImportObj$dataSet))
                                                dispose(importFileWin)
                                            }
                                        }
                                    }
                                )
                            )


iNZImportWinBeta <- setRefClass("iNZImportWinBeta",
                            fields = list(
                                GUI = "ANY",
                                importFileWin = "ANY",
                                filetypes = "list",
                                fname = "ANY",
                                fext = "ANY",
                                fColTypes = "ANY",
                                prevGp = "ANY",
                                prevLbl = "ANY",
                                prev = "ANY",
                                tmpData = "ANY",
                                advGp = "ANY",
                                delimiters = "list", csvdelim = "ANY", txtdelim = "ANY",
                                decMark = "ANY", decimalmarks = "list",
                                bigMark = "ANY", bigmarks = "list",
                                encoding = "ANY", encodings = "character",
                                dateFormat = "ANY", dateformats = "character"
                            ),
                            methods = list(
                                initialize = function(GUI) {
                                    initFields(GUI = GUI,
                                               filetypes = list("All files" = list(patterns = c("*")),
                                                                "Comma Separated Values (.csv)" = list(patterns = c("*.csv")),
                                                                "Tab-delimited Text Files (.txt)" = list(patterns = c("*.txt")),
                                                                "SPSS Files (.sav)" = list(patterns = c("*.sav")),
                                                                #"SAS Files (.sas)" = list(patterns = c("*.sas")),
                                                                "97-2003 Excel Files (.xls)" = list(patterns = c("*.xls")),
                                                                "2007 Excel Files (.xlsx)" = list(patterns = c("*.xlsx")),
                                                                "STATA Files (.dta)" = list(patterns = c("*.dta"))),
                                               fColTypes = NULL,
                                               delimiters = list("Comma (,)" = ",", "Semi-colon (;)" = ";", "Tab" = "\t"),
                                               csvdelim = ",", txtdelim = "\t",
                                               decimalmarks = list("Period (.)" = ".", "Comma (,)" = ","), decMark = ".",
                                               bigmarks = list("None" = "", "Comma (,)" = ",", "Period (.)" = "."), bigMark = "",
                                               encodings = c("UTF-8", "ISO-8859-1"), encoding = "UTF-8",
                                               dateformats = c("%Y-%m-%d", "%d/%m/%Y"), dateFormat = "%Y-%m-%d"
                                               )

                                    importFileWin <<- gwindow("Import File", parent = GUI$win, width = 600, visible = FALSE)
                                    mainGp <- gvbox(container = importFileWin, expand = TRUE, fill = TRUE)
                                    mainGp$set_borderwidth(10)

                                    ## Select file (and extension)
                                    fileGp <- gframe("Select File to Import", pos = 0, horizontal = FALSE, container = mainGp)
                                    fileGp$set_borderwidth(10)
                                    fileTbl <- glayout(container = fileGp)
                                    ii <- 1

                                    lbl <- glabel("File Name :")
                                    fname <<- gedit("", width = 40)
                                    browseBtn <- gbutton("Browse",
                                                         handler = function(h, ...) {
                                                             svalue(fname) <<- gfile(text = "Choose a file",
                                                                                     initial.dir = file.path(".", "data"),
                                                                                     filter = filetypes, quote = FALSE,
                                                                                     container = fileGp)
                                                         })
                                    fileTbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
                                    fileTbl[ii, 2:5, expand = TRUE] <- fname
                                    fileTbl[ii, 6] <- browseBtn
                                    ii <- ii + 1

                                    ## --- Extension
                                    lbl <- glabel("File Type :")
                                    filetype <- gcombobox(c(names(filetypes)[-1]), selected = 0)
                                    fileTbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
                                    fileTbl[ii, 2:5, expand = TRUE] <- filetype
                                    ii <- ii + 1


                                    ## Change handlers:
                                    addHandlerChanged(fname, function(h, ...) {
                                        fext <<- tools::file_ext(svalue(h$obj))

                                        blockHandlers(filetype)
                                        match <- which(sapply(filetypes[-1], function(ft) grepl(paste0(ft$patterns, "$"), paste0(".", fext))))
                                        svalue(filetype, index = TRUE) <- if (length(match) > 0) match else 0
                                        unblockHandlers(filetype)

                                        generatePreview(h, ...)
                                    })
                                    addHandlerKeystroke(fname, function(h, ...) h$obj$invoke_change_handler())

                                    addHandlerChanged(filetype, function(h, ...) {
                                        ## set the file extension
                                        fext <<- gsub("[*.]", "", filetypes[[svalue(h$obj, index = TRUE) + 1]]$patterns)
                                        generatePreview(h, ...)
                                    })

                                    ## Preview:
                                    prevGp <<- gframe("Preview", pos = 0, horizontal = FALSE, container = mainGp)
                                    size(prevGp) <<- c(100, 170)
                                    prevGp$set_borderwidth(10)

                                    prevLbl <<- glabel("No file selected.", container = prevGp, anchor = c(-1, 1), fill = TRUE)
                                    font(prevLbl) <<- list(size = 9)
                                    prev <<- NULL


                                    ## Advanced Import Settings
                                    advGp <<- gexpandgroup("Advanced Options", horizontal = FALSE, container = mainGp)
                                    visible(advGp) <<- FALSE


                                    addSpring(mainGp)
                                    ## Import Data!
                                    btnGp <- ggroup(container = mainGp)
                                    addSpring(btnGp)
                                    cancelBtn <- gbutton("Cancel", handler = function(h, ...) dispose(importFileWin), container = btnGp)
                                    okBtn <- gbutton("Import", handler = function(h, ...) {
                                        if (is.null(tmpData) || iNZightTools::isPreview(tmpData)) readData()

                                        ## coerce character to factor
                                        invisible(sapply(which(sapply(tmpData, class) == "character"),
                                                         function(i) tmpData[[i]] <<- factor(tmpData[[i]])))
                                        GUI$setDocument(iNZDocument$new(data = as.data.frame(tmpData, stringsAsFactors = TRUE)))

                                        ## dunno why but need to delete gdf ...
                                        #if (!is.null(prev)) delete(prevGp, prev)
                                        dispose(importFileWin)
                                    }, container = btnGp)


                                    addHandlerDestroy(importFileWin, handler = function(h, ...) {
                                        ## Not sure why but if this isn't done before the window closes,
                                        ## a GTK Critical error is thrown.
                                        if (!is.null(prev)) delete(prevGp, prev)
                                        return(TRUE)
                                    })
                                    visible(importFileWin) <<- TRUE
                                },
                                readData = function(preview = FALSE) {
                                    ## Read data using object values:
                                    tmpData <<- suppressWarnings(suppressMessages({
                                        iNZightTools::iNZread(svalue(fname), extension = fext,
                                                              preview = preview, col.types = getTypes(),
                                                              delim = switch(fext, "csv" = csvdelim, "txt" = txtdelim, NULL),
                                                              decimal.mark = decMark, grouping.mark = bigMark)
                                    }))

                                    ## do a check that col classes match requested ...
                                    if (is.null(fColTypes) || length(fColTypes) != ncol(tmpData))
                                        fColTypes <<- rep("auto", ncol(tmpData))
                                },
                                ## Generate a preview
                                generatePreview = function(h, ..., reload = FALSE) {
                                    if (length(svalue(fname)) && file.exists(svalue(fname))) {
                                        if (!is.null(prev)) {
                                            delete(prevGp, prev)
                                            prev <<- NULL
                                        }
                                        svalue(prevLbl) <<- "Loading preview ..."

                                        ## load the preview ...
                                        tryCatch({
                                            readData(preview = TRUE)
                                            ## set the preview
                                            svalue(prevLbl) <<- "Right-click column names to change the type (c = categorical, n = numeric)\n"
                                            prev <<- gdf(head(tmpData, 5), container = prevGp)
                                            invisible(prev$remove_popup_menu())
                                            invisible(prev$add_popup(function(col_index) {
                                                j <- prev$get_column_index(col_index)
                                                types <- c("auto", "numeric", "categorical")
                                                list(gradio(types,
                                                            selected = match(fColTypes[j], types),
                                                            handler = function(h, ...) {
                                                                fColTypes[j] <<- types[svalue(h$obj, index = TRUE)]
                                                                generatePreview(h, ..., reload = TRUE)
                                                            }))
                                            }))
                                            names(prev) <<- paste0(
                                                names(prev), " (",
                                                sapply(tmpData, function(x)
                                                    switch(class(x),
                                                           "integer" = , "numeric" = "n",
                                                           "factor" = , "character" = "c")),
                                                ")")
                                        },
                                        error = function(e) {
                                            svalue(prevLbl) <<- "Unable to read the file. Check the file type is correct and try again."
                                            print(e)
                                        }, finally = advancedOptions())
                                        advancedOptions()
                                    } else {
                                        if (!is.null(prev))
                                            delete(prevGp, prev)
                                        prev <<- NULL
                                        svalue(prevLbl) <<- "No file selected."
                                        visible(prevLbl) <<- TRUE
                                    }
                                },
                                getTypes = function() {
                                    if (is.null(fColTypes) || all(fColTypes == "auto")) return(NULL)
                                    sapply(fColTypes, function(x) switch(x, "categorical" = "factor", x))
                                },
                                advancedOptions = function() {
                                    ## populate the Advanced Options panel (advGp) with extra options for various data sets.
                                    ## but first, delete the old one ...
                                    if (length(advGp$children)) sapply(advGp$children, function(ch) advGp$remove_child(ch))

                                    ## build it up!
                                    tbl <- glayout(container = advGp)
                                    ii <- 1

                                    switch(fext,
                                           "csv" =, "txt" = {
                                               ## ----------------- LEFT HAND SIDE
                                               ## --- DELIMITER
                                               lbl <- glabel("Delimiter :")
                                               ## add custom choices ...
                                               delimOpt <- gcombobox(names(delimiters),
                                                                     selected = which(sapply(delimiters, function(x) get(paste0(fext, "delim")) == x)),
                                                                     editable = FALSE,
                                                                     handler = function(h, ...) {
                                                                         if (fext == "txt") txtdelim <<- delimiters[[svalue(h$obj, index = TRUE)]]
                                                                         else csvdelim <<- delimiters[[svalue(h$obj, index = TRUE)]]
                                                                         ## changing delimiter == changing where columns are
                                                                         fColTypes <<- NULL
                                                                         generatePreview(h, ...)
                                                                     })
                                               tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
                                               tbl[ii, 2:3, expand = TRUE] <- delimOpt
                                               ii <- ii + 1

                                               ## --- DECIMAL MARK
                                               lbl <- glabel("Decimal Mark :")
                                               decMarkOpt <- gcombobox(names(decimalmarks),
                                                                       selected = match(decMark, decimalmarks), ##which(sapply(decimalmarks, function(x) decMark == x)),
                                                                       handler = function(h, ...) {
                                                                           decMark <<- decimalmarks[[svalue(h$obj, index = TRUE)]]
                                                                           ## Do not allow value to be same as thousands separator!
                                                                           if (decMark == bigMark)
                                                                               gmessage("Decimal mark and thousands separator must be different.",
                                                                                        type = "error")
                                                                           else
                                                                               generatePreview(h, ...)
                                                                       })
                                               tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
                                               tbl[ii, 2:3, expand = TRUE] <- decMarkOpt
                                               ii <- ii + 1

                                               ## --- THOUSANDS SEPARATOR
                                               lbl <- glabel("Thousands Separator :")
                                               bigMarkOpt <- gcombobox(names(bigmarks),
                                                                       selected = match(bigMark, bigmarks), ##which(sapply(bigmarks, function(x) bigMark == x)),
                                                                       handler = function(h, ...) {
                                                                           bigMark <<- bigmarks[[svalue(h$obj, index = TRUE)]]
                                                                           ## Do not allow value to be same as thousands separator!
                                                                           if (decMark == bigMark)
                                                                               gmessage("Decimal mark and thousands separator must be different.",
                                                                                        type = "error")
                                                                           else
                                                                               generatePreview(h, ...)
                                                                       })
                                               tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
                                               tbl[ii, 2:3, expand = TRUE] <- bigMarkOpt
                                               ii <- ii + 1

                                               ## --- FILE ENCODING
                                               lbl <- glabel("File Encoding :")
                                               encOpt <- gcombobox(encodings,
                                                                   selected = match(encoding, encodings),
                                                                   handler = function(h, ...) {
                                                                       encoding <<- svalue(h$obj)
                                                                       generatePreview(h, ...)
                                                                   })
                                               tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
                                               tbl[ii, 2:3, expand = TRUE] <- encOpt
                                               ii <- ii + 1


                                               ## ----------------- RIGHT HAND SIDE
                                               ii <- 1

                                               ## --- DATE FORMAT
                                               ## this should be a drop down of some common formats (2016-01-16, 16 Jan 2016, 16/01/16, 01/16/16, ...)
                                               lbl <- glabel("Date Format :")
#                                               dateFmt <- gcombobox()


                                           }, {
                                               lbl <- glabel("No options available for this file type.")
                                               tbl[ii, 1, anchor = c(-1, 0), expand = TRUE] <- lbl
                                           })
                                },
                                closeHandler = function(h, ...) {
                                    #addHandlerUnrealize(importFileWin, handler = function(h, ...) {
                                    print("closing now")
                                    if (!is.null(prev)) delete(prevGp, prev)
                                    return(TRUE)
                                    #})
                                },

                                initialize_old= function(GUI) {
                                    initFields(GUI = GUI)
                                    ## set up main import window
                                    importFileWin <<- gwindow("File Browser",
                                                              parent = GUI$win)
                                    fileMainGp = ggroup(container = importFileWin, horizontal = FALSE)
                                    filetbl = glayout(container = fileMainGp)
                                    ## create list of possible file extensions
                                    l = list()
                                    l[[gettext("Tab-delimited Text files")]] = c("txt")
                                    l[[gettext("CSV files")]] = c("csv")
                                    l[[gettext("2007 Excel files")]] = c("xlsx")
                                    l[[gettext("97-2003 Excel files")]] = c("xls")
                                    l[[gettext("Infoshare CSV files")]] = c("infosharecsv")
                                    fileExtensions = l
                                    ## set up filebrowse and dropdown
                                    filterList = lapply(fileExtensions, function(i)
                                        list(patterns = paste("*.",i,sep="")))
                                    ll = list()
                                    ll$"All files " = list(patterns=c("*"))
                                    filterList = c(ll,filterList)
                                    filetbl[2,2] = glabel("Local file")
                                    filetbl[2,3] <- (filebrowse = gfilebrowse(text="Specify a file",
                                                         initial.dir = file.path(".", "data"),
                                                         action=invisible,
                                                         container=filetbl, filter=filterList, quote=FALSE))
                                    filetbl[3,2:3] <- gseparator(container = filetbl)
                                    filetbl[4,2] = gettext("File type is")
                                    filetbl[4,3] <- (filetype =
                                                     gcombobox(c("<use file extension to determine>",
                                                                 sapply(
                                                                     names(filterList[!filterList %in% ll]),
                                                                     .self$popchar)),
                                                               container = filetbl))
                                    visible(filetbl) <- TRUE
                                    ## set up the buttons
                                    buttonGp = ggroup(container = fileMainGp)
                                    addSpring(buttonGp)
                                    okButton = gbutton("OK",
                                        handler = function(h, ...) {
                                            .self$okButtonHandler(h,
                                                                  fileBrowse = filebrowse,
                                                                  fileType = filetype,
                                                                  fileExtensions = fileExtensions, ...)
                                            })
                                    cancelButton = gbutton("Cancel",
                                        handler = function(h, ...) dispose(importFileWin))
                                    add(buttonGp, okButton)
                                    add(buttonGp, cancelButton)
                                    add(fileMainGp,
                                        glabel(""))
                                    },
                                    pop = function(x) {
                                        x[-length(x)]
                                    },
                                    popchar = function(str) {
                                        paste(.self$pop(unlist(strsplit(str,""))),collapse="")
                                    },
                                    okButtonHandler = function(h, fileBrowse, fileType,
                                        fileExtensions,...) {
                                        theFile = svalue(fileBrowse)
                                        ext = NULL ## the extension, figure out
                                        if(theFile == "Specify a file" || !file.exists(theFile)) {
                                            ## raise error
                                        }else{
                                            ## file is now theFile
                                            ## get extension type from droplist
                                            fileType = svalue(fileType)
                                            if(fileType != "<use file extension to determine>") {
                                                ## use filterList to get
                                                fileType = paste(fileType,"s", sep="", collapse="") ## append s back
                                                ext = fileExtensions[[fileType]][1]
                                                sprintf("Set extension to %s \n",ext)
                                            } else if(is.null(ext)) {
                                                tmp = unlist(strsplit(basename(theFile), split="\\."))
                                                ext = tmp[length(tmp)]
                                            }
                                            dataImportObj <- iNZDataImportExport$new()
                                            dataImportObj$importData(theFile, ext)
                                            ## if an error occured during the file import, raise a msg
                                            ## otherwise set the document in the GUI object
                                            if(dataImportObj$error$cur)
                                                gmessage(title = dataImportObj$error$title,
                                                         msg = dataImportObj$error$msg,
                                                         icon = dataImportObj$error$icon,
                                                         parent = importFileWin)
                                            else {
                                                GUI$setDocument(iNZDocument$new(data = dataImportObj$dataSet))
                                                dispose(importFileWin)
                                            }
                                        }
                                    }
                                )
                            )
