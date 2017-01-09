iNZImportWin <- setRefClass("iNZImportWin",
                            fields = list(
                                GUI = "ANY",
                                importFileWin = "ANY",
                                filetypes = "list",
                                fname = "ANY",
                                fext = "ANY",
                                prevGp = "ANY",
                                prevLbl = "ANY",
                                prev = "ANY",
                                tmpData = "ANY"
                            ),
                            methods = list(
                                initialize = function(gui) {
                                    initFields(GUI = gui,
                                               filetypes = list("All files" = list(patterns = c("*")),
                                                                "Comma Separated Values (.csv)" = list(patterns = c("*.csv")),
                                                                "Tab-delimited Text Files (.txt)" = list(patterns = c("*.txt")),
                                                                "SPSS Files (.sav)" = list(patterns = c("*.sav")),
                                                                "SAS Files (.???)" = list(patterns = c("*.sas")),
                                                                "97-2003 Excel Files (.xls)" = list(patterns = c("*.xls")),
                                                                "2007 Excel Files (.xlsx)" = list(patterns = c("*.xlsx"))))

                                    importFileWin <<- gwindow("Import File", parent = GUI$win, width = 600, visible = FALSE)
                                    mainGp <- gvbox(container = importFileWin)
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

                                    addHandlerChanged(filetype, generatePreview)

                                    ## Preview:
                                    prevGp <<- gframe("Preview", pos = 0, container = mainGp)
                                    #size(prevGp) <<- c(100, 150)
                                    prevGp$set_borderwidth(10)

                                    prevLbl <<- glabel("No file selected.", container = prevGp, anchor = c(1, -1))
                                    prev <<- NULL
                                    
                                    visible(importFileWin) <<- TRUE
                                },
                                ## Generate a preview
                                generatePreview = function(h, ...) {
                                    if (length(svalue(fname)) && file.exists(svalue(fname))) {
                                        if (!is.null(prev))
                                            delete(prevGp, prev)
                                        prev <<- NULL
                                        svalue(prevLbl) <<- "Loading preview ..."
                                        
                                        ## load the preview ...
                                        tmpData <<- read.table(svalue(fname), header = TRUE, nrows = 5, sep = "\t")
                                        
                                        ## set the preview
                                        Sys.sleep(2)
                                        visible(prevLbl) <<- FALSE
                                        prev <<- gtable(tmpData, container = prevGp)
                                    } else {
                                        if (!is.null(prev))
                                            delete(prevGp, prev)
                                        prev <<- NULL
                                        svalue(prevLbl) <<- "No file selected."
                                        visible(prevLbl) <<- TRUE
                                    }
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
                            
