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
                            
