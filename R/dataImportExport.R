iNZDataImportExport <- setRefClass(
    "iNZDataImportExport",
    fields = list(
        dataSet = "data.frame",
        error = "list"
        ),
    methods = list(
        initialize = function() {
            error <<- list(cur = FALSE)
        },
        importData = function(theFile, ext) {
            tmp = unlist(strsplit(basename(theFile), split="\\."))
            dataset.name = paste(tmp[-length(tmp)], collapse = ".")
            ext.tmp = tmp[length(tmp)]                              
            if(length(ext) == 0)
                setError("Check file type", "Error")
            else if(ext.tmp != ext)
                setError("Chosen file is different than the selected file type",
                         "Error")
            else if (ext %in% c("txt", "csv", "infosharecsv")) {
                if (ext == "txt")
                    out <- try(read.table(theFile, header = TRUE, sep = "\t",
                                          na.strings = c("NULL","NA",
                                              "N/A","#N/A","","<NA>"),
                                          check.names = TRUE))
                else if (ext == "csv")
                    out <- try(read.csv(theFile, header = TRUE,
                                        na.strings = c("NULL","NA","N/A",
                                            "#N/A","","<NA>"),
                                        comment.char = "#",
                                        check.names = TRUE))
                else if (ext == "infosharecsv")
                    out <- try(read.infoshare(theFile))
                if (inherits(out, "try-error")) {
                    setError(sprintf("Error loading file: %s\n", basename(theFile)),
                             "Error loading data file")
                } else {
                    attr(out, "name") <- dataset.name ## associate a name with dataSet
                    dataSet <<- out
                }
            } else if (ext %in% c("xls", "xlsx")) {
                if ("RODBC" %in% row.names(installed.packages()))
                    try(require(RODBC), silent = TRUE)
                
                if (ext == "xls") {
                    channel <-  if (exists("odbcConnectExcel"))
                        try(RODBC::odbcConnectExcel(theFile,
                                                    readOnly = TRUE,
                                                    readOnlyOptimize=TRUE))
                    else
                        NULL
                    excelString <- "Excel"
                } else if (ext == "xlsx") {
                    channel <- if (exists("odbcConnectExcel2007"))
                        try(RODBC::odbcConnectExcel2007(theFile,
                                                        readOnly = TRUE,
                                                        readOnlyOptimize=TRUE))
                    else
                        NULL
                    excelString <- "Excel (>= 2007)"
                }
                if (is.null(channel) || inherits(channel, "try-error")) {
                    setError(paste(sprintf("Error loading file: %s\n", basename(theFile)),
                                   paste("Is", excelString, "present on this system?"),
                                   sep = "\n"),
                             "Error loading Excel file")
                    if (exists("odbcCloseAll"))
                        odbcCloseAll()
                } else {                   
                    out <- try(sqlFetch(channel, sqtable = "Sheet1",
                                        na.strings = c("NULL","NA","N/A","#N/A","","<NA>"),
                                        as.is = TRUE)) #no na.omit()
                    if(inherits(out,"try-error")){
                        setError("Please ensure that the Excel worksheet containing the data is named as Sheet1\n\nIf the error persists, please save the dataset as a CSV (comma separated) file")
                    } else {
                        ## convert character to factor
                        out <- lapply(out, function(x) {
                            if(all(is.na(x)))
                                as.factor(as.character(x))
                            else
                                x})
                        out <- as.data.frame(out)
                        
                        attr(out, "name") <- dataset.name ## associate a name with dataSet
                        dataSet <<- out                       
                        odbcCloseAll()
                    }
                }
            }                                                                  
        },
        setError = function(msg, title=msg, icon = "error") {
            error <<- list(cur = TRUE, title = title,
                           icon = icon, msg = msg)
        })
    )
