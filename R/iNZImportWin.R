iNZImportWin <- setRefClass(
    "iNZImportWin",
    fields = list(
        GUI = "ANY",
        importFileWin = "ANY",
        loadURL = "ANY",
        filetypes = "list",
        fileTbl = "ANY",
        fname = "character", filename = "ANY", fileurl = "ANY",
        browseBtn = "ANY",
        fext = "ANY",
        filetype = "ANY",
        fColTypes = "ANY",
        rdaName = "ANY",
        rdaLabel = "ANY",
        prevGp = "ANY",
        prevLbl = "ANY",
        prev = "ANY",
        tmpData = "ANY",
        advGp = "ANY",
        delimiters = "list", csvdelim = "ANY", txtdelim = "ANY",
        decMark = "ANY", decimalmarks = "list",
        bigMark = "ANY", bigmarks = "list",
        encoding = "ANY", encodings = "character",
        dateFormat = "ANY", dateformats = "character",
        okBtn = "ANY", cancelBtn = "ANY"
    ),
    methods = list(
        initialize = function(GUI) {
            initFields(
                GUI = GUI,
                filetypes = list(
                    "All files" = list(patterns = c("*")),
                    "Comma Separated Values (.csv)" = list(patterns = c("*.csv")),
                    "Tab-delimited Text Files (.txt)" = list(patterns = c("*.txt")),
                    "SPSS Files (.sav)" = list(patterns = c("*.sav")),
                    "SAS Data Files (.sas7bdat)" = list(patterns = c("*.sas7bdat")),
                    "SAS XPORT Files (.xpt)" = list(patterns = c("*.xpt")),
                    "97-2003 Excel Files (.xls)" = list(patterns = c("*.xls")),
                    "2007 Excel Files (.xlsx)" = list(patterns = c("*.xlsx")),
                    "STATA Files (.dta)" = list(patterns = c("*.dta")),
                    "R Object (.rds)" = list(patterns = c("*.rds")),
                    "RData Files (.RData, .rda)" = list(patterns = c("*.RData", "*.rda"))
                ),
                fColTypes = NULL,
                rdaName = NULL,
                delimiters = list(
                    "Comma (,)" = ",",
                    "Semi-colon (;)" = ";",
                    "Tab" = "\t"
                ),
                csvdelim = ",",
                txtdelim = "\t",
                decimalmarks = list("Period (.)" = ".", "Comma (,)" = ","),
                decMark = ".",
                bigmarks = list(
                    "None" = "",
                    "Comma (,)" = ",",
                    "Period (.)" = "."
                ),
                bigMark = "",
                encodings = c("UTF-8", "ISO-8859-1"),
                encoding = "UTF-8",
                dateformats = c("%Y-%m-%d", "%d/%m/%Y"),
                dateFormat = "%Y-%m-%d"
            )

            importFileWin <<- gwindow("Import File",
                parent = GUI$win,
                width = 600,
                visible = FALSE
            )
            mainGp <- gvbox(
                container = importFileWin,
                expand = TRUE,
                fill = TRUE
            )
            mainGp$set_borderwidth(10)

            glabel(paste(sep = "\n",
                "Please let us know if you have difficulty importing data (if you can include the data",
                "that would extremely helpful). Email: inzight_support@stat.auckland.ac.nz"),
                container = mainGp,
                fill = TRUE
            )

            ## Select file (and extension)
            fileGp <- gframe("Select File to Import",
                pos = 0,
                horizontal = FALSE,
                container = mainGp
            )
            fileGp$set_borderwidth(10)
            fileTbl <<- glayout(container = fileGp)
            ii <- 1

            lbl <- glabel("File Name :")
            font(lbl) <- list(weight = "bold")
            filename <<- glabel("")
            browseBtn <<- gbutton("Browse",
                handler = function(h, ...) {
                    fname <<- gfile(
                        text = "Choose a file",
                        initial.dir = file.path(".", "data"),
                        filter = filetypes,
                        quote = FALSE,
                        container = fileGp
                    )
                    setfile(h, ...)
                }
            )
            fileTbl[ii, 1, anchor = c(1, 0)] <<- lbl
            font(lbl) <- list(weight = "bold")
            fileTbl[ii, 2:4, expand = TRUE, anchor = c(1, 0)] <<- filename
            fileTbl[ii, 5] <<- browseBtn
            ii <- ii + 1

            ## --- URL?
            loadURL <<- gcheckbox("Import from URL", checked = FALSE)
            fileTbl[ii, 2:5, expand = TRUE, anchor = c(-1, 0)] <<- loadURL
            ii <- ii + 1

            ## --- Extension
            lbl <- glabel("File Type :")
            font(lbl) <- list(weight = "bold")
            filetype <<- gcombobox(c(names(filetypes)[-1]), selected = 0)
            fileTbl[ii, 1, anchor = c(1, 0)] <<- lbl
            fileTbl[ii, 2:5, expand = TRUE] <<- filetype
            ii <- ii + 1


            ## Change handlers:
            addHandlerChanged(filetype,
                function(h, ...) {
                    ## set the file extension
                    fext <<- gsub("[*.]", "",
                        filetypes[[svalue(h$obj, index = TRUE) + 1]]$patterns[1]
                    )
                    generatePreview(h, ...)
                }
            )
            addHandlerChanged(loadURL,
                function(h, ...) {
                    ## Switch to loading a URL
                    fileTbl[1,1]$set_value(
                        ifelse(svalue(loadURL), "File URL :", "File Name :")
                    )
                    visible(browseBtn) <<- !svalue(loadURL)
                    if (svalue(loadURL)) {
                        delete(fileTbl, fileTbl[1, 2])
                        fileurl <<- gedit(text = "https://", width = 40)
                        fileTbl[1, 2:5, expand = TRUE] <<- fileurl
                        addHandlerChanged(fileurl,
                            function(h, ...) {
                                fname <<- svalue(fileurl)
                                setfile()
                                generatePreview(h, ...)
                            }
                        )
                    } else {
                        delete(fileTbl, fileTbl[1, 2])
                        filename <<- glabel("")
                        fileTbl[1, 2:4, expand = TRUE, anchor = c(1, 0)] <<- filename
                    }
                }
            )

            ## Preview:
            prevGp <<- gframe("Preview",
                pos = 0,
                horizontal = FALSE,
                container = mainGp
            )
            size(prevGp) <<- c(100, 170)
            prevGp$set_borderwidth(10)

            prevLbl <<- glabel("No file selected.",
                container = prevGp,
                anchor = c(-1, 1),
                fill = TRUE
            )
            font(prevLbl) <<- list(size = 9)
            prev <<- NULL


            ## Advanced Import Settings
            advGp <<- gexpandgroup("Advanced Options",
                horizontal = FALSE,
                container = mainGp
            )
            visible(advGp) <<- FALSE


            addSpring(mainGp)
            ## Import Data!
            btnGp <- ggroup(container = mainGp)
            addSpring(btnGp)
            cancelBtn <<- gbutton("Cancel",
                handler = function(h, ...) dispose(importFileWin),
                container = btnGp
            )
            okBtn <<- gbutton("Import",
                handler = function(h, ...) {
                    infw <- gwindow("Loading data ...", width = 320, height = 80, visible = FALSE, parent = GUI$win)
                    infg <- gvbox(container = infw)
                    addSpace(infg, 10)
                    infl <- glabel(
                        "Please wait while iNZight loads your data.\nIt might take some time depending on the size.",
                        container = infg,
                        anchor = c(0, -1)
                    )
                    font(infl) <- list(weight = "bold")
                    visible(infw) <- TRUE

                    ## without this, the text doesn't load before the next call is made,
                    ## which means the message is pointless ...
                    Sys.sleep(0.1)

                    if (is.null(tmpData) || iNZightTools::is_preview(tmpData)) {
                        readx <- try(readData(), silent = TRUE)

                        if (inherits(readx, "try-error")) {
                            dispose(infw)
                            gmessage("There was an error loading the data.",
                                icon = "error",
                                title = "Unable to load data.",
                                parent = importFileWin
                            )
                            return()
                        }
                    }

                    ## give the dataset a name ...
                    if (is.null(attr(tmpData, "name", exact = TRUE)))
                        attr(tmpData, "name") <<-
                            if (fext %in% c("RData", "rda"))
                                svalue(rdaName)
                            else
                                make.names(
                                    tools::file_path_sans_ext(basename(fname))
                                )

                    ## coerce character to factor
                    GUI$setDocument(
                        iNZDocument$new(
                            data = as.data.frame(tmpData,
                                stringsAsFactors = TRUE
                            )
                        ),
                        reset = TRUE
                    )

                    dispose(infw)

                    ## and activate menu
                    # GUI$menuBarWidget$defaultMenu()

                    ## dunno why but need to delete gdf ...
                    #if (!is.null(prev)) delete(prevGp, prev)
                    dispose(importFileWin)
                },
                container = btnGp
            )


            addHandlerDestroy(importFileWin,
                handler = function(h, ...) {
                    ## Not sure why but if this isn't done before the window closes,
                    ## a GTK Critical error is thrown.
                    if (!is.null(prev)) delete(prevGp, prev)
                    return(TRUE)
                }
            )
            visible(importFileWin) <<- TRUE
        }, # initialize()
        setfile = function(...) {
            svalue(filename) <<- basename(fname)
            fext <<- tools::file_ext(fname)

            blockHandlers(filetype)
            match <- which(sapply(filetypes[-1],
                function(ft)
                    grepl(
                        paste0(ft$patterns, "$", collapse = "|"),
                        paste0(".", fext)
                    )
            ))
            svalue(filetype, index = TRUE) <<-
                if (length(match) > 0) match else 0
            unblockHandlers(filetype)

            # and reset some things about the dataset
            fColTypes <<- NULL
            removeDataName()

            generatePreview(...)
        },
        col_types = function() {
            if (is.null(fColTypes)) return(NULL)
            if (all(fColTypes == "auto")) return(NULL)

            vnames <- colnames(tmpData)[fColTypes != "auto"]
            vtypes <- sapply(fColTypes[fColTypes != "auto"],
                function(x) {
                    switch(x,
                        "numeric" = "n",
                        "categorical" = "c",
                        "date" = "D",
                        "time" = "t",
                        "datetime" = "T"
                    )
                }
            )
            structure(vtypes, .Names = vnames)
        },
        readData = function(preview = FALSE) {
            file_is_url <- svalue(loadURL)
            ## Read data using object values:
            ## this needs to be conditionally constructed ..
            switch(fext,
                "csv" = ,
                "txt" = {
                    tmpData <<- suppressWarnings(suppressMessages({
                        iNZightTools::smart_read(fname,
                            fext,
                            preview = preview,
                            column_types = col_types(),
                            encoding = encoding,
                            delimiter = switch(fext,
                                "csv" = csvdelim,
                                "txt" = txtdelim,
                                NULL
                            ),
                            decimal_mark = decMark,
                            grouping_mark = bigMark
                        )
                    }))
                },
                "RData" = ,
                "rda" = {
                    data_list <- iNZightTools::load_rda(fname)
                    dnames <- names(data_list)
                    cur_val <- svalue(rdaName)
                    blockHandlers(rdaName)
                    rdaName$set_items(dnames)
                    if (!cur_val %in% dnames)
                        svalue(rdaName) <<- dnames[1]
                    else
                        svalue(rdaName) <<- cur_val
                    unblockHandlers(rdaName)
                    tmpData <<- data_list[[svalue(rdaName)]]
                },
                "xls" = ,
                "xlsx" = {
                    tmpData <<- iNZightTools::smart_read(
                        fname,
                        fext,
                        preview = preview,
                        column_types = col_types(),
                        sheet = if (svalue(rdaName) == "(none)") NULL else svalue(rdaName)
                    )
                },
                {
                    tmpData <<- iNZightTools::smart_read(
                        fname,
                        fext,
                        preview = preview,
                        column_types = col_types()
                    )
                }
            )

            ## do a check that col classes match requested ...
            if (is.null(fColTypes) || length(fColTypes) != ncol(tmpData))
                fColTypes <<- rep("auto", ncol(tmpData))

            if (!is.null(iNZightTools::sheets(tmpData))) {
                sheet_list <- iNZightTools::sheets(tmpData)
                cur_val <- svalue(rdaName)
                blockHandlers(rdaName)
                rdaName$set_items(sheet_list)
                if (!cur_val %in% sheet_list)
                    svalue(rdaName) <<- sheet_list[1]
                else
                    svalue(rdaName) <<- cur_val
                unblockHandlers(rdaName)
            }
            if (!is.null(rdaName)) {
                svalue(rdaLabel) <<- ifelse(fext %in% c("xls", "xlsx"), "Sheet :", "Dataset :")
            }
        },
        ## Generate a preview
        generatePreview = function(h, ..., reload = FALSE) {
            if (length(fname) &&
                (file.exists(fname) || svalue(loadURL))) {
                if (!is.null(prev)) {
                    delete(prevGp, prev)
                    prev <<- NULL
                }
                svalue(prevLbl) <<- "Loading preview ..."

                ## do extra stuff if its an RData file
                can_edit_types <- TRUE
                if (fext %in% c("RData", "rda", "xls", "xlsx")) {
                    createDataName()
                    can_edit_types <- FALSE
                } else {
                    removeDataName()
                }

                ## load the preview ...
                tryCatch(
                    {
                        readData(preview = TRUE)
                        ## set the preview
                        if (can_edit_types)
                            svalue(prevLbl) <<-
                                paste(
                                    "Right-click column names to change the type",
                                    "(c = categorical, n = numeric,",
                                    "d = date, t = time)\n"
                                )
                        else
                            svalue(prevLbl) <<- ""
                        prev <<- gdf(head(tmpData, 5), container = prevGp)
                        invisible(prev$remove_popup_menu())

                        if (can_edit_types) {
                            invisible(prev$add_popup(function(col_index) {
                                j <- prev$get_column_index(col_index)
                                types <- c(
                                    "auto",
                                    "numeric",
                                    "categorical",
                                    "date",
                                    "time",
                                    "datetime"
                                )
                                if (!fext %in% c("csv", "txt"))
                                    types <- types[1:3]
                                list(
                                    gradio(types,
                                        selected = match(fColTypes[j], types),
                                        handler = function(h, ...) {
                                            fColTypes[j] <<-
                                                types[svalue(h$obj, index = TRUE)]
                                            generatePreview(h, ..., reload = TRUE)
                                        }
                                    )
                                )
                            }))
                        }
                        names(prev) <<- paste0(
                            names(prev),
                            " (",
                            sapply(tmpData, function(x)
                                switch(class(x)[1],
                                    "integer" = ,
                                    "numeric" = "n",
                                    "factor" = ,
                                    "character" = "c",
                                    "Date" = "d",
                                    "hms" = "t",
                                    "POSIXct" = "dt"
                                )
                            ),
                            ")"
                        )
                    },
                    error = function(e) {
                        svalue(prevLbl) <<-
                            "Unable to read the file. Check the file type is correct and try again."
                        print(e)
                    },
                    finally = advancedOptions()
                )
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
            if (is.null(fColTypes))
                return(NULL)
            types <- lapply(fColTypes, function(x)
                switch(x,
                    "numeric" = "n",
                    "factor" = "c",
                    "date" = "D",
                    "time" = "t",
                    "datetime" = "dt",
                    NULL
                )
            )
            types
        },
        createDataName = function() {
            if (is.null(rdaName)) {
                rdaLabel <<- glabel("Dataset :")
                font(rdaLabel) <<- list(weight = "bold")
                rdaName <<- gcombobox("(none)")
                fileTbl[4, 1, anchor = c(1, 0)] <<- rdaLabel
                fileTbl[4, 2:5, expand = TRUE] <<- rdaName
                addHandlerChanged(rdaName, generatePreview)
            }
        },
        removeDataName = function() {
            if (!is.null(rdaName)) {
                delete(fileTbl, fileTbl[4,1])
                delete(fileTbl, fileTbl[4,2])
                rdaName <<- NULL
                rdaLabel <<- NULL
            }
        },
        advancedOptions = function() {
            ## populate the Advanced Options panel (advGp) with extra options for various data sets.
            ## but first, delete the old one ...
            if (length(advGp$children))
                sapply(advGp$children, function(ch) advGp$remove_child(ch))

            ## build it up!
            tbl <- glayout(container = advGp)
            ii <- 1

            switch(fext,
                "csv" =,
                "txt" = {
                    ## ----------------- LEFT HAND SIDE
                    ## --- DELIMITER
                    lbl <- glabel("Delimiter :")
                    ## add custom choices ...
                    delimOpt <- gcombobox(names(delimiters),
                        selected = which(sapply(delimiters,
                            function(x) get(paste0(fext, "delim")) == x)),
                        editable = FALSE,
                        handler = function(h, ...) {
                            if (fext == "txt")
                                txtdelim <<- delimiters[[svalue(h$obj, index = TRUE)]]
                            else
                                csvdelim <<- delimiters[[svalue(h$obj, index = TRUE)]]
                            ## changing delimiter == changing where columns are
                            fColTypes <<- NULL
                            generatePreview(h, ...)
                        }
                    )
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
                                gmessage(
                                    "Decimal mark and thousands separator must be different.",
                                    type = "error"
                                )
                            else
                                generatePreview(h, ...)
                        }
                    )
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
                                gmessage(
                                    "Decimal mark and thousands separator must be different.",
                                    type = "error"
                                )
                            else
                                generatePreview(h, ...)
                        }
                    )
                    tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
                    tbl[ii, 2:3, expand = TRUE] <- bigMarkOpt
                    ii <- ii + 1

                    ## --- FILE ENCODING
                    lbl <- glabel("File Encoding :")
                    encOpt <- gcombobox(encodings,
                        selected = match(encoding, encodings),
                        handler = function(h, ...) {
                            # encoding <<- strsplit(svalue(h$obj), "/")[[1]][1]
                            encoding <<- svalue(h$obj)
                            generatePreview(h, ...)
                        }
                    )
                    tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
                    tbl[ii, 2:3, expand = TRUE] <- encOpt
                    ii <- ii + 1

                    ## ----------------- RIGHT HAND SIDE
                    ii <- 1

                    ## --- DATE FORMAT
                    ## this should be a drop down of some common formats (2016-01-16, 16 Jan 2016, 16/01/16, 01/16/16, ...)
                    lbl <- glabel("Date Format :")
                    # dateFmt <- gcombobox()


                },
                ## default case
                {
                    lbl <- glabel("No options available for this file type.")
                    tbl[ii, 1, anchor = c(-1, 0), expand = TRUE] <- lbl
                }
            ) # end switch(fext)
        },
        closeHandler = function(h, ...) {
            #addHandlerUnrealize(importFileWin, handler = function(h, ...) {
            print("closing now")
            if (!is.null(prev)) delete(prevGp, prev)
            return(TRUE)
            #})
        }
    )
)
