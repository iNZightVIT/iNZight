iNZDataModel <- setRefClass(
    "iNZDataModel",
    properties(
        fields = list(
            dataSet = "ANY",
            origDataSet = "ANY",
            rowDataSet = "ANY",
            dataDesign = "ANY",
            dataDesignName = "character",
            name = "character",
            oldname = "character",
            freqtables = "list"
        ),
        prototype = list(
            dataSet = data.frame(empty = " ", stringsAsFactors = TRUE),
            origDataSet = data.frame(empty = " ", stringsAsFactors = TRUE),
            rowDataSet = data.frame(Row.names = 1, empty = " ", stringsAsFactors = TRUE),
            dataDesign = NULL,
            name = "data", oldname = "",
            freqtables = list()
        )
    ),
    contains = "PropertySet", ## need this to add observer to object
    methods = list(
        initialize = function(data = NULL) {
            if(!is.null(data)) {
                .self$setData(data)
            }
        },
        setData = function(data) {
            ## validate names
            names(data) <- make.names(names(data), unique = TRUE)

            ## set data name (default = "data")
            if (is.null(attr(data, "name", exact = TRUE)))
                attr(data, "name") <- "data"

            dataSet <<- data
            origDataSet <<- data
            rowData <- data.frame(Row.names = 1:nrow(data), data,
                                  check.names = TRUE,
                                  stringsAsFactors = TRUE)
            rowDataSet <<- rowData
            name <<- attr(data, "name", exact = TRUE)
            oldname <<- ""
        },
        updateData = function(data) {
            if (is.null(attr(data, "name", exact = TRUE)))
                attr(data, "name") <- "data"
            dataSet <<- data
            name <<- attr(data, "name", exact = TRUE)
        },
        setNames = function(newNames) {
            newNames <- make.names(newNames, unique = TRUE)
            names(dataSet) <<- newNames
        },
        getData = function() {
            dataSet
        },
        getRowData = function() {
            rowDataSet
        },
        addDataObserver = function(FUN, ...) {
            .self$dataSetChanged$connect(FUN, ...)
        },
        addNameObserver = function(FUN, ...) {
            .self$nameChanged$connect(FUN, ...)
        },
        addObjObserver = function(FUN, ...) {
            .self$changed$connect(FUN, ...)
        },
        setFrequencies = function(freq, gui) {
            if (is.null(freq) || freq == "") {
                gui$getActiveDoc()$setSettings(list(freq = NULL))
            }
            gui$getActiveDoc()$setSettings(list(
                freq = freq # gui$getActiveData()[[freq]]
            ))
            # remove any non-categorical variables
            newdata <- gui$getActiveData()
            catvars <- c(names(newdata)[sapply(newdata, is_cat)], freq)
            newdata <- newdata[, catvars]
            attr(newdata, "name") <- paste(sep = ".",
                attr(gui$getActiveData(), "name", exact = TRUE),
                "freq"
            )
            gui$setDocument(iNZDocument$new(data = newdata))
        },
        setDesign = function(strata = NULL, clus1 = NULL, clus2 = NULL,
                             wt = NULL, nest = NULL, fpc = NULL,
                             repweights = NULL, reptype = NULL,
                             scale = NULL, rscales = NULL,
                             poststrat = NULL,
                             type = c("survey", "replicate"),
                             gui, ...) {
            if (is.null(strata) & is.null(clus1) & is.null(clus2) &
                is.null(wt) & is.null(nest) & is.null(fpc) &
                is.null(repweights) & is.null(poststrat)) {
                dataDesign <<- NULL
                dataDesignName <<- name
            } else {
                dataDesign <<-
                    switch(type,
                        "survey" = list(
                            strata = strata,
                            clus1  = clus1,
                            clus2  = clus2,
                            wt     = wt,
                            fpc    = fpc,
                            nest   = nest,
                            poststrat = poststrat,
                            type = type
                        ),
                        "replicate" = list(
                            wt = wt,
                            repweights = repweights,
                            reptype = reptype,
                            scale = scale,
                            rscales = rscales,
                            poststrat = poststrat,
                            type = type
                        )
                    )
                dataDesignName <<-
                    sprintf("%s.%s",
                        name,
                        switch(type, "survey" = "svy", "replicate" = "repsvy")
                    )
            }
        },
        createSurveyObject = function() {
            des <- getDesign()

            weights <- if (is.null(des$wt)) "NULL" else paste("~", des$wt)
            if (des$type == "survey") {
                id <- if (is.null(des$clus1) & is.null(des$clus2)) {
                    "~ 1"
                } else if (is.null(des$clus1)) {
                    paste("~", des$clus2)
                } else if (is.null(des$clus2)) {
                    paste("~", des$clus1)
                } else {
                    paste("~", des$clus1, "+", des$clus2)
                }

                strata <- if (is.null(des$strata)) "NULL" else paste("~", des$strata)
                fpcs <- if (is.null(des$fpc)) "NULL" else paste("~", des$fpc)
                obj <-
                    parse(text =
                        paste0(
                            "survey::svydesign(",
                            "id = ", id, ", ",
                            if (!is.null(des$strata)) sprintf("strata = %s, ", strata),
                            if (!is.null(des$wt) || !is.null(des$freq))
                                sprintf("weights = %s, ", weights),
                            if (!is.null(des$fpc)) sprintf("fpc = %s, ", fpcs),
                            if (!is.null(des$nest) && des$nest) "nest = TRUE, ",
                            "data = dataSet)"
                        )
                    )
            } else {
                ## replicate weights specified
                repweights <- if(is.null(des$repweights)) "NULL"
                    else paste("~", paste(des$repweights, collapse = " + "))
                type <- des$reptype
                rscales <- if (is.null(des$rscales)) "NULL"
                    else sprintf("c(%s)", paste(des$rscales, collapse = ", "))
                obj <-
                    parse(text =
                        paste0("survey::svrepdesign(",
                            if (!is.null(des$wt))
                                sprintf("weights = %s, ", weights),
                            sprintf("repweights = %s, ", repweights),
                            sprintf("type = '%s', ", type),
                            if (!is.null(des$scale))
                                sprintf("scale = %s, ", des$scale),
                            if (!is.null(des$rscales))
                                sprintf("rscales = %s, ", rscales),
                            "data = dataSet)"
                        )
                    )
            }

            if (!is.null(des$poststrat)) {
                design_obj <- eval(obj)
                ## Note: if allowing continuous variables in future,
                ##       this needs a better name:
                pop.totals <- structure(
                    do.call(c,
                        c(
                            list(sum(des$poststrat[[1]]$Freq)),
                            lapply(des$poststrat, function(df) df$Freq[-1])
                        )
                    ),
                    .Names = do.call(c,
                        c(
                            list("(Intercept)"),
                            lapply(des$poststrat, function(df)
                                paste0(names(df)[1], as.character(df[-1,1]))
                            )
                        )
                    )
                )
                obj <- parse(
                    text = sprintf(
                        "survey::calibrate(design_obj, ~%s, pop.totals)",
                        paste(names(des$poststrat), collapse = " + ")
                    )
                )
            }

            eval(obj)
        },
        getDesign = function() {
            dataDesign
        },
        storeFreqTables = function(tbls) {
            freqtables <<- tbls
        },
        getFreqTables = function() {
            freqtables
        },
        getCode = function(remove = TRUE) {
            code <- attr(dataSet, "code")
            if (remove) {
                .dataSetChanged$block()
                attr(dataSet, "code") <<- ""
                .dataSetChanged$unblock()
            }
            code
        },
        setName = function(x) {
            if (x == "") return()

            oldname <<- name
            ## set the dataset's name
            .dataSetChanged$block()
            attr(dataSet, "name") <<- x
            if (oldname != "")
                attr(dataSet, "code") <<- sprintf(".dataset <- %s", oldname)
            .dataSetChanged$unblock()

            name <<- x
        }
        )
    )

iNZPlotSettings <- setRefClass(
    "iNZPlotSettings",
    properties(fields = list(
                   settings = "list",
                   defaultSettings = "list"),
               prototype = list(
                   settings = list(),
                   defaultSettings = list())),
    contains = "PropertySet", ## need this to add observer to object
    methods = list(
        initialize = function(settings = NULL) {
            if(!is.null(settings))
                settings <<- settings
            else
                settings <<- unclass(iNZightPlots:::inzpar())
            defaultSettings <<- unclass(iNZightPlots:::inzpar())
        },
        getSettings = function() {
            settings
        },
        ## change the plot settings
        ## reset: if TRUE, the default plot settings are loaded
        ##        for the additions to the plot
        setSettings = function(setList, reset = FALSE) {
            ## changing x or y? reset the limits
            if (!is.null(setList$x) || !is.null(setList$y)) {
                settings$xlim <<- NULL
                settings$ylim <<- NULL
            }

            if (reset)
                setList <- modifyList(setList,
                                      defaultSettings,
                                      keep.null = TRUE)
            settings <<- modifyList(settings, setList,
                                    keep.null = TRUE)
            defaultSettings <<- modifyList(
                defaultSettings,
                extractDefaults(settings),
                keep.null = TRUE)
        },
        ## reset the plot settings (except the data fields)
        resetSettings = function() {
            setSettings(unclass(iNZightPlots:::inzpar()))
        },
        ## extract a sub-list of a settings list
        ## than can be used to merge with defaultSettings
        extractDefaults = function(theSettings) {
            # defaultFields <- c("cex", "bg", "col.pt", "col.pt", "cex.pt", "cex.dotpt",
            #                    "alpha", "fill.pt", "pch", "internal.labels", "trend")
            defaultFields <- names(defaultSettings)
            forget <- c('plottype', 'xlim', 'ylim')
            defaultFields <- defaultFields[!defaultFields %in% forget]
            theSettings[defaultFields]
        },
        addSettingsObserver = function(FUN, ...) {
            .self$settingsChanged$connect(FUN, ...)
        },
        addObjObserver = function(FUN, ...) {
            .self$changed$connect(FUN, ...)
        })
    )

iNZDocument <- setRefClass(
    "iNZDocument",
    fields = list(
        dataModel = "iNZDataModel",
        plotSettings = "iNZPlotSettings"
        ),
    methods = list(
        initialize = function(data=NULL, settings=NULL) {
            initFields(dataModel = iNZDataModel$new(data),
                       plotSettings = iNZPlotSettings$new(settings))
        },
        getModel = function() {
            dataModel
        },
        getPlotSettings = function() {
            plotSettings
        },
        getData = function() {
            dataModel$getData()
        },
        getSettings = function() {
            plotSettings$getSettings()
        },
        getRowData = function() {
            dataModel$getRowData()
        },
        setSettings = function(setList, reset = FALSE) {
            plotSettings$setSettings(setList, reset)
        },
        ## update the settings to take in current x,y values
        ## from the dataset
        updateSettings = function() {
            settings <- plotSettings$settings
            if (!is.null(settings$x) && !is.null(settings$varnames$x)) {
                settings$x <- getData()[[settings$varnames$x]]
            }
            if (!is.null(settings$y) && !is.null(settings$varnames$y)) {
                settings$y <- getData()[[settings$varnames$y]]
            }
            setSettings(settings)
        },
        getCode = function() {
            dataModel$getCode()
        },
        addDataObserver = function(FUN, ...) {
            dataModel$addDataObserver(FUN, ...)
        },
        addSettingsObserver = function(FUN, ...) {
            plotSettings$addSettingsObserver(FUN, ...)
        },
        addDataObjObserver = function(FUN, ...) {
            dataModel$addObjObserver(FUN, ...)
        },
        addSettingsObjObserver = function(FUN, ...) {
            plotSettings$addObjObserver(FUN, ...)
        }
        )
    )


iNZDataNameWidget <- setRefClass(
    "iNZDataNameWidget",
    fields = list(
        GUI = "ANY",  ## the iNZight GUI object
        datName = "ANY", ## the string for the data set name
        widget = "ANY",
        nameLabel = "ANY"
        ),
    methods = list(
        initialize = function(gui) {
            initFields(GUI = gui,
                       datName = "No data loaded")
            widget <<- ggroup()
            addSpace(widget, 50)
            add(widget, glabel("Data set: "))
            nameLabel <<- gcombobox(.self$datName, handler = function(h, ...) {
                ## prevent code writing ...
                pset <- GUI$getActiveDoc()$getSettings()
                # GUI$ctrlWidget$resetWidget()
                GUI$rhistory$disabled <<- TRUE
                GUI$activeDoc <<- svalue(h$obj, index = TRUE)
                GUI$rhistory$disabled <<- FALSE
                GUI$ctrlWidget$setState(pset)
            })
            add(widget, nameLabel, expand = TRUE)
            enabled(nameLabel) <<- FALSE

            updateWidget()
        },
        updateWidget = function() {
            dataSet <- GUI$getActiveData()
            if(is.null(dataSet)){
                datName <<- "No data loaded"
                enabled(nameLabel) <<- FALSE
            } else {
                if((names(dataSet)[1] == "empty"))
                    datName <<- "No data loaded"
                else {
                    datName <<- attr(dataSet, "name", exact = TRUE)
                }
                enabled(nameLabel) <<- TRUE
            }
            names <- sapply(GUI$iNZDocuments, function(d) d$getModel()$name)
            blockHandlers(nameLabel)
            nameLabel$set_items(names)
            svalue(nameLabel, index = TRUE) <<- GUI$activeDoc
            unblockHandlers(nameLabel)
        }
        )
    )
