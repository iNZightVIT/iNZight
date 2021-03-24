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
            freqtables = "list",
            currentDesign = "list",
            design_only = "logical"
        ),
        prototype = list(
            dataSet = data.frame(empty = " ", stringsAsFactors = TRUE),
            origDataSet = data.frame(empty = " ", stringsAsFactors = TRUE),
            rowDataSet = data.frame(Row.names = 1, empty = " ", stringsAsFactors = TRUE),
            dataDesign = NULL,
            name = "data", oldname = "",
            freqtables = list(),
            currentDesign = list(),
            design_only = FALSE
        )
    ),
    contains = "PropertySet", ## need this to add observer to object
    methods = list(
        initialize = function(data = NULL) {
            if (is.null(data)) return()

            if (inherits(data, "inzsvyspec")) {
                .self$setData(data$data)
                .self$setDesign(data)
                design_only <<- TRUE
            } else {
                .self$setData(data)
                design_only <<- FALSE
            }
        },
        setData = function(data) {
            ## validate names
            attr(data, "names") <- make.names(colnames(data), unique = TRUE)

            ## set data name (default = "data")
            if (is.null(attr(data, "name", exact = TRUE)))
                attr(data, "name") <- "data"

            dataSet <<- data
            origDataSet <<- data
            rowData <- data.frame(
                Row.names = 1:nrow(data),
                data,
                check.names = TRUE,
                stringsAsFactors = TRUE
            )
            rowDataSet <<- rowData
            name <<- attr(data, "name", exact = TRUE)
            oldname <<- ""
        },
        updateData = function(data) {
            if (inherits(data, "inzsvyspec")) {
                dataDesign <<- unclass(data)
                invisible(createSurveyObject(reload = TRUE))
                data <- data$data
                design_only <<- TRUE
            }

            if (is.null(attr(data, "name", exact = TRUE)))
                attr(data, "name") <- "data"
            dataSet <<- data
            name <<- attr(data, "name", exact = TRUE)
            if (!is.null(dataDesign)) createSurveyObject(TRUE)
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
        removeSignals = function() {
            for (i in seq_along(listeners(dataSetChanged)))
                dataSetChanged$disconnect(1)
            for (i in seq_along(listeners(nameChanged)))
                nameChanged$disconnect(1)
            for (i in seq_along(listeners(.changed)))
                .changed$disconnect(1)
        },
        setFrequencies = function(freq, gui) {
            if (is.null(freq) || freq == "") {
                gui$getActiveDoc()$setSettings(list(freq = NULL))
            }
            gui$getActiveDoc()$setSettings(
                list(
                    freq = as.name(freq) # gui$getActiveData()[[freq]]
                )
            )
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
        setDesign = function(x, gui) {
            if (missing(x)) {
                dataDesign <<- NULL
                dataDesignName <<- name
                return()
            }
            if (inherits(x, "inzsvyspec")) {
                if (is.null(x$design))
                    x <- iNZightTools::make_survey(dataSet, x)
            } else {
                spec <- structure(
                    list(
                        spec = list(
                            ids = if (is.null(x$ids)) 1 else x$ids,
                            probs = x$probs,
                            strata = x$strata,
                            fpc = x$fpc,
                            nest = as.logical(x$nest),
                            weights = x$weights,
                            type = x$type,
                            repweights = x$repweights,
                            scale = x$scale,
                            rscales = x$rscales,
                            reptype = x$reptype,
                            calibrate = x$calibrate
                        )
                    ),
                    class = "inzsvyspec"
                )
                x <- iNZightTools::make_survey(dataSet, spec)
            }
            dataDesign <<- unclass(x)
            dataDesignName <<- sprintf("%s.%s",
                name,
                switch(x$spec$type,
                    "survey" = "svy",
                    "replicate" = "repsvy"
                )
            )
            # when design changed, update the object
            invisible(createSurveyObject(reload = TRUE))
            if (!missing(gui)) {
                gui$dataNameWidget$updateWidget()
                gui$menuBarWidget$defaultMenu()
            }
        },
        createSurveyObject = function(reload = FALSE) {
            if (!is.null(currentDesign$design) && !reload)
                return(currentDesign$design)
            currentDesign <<- getDesign()
            currentDesign$design
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
        },
        getName = function() {
            name
        }
    )
)

iNZPlotSettings <- setRefClass(
    "iNZPlotSettings",
    properties(
        fields = list(
            settings = "list",
            defaultSettings = "list"
        ),
        prototype = list(
            settings = list(),
            defaultSettings = list()
        )
    ),
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
                setList <- modifyList(setList, defaultSettings, keep.null = TRUE)

            settings <<- modifyList(settings, setList, keep.null = TRUE)
            defaultSettings <<- modifyList(
                defaultSettings,
                extractDefaults(settings),
                keep.null = TRUE
            )
        },
        ## reset the plot settings (except the data fields)
        resetSettings = function() {
            setSettings(unclass(iNZightPlots:::inzpar()))
        },
        ## extract a sub-list of a settings list
        ## than can be used to merge with defaultSettings
        extractDefaults = function(theSettings) {
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
        },
        removeSignals = function() {
            for (i in seq_along(listeners(settingsChanged)))
                settingsChanged$disconnect(1)
            for (i in seq_along(listeners(.changed)))
                .changed$disconnect(1)
        }
    )
)

iNZDocument <- setRefClass(
    "iNZDocument",
    fields = list(
        dataModel = "iNZDataModel",
        plotSettings = "iNZPlotSettings"
    ),
    methods = list(
        initialize = function(data = NULL, settings = NULL) {
            initFields(
                dataModel = iNZDataModel$new(data),
                plotSettings = iNZPlotSettings$new(settings)
            )
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
                settings$x <- as.name(settings$varnames$x)
            }
            if (!is.null(settings$y) && !is.null(settings$varnames$y)) {
                settings$y <- as.name(settings$varnames$y)
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
        },
        removeSignals = function() {
            dataModel$removeSignals()
            plotSettings$removeSignals()
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
            initFields(
                GUI = gui,
                datName = "No data loaded"
            )

            widget <<- ggroup()
            addSpace(widget, 50)
            add(widget, glabel("Data set: "))
            nameLabel <<- gcombobox(.self$datName,
                handler = function(h, ...) {
                    ## prevent code writing ...
                    pset <- GUI$getActiveDoc()$getSettings()
                    GUI$rhistory$disabled <<- TRUE
                    GUI$activeDoc <<- svalue(h$obj, index = TRUE)
                    GUI$rhistory$disabled <<- FALSE
                    GUI$ctrlWidget$setState(pset)
                }
            )
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
            names <- sapply(GUI$iNZDocuments,
                function(d) {
                    n <- d$getModel()$getName()
                    if (!is.null(d$getModel()$getDesign()))
                        n <- sprintf("%s (survey design)",
                            d$getModel()$dataDesignName
                        )
                    n
                }
            )
            blockHandlers(nameLabel)
            nameLabel$set_items(names)
            svalue(nameLabel, index = TRUE) <<- GUI$activeDoc
            unblockHandlers(nameLabel)
        }
    )
)
