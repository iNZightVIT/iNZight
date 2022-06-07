#' @importFrom iNZightTools inzdf
iNZDataModel <- setRefClass(
    "iNZDataModel",
    properties(
        fields = list(
            dataSet = "ANY",
            origDataSet = "ANY",
            dataDesign = "ANY",
            dataDesignName = "character",
            name = "character",
            oldname = "character",
            freqtables = "list",
            currentDesign = "list",
            design_only = "logical",
            dictionary = "list",
            dict_df = "data.frame"
        ),
        prototype = list(
            dataSet = iNZightTools::inzdf(
                data.frame(empty = " ", stringsAsFactors = TRUE),
                name = "(empty)"
            ),
            origDataSet = iNZightTools::inzdf(
                data.frame(empty = " ", stringsAsFactors = TRUE),
                name = "(empty)"
            ),
            dataDesign = NULL,
            name = "data", oldname = "",
            freqtables = list(),
            currentDesign = list(),
            design_only = FALSE,
            dictionary = list(),
            dict_df = data.frame()
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

            if (!inherits(data, "inzdf"))
                data <- iNZightTools::inzdf(data)

            dataSet <<- data
            origDataSet <<- data
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
            dataSet <<- inzdf(data)
            name <<- attr(data, "name", exact = TRUE)
            if (!is.null(dataDesign)) createSurveyObject(TRUE)
        },
        setNames = function(newNames) {
            newNames <- make.names(newNames, unique = TRUE)
            names(dataSet) <<- newNames
        },
        getData = function(lazy = FALSE) {
            if (lazy) return(dataSet)
            as.data.frame(dataSet)
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
                attr(gui$getActiveData(lazy = TRUE), "name", exact = TRUE),
                "freq"
            )
            gui$setDocument(iNZDocument$new(data = newdata, preferences = gui$preferences))
        },
        setDesign = function(x, gui) {
            if (missing(x)) {
                dataDesign <<- NULL
                dataDesignName <<- name
                if (!missing(gui)) {
                    gui$dataNameWidget$updateWidget()
                    gui$menuBarWidget$defaultMenu()
                }
                return()
            }
            if (inherits(x, "inzsvyspec")) {
                spec <- x
                if (is.null(x$design))
                    x <- iNZightTools::make_survey(getData(), x)
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
                x <- iNZightTools::make_survey(getData(), spec)
            }
            if (!is.null(spec$spec$calibrate)) {
                cal <- lapply(names(spec$spec$calibrate),
                    function(v) {
                        c <- spec$spec$calibrate[[v]]
                        d <- data.frame(x = names(c), Freq = as.numeric(c))
                        names(d) <- c(v, "Freq")
                        d
                    }
                )
                names(cal) <- names(spec$spec$calibrate)
                storeFreqTables(cal)
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
        },
        setDictionary = function(dict, apply = FALSE) {
            # do this once, so it's easily available:
            cn <- tolower(colnames(getData()))
            # cn <- cn[cn %in% names(dict)]
            dict <- dict[tolower(names(dict)) %in% cn]
            dict_df <<- iNZightTools::as_tibble(dict, code_sep = "\n")
            dictionary <<- unclass(dict)
            if (!apply) return()
            newdat <- try(
                iNZightTools::apply_dictionary(getData(), dict),
                silent = TRUE
            )
            if (inherits(newdat, "try-error")) return()
            attr(newdat, "name") <- name
            setData(newdat)
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
        initialize = function(settings = NULL, preferences = list()) {
            defaultSettings <<- unclass(iNZightPlots:::inzpar())

            pref_names <- c("gg_theme")
            if (any(pref_names %in% names(preferences))) {
                prefs <- preferences[names(preferences) %in% pref_names]
                if (length(prefs))
                    defaultSettings <<- modifyList(defaultSettings, prefs, keep.null = TRUE)
            }
            if (!is.null(settings))
                settings <<- settings
            else
                settings <<- defaultSettings
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
        initialize = function(data = NULL, settings = NULL, preferences = list()) {
            ## set data name (default = "data")
            if (!is.null(data) && is.null(attr(data, "name", exact = TRUE)))
                attr(data, "name") <- deparse(substitute(data))
            initFields(
                dataModel = iNZDataModel$new(data),
                plotSettings = iNZPlotSettings$new(settings, preferences)
            )
        },
        getModel = function() {
            dataModel
        },
        getPlotSettings = function() {
            plotSettings
        },
        getData = function(lazy = FALSE) {
            dataModel$getData(lazy)
        },
        getSettings = function() {
            plotSettings$getSettings()
        },
        setSettings = function(setList, reset = FALSE) {
            plotSettings$setSettings(setList, reset)
        },
        setDictionary = function(dict, apply = TRUE) {
            dataModel$setDictionary(dict, apply = apply)
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
        import_button = "ANY",
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
            addSpace(widget, 5)
            add(widget, glabel("Dataset: "))

            # nameLabel <<- glabel(.self$datName, expand = TRUE, anchor = c(-1, 0))
            # font(nameLabel) <<- list(weight = "bold")

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
            # add(widget, nameLabel, expand = TRUE)

            import_button <<- gbutton("Import data ...",
                handler = function(h, ...) iNZImportWin$new(gui)
            )
            import_button$set_icon("gw-file")
            add(widget, import_button, expand = TRUE)

            updateWidget()
        },
        updateWidget = function() {
            dataSet <- GUI$getActiveData(lazy = TRUE)
            if (is.null(dataSet) || names(dataSet)[1] == "empty"){
                datName <<- "No data loaded"
                if (identical(widget$children[[2]], nameLabel)) {
                    widget$remove_child(nameLabel)
                    add(widget, import_button, expand = TRUE)
                }
            } else {
                datName <<- attr(dataSet, "name", exact = TRUE)
                if (identical(widget$children[[2]], import_button)) {
                    widget$remove_child(import_button)
                    add(widget, nameLabel, expand = TRUE)
                }
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
            # svalue(nameLabel) <<- datName
            nameLabel$set_items(names)
            svalue(nameLabel, index = TRUE) <<- GUI$activeDoc
            unblockHandlers(nameLabel)
        }
    )
)
