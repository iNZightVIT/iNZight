iNZDataModel <- setRefClass(
    "iNZDataModel",
    properties(fields = list(
                   dataSet = "ANY",
                   origDataSet = "ANY",
                   rowDataSet = "ANY"),
               prototype = list(
                   dataSet = data.frame(empty = " "),
                   origDataSet = data.frame(empty = " "),
                   rowDataSet = data.frame(Row.names = 1, empty = " "))),
    contains = "PropertySet", ## need this to add observer to object
    methods = list(
        initialize = function(data = NULL) {
            if(!is.null(data)) {
                .self$setData(data)
                #.self$setOriginalData(data)
            }
        },
        #
        #setOriginalData = function(data) {
        #  origDataSet <<- data
          
        #},
        ##
        setData = function(data) {
            names(data) <- make.names(names(data), unique = TRUE)
            dataSet <<- data
            origDataSet <<- data
            rowData <- data.frame(Row.names = 1:nrow(data), data,
                                  check.names = TRUE)
            rowDataSet <<- rowData
        },
        updateData = function(data) {
            dataSet <<- data
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
        addObjObserver = function(FUN, ...) {
            .self$changed$connect(FUN, ...)
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
                settings <<- iNZightPlots:::inzPlotDefaults()
            defaultSettings <<- iNZightPlots:::inzPlotDefaults()
        },
        getSettings = function() {
            settings
        },
        ## change the plot settings
        ## reset: if TRUE, the default plot settings are loaded
        ##        for the additions to the plot
        setSettings = function(setList, reset = FALSE) {
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
            setSettings(iNZightPlots:::inzPlotDefaults())
        },
        ## extract a sub-list of a settings list
        ## than can be used to merge with defaultSettings
        extractDefaults = function(theSettings) {
            defaultFields <- c("cex", "bg", "col.pt")
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


