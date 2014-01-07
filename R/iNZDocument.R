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
            }
        },
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
                   settings = "list"),
               prototype = list(
                   settings = list())),
    contains = "PropertySet", ## need this to add observer to object
    methods = list(
        initialize = function(settings = NULL) {
            if(!is.null(settings))
                settings <<- settings
            else
                settings <<- iNZightPlots:::inzPlotDefaults()
        },
        getSettings = function() {
            settings
        },
        setSettings = function(setList) {
            ##settings <<- modifyList(settings,
            ##                       structure(list(value),
            ##                                .Names = field))
            ##settings[[field]] <<- value
            settings <<- modifyList(settings, setList)
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
        setSettings = function(setList) {
            plotSettings$setSettings(setList)
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
