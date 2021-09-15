iNZDataToolbar <- setRefClass(
    "iNZDataToolbar",
    fields = list(
        GUI = "ANY",
        viewGroup = "ANY",
        dataBtn = "ANY",
        listBtn = "ANY",
        infoBtn = "ANY",
        searchBtn = "ANY",
        ## max size before dataview gets deactived
        dataThreshold = "numeric"
    ),
    methods = list(
        initialize = function(gui, dataThreshold) {
            initFields(
                GUI = gui,
                dataThreshold = dataThreshold
            )
            viewGroup <<- ggroup()
            addSpring(viewGroup)

            dataBtn <<- gbutton(
                "",
                handler = function(h,...) .self$viewData(h,...)
            )
            tooltip(dataBtn) <<- "View dataset"
            dataBtn$set_icon("dataframe")

            listBtn <<- gbutton(
                "",
                handler = function(h,...) .self$viewList(h,...)
            )
            tooltip(listBtn) <<- "View variables"
            listBtn$set_icon("file")

            infoBtn <<- gbutton(
                "",
                handler = function(h, ...) gmessage("Dataset info")
            )
            tooltip(infoBtn) <<- "Dataset information"
            infoBtn$set_icon("info")

            searchBtn <<- gbutton(
                "",
                handler = function(h, ...) {
                    GUI$dataViewWidget$toggle_search()
                }
            )
            tooltip(searchBtn) <<- "Search for / Filter variables"
            searchBtn$set_icon("ed-search")

            add(viewGroup, dataBtn)
            add(viewGroup, listBtn)
            add(viewGroup, infoBtn)
            add(viewGroup, searchBtn)

            updateWidget()
        },
        viewData = function(h, ...) {
            dataSet <- GUI$getActiveData() ## get the active dataSet
            if (is.null(dataSet)) {
                gmessage(
                    "Please load a new data set (with named columns)",
                    parent = GUI$win
                )
            } else {
                if ((names(dataSet)[1] == "empty")) {
                    gmessage("Please load a new data set", parent = GUI$win)
                } else {
                    enabled(h$obj) = FALSE
                    GUI$dataViewWidget$dataView() ## change to data.frame view
                    enabled(listBtn) <<- TRUE
                }
            }
        },
        viewList = function(h, ...) {
            dataSet <- GUI$getActiveData() ## get the active dataSet
            if (is.null(dataSet)) {
                gmessage("Please load a new data set (with named columns)",
                         parent = GUI$win)
            } else {
                if ((names(dataSet)[1] == "empty")) {
                    gmessage("Please load a new data set", parent = GUI$win)
                } else {
                    enabled(h$obj) = FALSE
                    GUI$dataViewWidget$listView() ## change to list of col view
                    enabled(dataBtn) <<- TRUE
                }
            }
        },
        ## check wich view is activate and the current data size
        ## and enable the buttongs accordingly
        updateWidget = function() {
            dataSet <- GUI$getActiveData()
            if (is.null(dataSet) || names(dataSet)[1] == "empty") {
                enabled(listBtn) <<-
                    enabled(dataBtn) <<-
                    enabled(infoBtn) <<-
                    enabled(searchBtn) <<- FALSE
                return()
            }
            enabled(infoBtn) <<- enabled(searchBtn) <<- TRUE
            if (nrow(dataSet) * ncol(dataSet) >= dataThreshold) {
                enabled(listBtn) <<- FALSE
                enabled(dataBtn) <<- FALSE
            } else {
                if (visible(GUI$dataViewWidget$dfView)) {
                    enabled(listBtn) <<- TRUE
                    enabled(dataBtn) <<- FALSE
                } else {
                    enabled(listBtn) <<- FALSE
                    enabled(dataBtn) <<- TRUE
                }
            }
        }
    )
)