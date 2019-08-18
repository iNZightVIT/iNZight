iNZViewSwitcher <- setRefClass(
    "iNZViewSwitcher",
    fields = list(
        GUI = "ANY",
        viewGroup = "ANY",
        dataBtn = "ANY",
        listBtn = "ANY",
        ## max size before dataview gets deactived
        dataThreshold = "numeric"
        ),
    methods = list(
        initialize = function(gui, dataThreshold) {
            initFields(GUI = gui,
                       dataThreshold = dataThreshold)
            viewGroup <<- ggroup()
            addSpring(viewGroup)
            dataBtn <<- gbutton("View Data Set",
                                handler = function(h,...) .self$viewData(h,...))
            listBtn <<- gbutton("View Variables",
                                handler = function(h,...) .self$viewList(h,...))
            font(dataBtn) <<- list(weight="bold", family = "sans",
                                   color = "navy")
            font(listBtn) <<- list(weight="bold", family = "sans",
                                   color = "navy")
            dataSet <- GUI$getActiveData()
            ## if the data size is below threshold, start in data view,
            ## otherwise start don't allow view switching
            enabled(dataBtn) <<- FALSE
            if (nrow(dataSet) * ncol(dataSet) >= dataThreshold)
                enabled(listBtn) <<- FALSE
            
            add(viewGroup, dataBtn)
            add(viewGroup, listBtn)
        },
        viewData = function(h, ...) {
            dataSet <- GUI$getActiveData() ## get the active dataSet
            if(is.null(dataSet)){
                gmessage("Please load a new data set (with named columns)",
                         parent = GUI$win)
            } else {
                if((names(dataSet)[1] == "empty"))
                    gmessage("Please load a new data set", parent = GUI$win)
                else {
                    enabled(h$obj) = FALSE
                    GUI$dataViewWidget$dataView() ## change to data.frame view
                    enabled(listBtn) <<- TRUE
                }
            }
        },
        viewList = function(h, ...) {
            dataSet <- GUI$getActiveData() ## get the active dataSet
            if(is.null(dataSet)){
                gmessage("Please load a new data set (with named columns)",
                         parent = GUI$win)
            } else {
                if((names(dataSet)[1] == "empty"))
                    gmessage("Please load a new data set", parent = GUI$win)
                else {
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
        })
    )
