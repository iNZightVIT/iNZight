iNZViewSwitcher <- setRefClass("iNZViewSwitcher",
                               fields = list(
                                   GUI = "ANY",
                                   viewGroup = "ANY",
                                   dataBtn = "ANY",
                                   listBtn = "ANY"
                                   ),
                               methods = list(
                                   initialize = function(gui) {
                                       initFields(GUI = gui)
                                       viewGroup <<- ggroup()
                                       addSpring(viewGroup)
                                       dataBtn <<- gbutton("View Data Set",
                                                          handler = function(h,...) .self$viewData(h,...))
                                       listBtn <<- gbutton("View Variables",
                                                          handler = function(h,...) .self$viewList(h,...))
                                       font(dataBtn) <<- list(weight="bold", family = "normal",
                                                             color = "navy")
                                       font(listBtn) <<- list(weight="bold", family = "normal",
                                                             color = "navy")
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
                                   })
                               )
