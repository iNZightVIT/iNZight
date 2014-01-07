#--------------------------------------------------
# This used to be a gtoolbar in gWidgets
# gWidgets2 only allows toolbars attached to the main gwindow
# Therefore we create a subclass of GToolBar from the gWidgets2RGtk2 package
# that does not have this constraint
# have to give a container to this class so that the toolbar is added
# directly in the class initialization and not through add()
#--------------------------------------------------

iNZPlotToolbar <- setRefClass(
    "iNZPlotToolbar",
    fields = list(
        GUI = "ANY",
        plotWidget = "ANY"
        ),
    methods = list(
        initialize = function(gui, cont) {
            initFields(GUI = gui,
                       plotWidget = gui$plotWidget)
            actionList <- list(
                "newplot" = gaction(label = "newplot", icon = "newplot",
                    handler = function(h, ...)
                    plotWidget$addPlot(),
                    tooltip = "Create a new plot tab"),
                "new" = gaction(label = "new", icon = "new",
                    handler = function(h, ...) {
                        dev.new()
                        GUI$updatePlot()
                    },
                    tooltip = "Detach plot from window"),
                "refresh" = gaction(label = "refresh", icon = "refresh",
                    handler = function(h, ...) GUI$updatePlot(),
                    tooltip = "Redraw plot with current settings"),
                "rename" = gaction(label = "rename", icon = "editor",
                    handler = function(h, ...) plotWidget$renamePlot(),
                    tooltip = "Rename the plot tab"),
                "save" = gaction(label = "save", icon = "save",
                    handler = function(h, ...) plotWidget$savePlot(),
                    tooltip = "Save the plot to a file"),
                "close" = gaction(label = "close", icon = "close",
                    handler = function(h, ...) plotWidget$closePlot(),
                    tooltip = "Close the current plot tab"),
                "addToPlot" = gaction(label = "Add to Plot",
                    handler = function(h, ...) addToPlot(),
                    tooltip = "Add modifications to the Plot"),
                "removeAdd" = gaction(label = "Remove\nAdditions",
                    handler = function(h, ...) iNZPlotRmveModWin$new(GUI),
                    tooltip = "Remove modifications from the Plot"),
                "inference" = gaction(label = "Inference\nInformation")
                )
            tbarList <- c(actionList[1:4],
                          gseparator(),
                          actionList[5:6],
                          gseparator(),
                          gbutton(action = actionList[[7]]),
                          gseparator(),
                          gbutton(action = actionList[[8]]),
                          gseparator(),
                          gbutton(action = actionList[[9]]),
                          gseparator()
                          )
            gtoolbariNZ(tbarList, cont = cont, style="icons")
        },
        ## function to open the correct plot modification win
        ## depending on the currently selected variable types
        addToPlot = function() {
            curSet <- GUI$getActiveDoc()$getSettings()            
            if (is.numeric(curSet$x)) {
                if (is.numeric(curSet$y)) {
                    ## scatterplot
                    iNZScatterMod$new(GUI)
                } else if (is.null(curSet$y)) {
                    ## dot plot
                    iNZDotchartMod$new(GUI)                    
                } else {
                    ## dotchart subsetted by a factor
                    iNZDotchartMod$new(GUI)                    
                }                
            } else {
                if (is.numeric(curSet$y)) {
                    ## dotchart subsetted by a factor
                    iNZDotchartMod$new(GUI)
                } else if (is.null(curSet$y)) {
                    ## normal bar chart
                    iNZBarchartMod$new(GUI)
                } else {
                    ## segmented bar chart (2 factor vars)
                    ## no mod window for this plot type
                }
            }
        })
    )
