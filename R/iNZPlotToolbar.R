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
                        newdevice()  #dev.new()
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
                "inference" = gaction(label = "Inference\nInformation",
                    handler = function(h, ...) addInf(),
                    tooltip = "Add inference information to the Plot")
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
            

            ## And add to the menu bar:
            curMenu <- svalue(GUI$menubar)
            curMenu[["Plot"]] <- list(
                gaction("Add to plot ...", handler = function(h, ...) addToPlot()),
                gaction("Remove additions ...", handler = function(h, ...) iNZPlotRmveModWin$new(GUI)),
                gaction("Add inference ...", handler = function(h, ...) addInf()),
                gseparator(),
                gaction(label = "New Tab", icon = "newplot",
                        handler = function(h, ...) plotWidget$addPlot()),
                gaction(label = "Close Tab", icon = "close",
                        handler = function(h, ...) plotWidget$closePlot()),
                gaction(label = "Rename Tab", icon = "editor",
                        handler = function(h, ...) plotWidget$renamePlot()),
                gseparator(),
                gaction(label = "New Plot Window", icon = "new",
                        handler = function(h, ...) {
                            newdevice()
                            GUI$updatePlot()
                        }),
                gaction(label = "Redraw Plot", icon = "refresh",
                        handler = function(h, ...) GUI$updatePlot()),
                gaction(label = "Save Plot", icon = "save",
                        handler = function(h, ...) plotWidget$savePlot())
                )
            svalue(GUI$menubar) <<- curMenu

            gtoolbariNZ(tbarList, cont = cont, style="icons")
        },
        ## function to open the correct plot modification win
        ## depending on the currently selected variable types
        addToPlot = function() {
            curSet <- GUI$getActiveDoc()$getSettings()
            if (is.null(GUI$plotType))
                gmessage("You must select at least one variable before you can access the Add To Plot menu.",
                         title = "No variable selected")
            else
                switch(GUI$plotType,
                       "scatter" = iNZScatterMod$new(GUI),
                       "dot" = iNZDotchartMod$new(GUI),
                       "bar" = iNZBarchartMod$new(GUI),
                       "hist" = iNZHistogramMod$new(GUI),
                       "grid" = iNZGriddenMod$new(GUI),
                       "hex" = iNZHexbinMod$new(GUI),
                       iNZPlotModWin$new(GUI))
        },
        addInf = function() {
            curSet <- GUI$getActiveDoc()$getSettings()
            if (is.null(GUI$plotType))
                gmessage("You must select at least one variable before you can access the Inference menu.",
                         title = "No variable selected", parent = GUI$win)
            else
                switch(GUI$plotType,
                       "bar" = iNZBarchartInf$new(GUI),
                       "hist" = ,
                       "dot" = iNZDotchartInf$new(GUI),
                       "grid" = ,
                       "hex" = ,
                       "scatter" = {
                           if (is.null(curSet$trend) && curSet$smooth == 0)
                               gmessage("Use the Add to Plot menu to add a trend(s) and/or smoother.",
                                        title = "No trend or smoother", parent = GUI$win)
                           else
                               GUI$getActiveDoc()$setSettings(list(bs.inference = TRUE))
                       })
        })
    )
