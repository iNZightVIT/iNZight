#--------------------------------------------------
# This used to be a gtoolbar in gWidgets
# gWidgets2 only allows toolbars attached to the main gwindow
# Therefore we create a subclass of GToolBar from the gWidgets2RGtk2 package
# that does not have this constraint
# have to give a container to this class so that the toolbar is added
# directly in the class initialization and not through add()
#--------------------------------------------------

##' @importFrom iNZightTools newdevice
iNZPlotToolbar <- setRefClass(
    "iNZPlotToolbar",
    fields = list(
        GUI = "ANY",
        plotWidget = "ANY",
        menu = "ANY",
        popOut = "ANY"
        ),
    methods = list(
        initialize = function(gui, cont) {
            initFields(GUI = gui,
                       plotWidget = gui$plotWidget,
                       popOut = gui$preferences$popout)
            
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
                        handler = function(h, ...) newPlotWindow()),
                gaction(label = "Redraw Plot", icon = "refresh",
                        handler = function(h, ...) GUI$updatePlot()),
                gaction(label = "Save Plot", icon = "save",
                        handler = function(h, ...) plotWidget$savePlot())
                )
            if (popOut)
                curMenu[["Plot"]][5:8] <- NULL
            
            svalue(GUI$menubar) <<- curMenu

            newplotBtn <- gimage(stock.id = "newplot", size = "button", tooltip="hello")
            addHandlerClicked(newplotBtn, function(h, ...) newPlotWindow())

            newtabBtn <- gimage(stock.id = "new", size = "button")
            addHandlerClicked(newtabBtn, function(h, ...) plotWidget$addPlot())

            refreshplotBtn <- gimage(stock.id = "refresh", size = "button")
            addHandlerClicked(refreshplotBtn, function(h, ...) GUI$updatePlot())

            renametabBtn <- gimage(stock.id = "editor", size = "button")
            addHandlerClicked(renametabBtn, function(h, ...) plotWidget$renamePlot())


            saveplotBtn <- gimage(stock.id = "save", size = "button")
            addHandlerClicked(saveplotBtn, function(h, ...) plotWidget$savePlot())

            closetabBtn <- gimage(stock.id = "close", size = "button")
            addHandlerClicked(closetabBtn, function(h, ...) plotWidget$closePlot())


            # addtoplotBtn <- gbutton("Add to Plot", function(h, ...) addToPlot())
            ## removeaddBtn <- gbutton("Remove\nAdditions", function(h, ...) iNZPlotRmveModWin$new(GUI))
            ## inferenceBtn <- gbutton("Inference\nInformation", function(h, ...) addInf())
            
            addtoplotBtn <- gimage(stock.id = "lines", size = "button")
            addHandlerClicked(addtoplotBtn, function(h, ...) addToPlot())

            removeaddBtn <- gimage(stock.id = "lines", size = "button")
            addHandlerClicked(removeaddBtn, function(h, ...) iNZPlotRmveModWin$new(GUI))

            inferenceBtn <- gimage(stock.id = "spike", size = "button")
            addHandlerClicked(inferenceBtn, function(h, ...) addInf())
            
            addSpace(cont, 10)
            add(cont, newplotBtn)
            if (!popOut) add(cont, newtabBtn)
            add(cont, refreshplotBtn)
            if (!popOut) add(cont, renametabBtn)
            addSpace(cont, 10)
            add(cont, saveplotBtn)
            if (!popOut) add(cont, closetabBtn)

            addSpring(cont)
            
            add(cont, addtoplotBtn)
            add(cont, removeaddBtn)
            add(cont, inferenceBtn)
            addSpace(cont, 10)
        },
        ## function to open a new plot window
        newPlotWindow = function() {
            newdevice()
            GUI$updatePlot()
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
                       gmessage("Select at least one variable before using Add to Plot.", title = "No variables selected",
                                icon = "error", parent = GUI$win))
        },
        addInf = function() {
            if (!is.null(GUI$getActiveDoc()$getModel()$getDesign())) {
                gmessage("Inferential markup of plots for survey data is still in development. If nothing shows up, it's because we haven't got to it yet. If you notice errors (wrong values for data you know) let us know.",
                         icon = "warning", parent = GUI$win, title = "Developmental Feature")
            }# else {
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
#            }
        })
    )
