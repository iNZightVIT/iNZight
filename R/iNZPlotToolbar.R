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
        popOut = "ANY",
        toolbarcont = "ANY",
        iconbar = "ANY",
        altbar = "ANY"
        ),
    methods = list(
        initialize = function(gui, cont) {
            initFields(GUI = gui,
                       plotWidget = gui$plotWidget,
                       popOut = gui$preferences$popout)

            toolbarcont <<- ggroup(container = cont, spacing = 0, fill = TRUE, expand = TRUE)
            iconbar <<- ggroup(horizontal = !popOut, container = toolbarcont, spacing = 15,
                               fill = TRUE, expand = TRUE)

            makeToolbar()

        },
        ## update the toolbar (as opposed to initialize it)
        update = function(btns = c("add", "rmv", "inf"),
                          refresh = NULL,
                          extra = NULL) {

            visible(iconbar) <<- FALSE

            if (length(toolbarcont$children) > 1)
                delete(toolbarcont, toolbarcont$children[[2]])

            altbar <<- ggroup(horizontal = !popOut,container = toolbarcont, spacing = 15,
                              fill = TRUE, expand = TRUE)

            makeToolbar(btns, refresh.fn = refresh, extra, cont = altbar)
        },
        restore = function() {
            setPlotMenu()
            delete(toolbarcont, toolbarcont$children[[2]])
            visible(iconbar) <<- TRUE
        },
        ## create the toolbar!
        makeToolbar = function(btns = c("add", "rmv", "inf"),
                               refresh.fn = NULL,
            extra, cont = iconbar) {

            ## link the menu:
            setPlotMenu(btns, refresh.fn, extra)

            if (is.null(refresh.fn)) {
                refreshFn = GUI$updatePlot
            } else {
                refreshFn = GUI$activeModule[[refresh.fn]]
            }

            img.add2plot <- system.file("images/toolbar-add.png", package = "iNZight")
            img.rmvplot <- system.file("images/toolbar-remove.png", package = "iNZight")
            img.infinfo <- system.file("images/toolbar-inference.png", package = "iNZight")

            newplotBtn <- gimagebutton(stock.id = "newplot", size = "button", name = "newplotbutton",
                                       tooltip = "New Graphics Window")
            addHandlerClicked(newplotBtn, function(h, ...) newPlotWindow(refreshFn))

            newtabBtn <- gimagebutton(stock.id = "new", size = "button",
                                      tooltip = "New Plot Tab")
            addHandlerClicked(newtabBtn, function(h, ...) plotWidget$addPlot())

            refreshplotBtn <- gimagebutton(stock.id = "refresh", size = "button",
                                           tooltip = "Redraw Plot")
            addHandlerClicked(refreshplotBtn, function(h, ...) refreshFn())

            renametabBtn <- gimagebutton(stock.id = "editor", size = "button",
                                         tooltip = "Rename Plot Tab")
            addHandlerClicked(renametabBtn, function(h, ...) plotWidget$renamePlot())


            saveplotBtn <- gimagebutton(stock.id = "save", size = "button",
                                        tooltip = "Save Plot")
            addHandlerClicked(saveplotBtn, function(h, ...) plotWidget$savePlot(refreshFn))

            closetabBtn <- gimagebutton(stock.id = "close", size = "button",
                                        tooltip = "Close Plot Tab")
            addHandlerClicked(closetabBtn, function(h, ...) plotWidget$closePlot())


            ## -- IMAGES
            addtoplotBtn <- gimagebutton(filename = img.add2plot, size = "button",
                                         tooltip = "Add to Plot")
            addHandlerClicked(addtoplotBtn, function(h, ...) addToPlot())

            removeaddBtn <- gimagebutton(filename = img.rmvplot, size = "button",
                                         tooltip = "Remove Additions")
            addHandlerClicked(removeaddBtn, function(h, ...) iNZPlotRmveModWin$new(GUI))

            inferenceBtn <- gimagebutton(filename = img.infinfo, size = "button",
                                         tooltip = "Add Inference Information")
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

            if ("add" %in% btns)
                add(cont, addtoplotBtn)

            if ("rmv" %in% btns)
                add(cont, removeaddBtn)

            if ("inf" %in% btns)
                add(cont, inferenceBtn)

            if (!missing(extra)) {
                addSpace(cont, 10)
                lapply(extra, function(x) add(cont, x))
            }

            addSpace(cont, 10)

        },
        ## Plot Menu
        setPlotMenu = function(btns = c("add", "rmv", "inf"),
                               refresh.fn = NULL,
                               extra) {

            if (is.null(refresh.fn)) {
                refreshFn = GUI$updatePlot
            } else {
                refreshFn = GUI$activeModule[[refresh.fn]]
            }

            curMenu <- svalue(GUI$menubar)
            curMenu[["Plot"]] <- list(
                gaction("Add to plot ...",
                                             handler = function(h, ...) addToPlot()),
                gaction("Remove additions ...", handler = function(h, ...) iNZPlotRmveModWin$new(GUI)),
                gaction("Add inference ...", handler = function(h, ...) addInf()),
                gseparator(),
                gaction(label = "New Tab", icon = "new",
                        handler = function(h, ...) plotWidget$addPlot()),
                gaction(label = "Close Tab", icon = "close",
                        handler = function(h, ...) plotWidget$closePlot()),
                gaction(label = "Rename Tab", icon = "editor",
                        handler = function(h, ...) plotWidget$renamePlot()),
                gseparator(),
                gaction(label = "New Plot Window", icon = "newplot",
                        handler = function(h, ...) newPlotWindow()),
                gaction(label = "Redraw Plot", icon = "refresh",
                        handler = function(h, ...) refreshFn()),
                gaction(label = "Save Plot", icon = "save",
                        handler = function(h, ...) plotWidget$savePlot(refreshFn))
                )

            if (popOut)
                curMenu[["Plot"]][5:8] <- NULL

            curMenu[["Plot"]][which(!c("add", "rmv", "inf") %in% btns)] <- NULL

            svalue(GUI$menubar) <<- curMenu

        },
        ## function to open a new plot window
        newPlotWindow = function(f) {
            newdevice()
            f()
        },
        ## function to open the correct plot modification win
        ## depending on the currently selected variable types
        addToPlot = function() {
            curSet <- GUI$getActiveDoc()$getSettings()
            err <- FALSE
            if (is.null(GUI$plotType))
                err <- TRUE
            if (GUI$plotType == "none")
                err <- TRUE

            if (err)
                gmessage("Hmm... you'll have to create a plot before you can add to it!",
                         title = "No plot!")
            else
                iNZPlotMod$new(GUI)
        },
        addInf = function() {
            if (!is.null(GUI$getActiveDoc()$getModel()$getDesign())) {
                gmessage("Inferential markup of plots for survey data is still in development. If nothing shows up, it's because we haven't got to it yet. If you notice errors (wrong values for data you know) let us know.",
                         icon = "warning", parent = GUI$win, title = "Developmental Feature")
            }

            curSet <- GUI$getActiveDoc()$getSettings()
            err <- FALSE
            if (is.null(GUI$plotType))
                err <- TRUE
            if (GUI$plotType == "none")
                err <- TRUE

            if (err)
                gmessage("It looks like you haven't created a plot yet! Do that, then you can add inference to it!",
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
                                       GUI$getActiveDoc()$setSettings(list(bs.inference = !curSet$bs.inference))
                               })
        })
)
