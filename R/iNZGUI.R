iNZGUI <- setRefClass(
    "iNZGUI",
    properties(fields = list(
                   ## list of iNZDocuments (contain data, plotSettings)
                   iNZDocuments = "list",
                   ## the active document of the iNZDocuments list
                   activeDoc = "numeric",
                   ## the main GUI window
                   win = "ANY",
                   ## the Widget containing the 2 data views
                   dataViewWidget = "ANY",
                   ## widget that handles the plot notebook
                   plotWidget = "ANY"
                   ),
               prototype = list(
                   activeDoc = 1
                   )
               ),
    methods = list(
        initializeGui = function(data = NULL) {
            iNZDocuments <<- list(iNZDocument$new(data = data))
            win.title <- "iNZight"
            win <<- gwindow(win.title, visible = FALSE, width = 870,
                            height = 600)
            g <- gpanedgroup(container = win, expand = TRUE)
            #w$set_borderwidth(8)
            ## Left side group
            gp1 <- ggroup(horizontal = FALSE, container = g)
            size(gp1) <- c(200, 200)
            ## Right side group
            gp2 <- ggroup(horizontal = FALSE, container = g, expand = TRUE)
            ## set up widgets in the left group
            initializeMenu(gp1) ## set up the menu bar at the top
            initializeDataView()
            add(gp1, .self$initializeViewSwitcher()$viewGroup)
            add(gp1, dataViewWidget$dataGp, expand = TRUE)
            add(gp1, initializeControlWidget()$ctrlGp, expand = FALSE)
            ## set up widgets in the right group
            ## set up plot notebook
            initializePlotWidget()
            add(gp2, plotWidget$plotNb, expand = TRUE)
            initializePlotToolbar(gp2)
            visible(win) <- TRUE
            ## ensures that all plot control btns are visible on startup
            svalue(g) <- 0.375
            ## first plot(empty) needs to be added after window is drawn
            ## to ensure the correct device nr
            plotWidget$addPlot()
        },
        ## set up the menu bar widget
        initializeMenu = function(cont) {
            actionList <- list(
                import = gaction(label = "import", icon = "symbol_diamond",
                    tooltip = "Import a new Dataset",
                    handler = function(h, ...) iNZImportWin$new(.self)),
                export = gaction(label = "export", icon = "symbol_diamond",
                    handler = function(h, ...) iNZSaveWin$new(.self,
                        type = "data",
                        data = .self$getActiveData()))
                )
            menuBarList <- list(file = actionList[1:2]
                                )
            gmenu(menuBarList, container = cont)
        },
        ## set up buttons to switch between data and variable view
        initializeViewSwitcher = function() {
            iNZViewSwitcher$new(.self)
        },
        ## set up the widget to display/edit the loaded dataSet
        initializeDataView = function() {
            ## create the widget
            dataViewWidget <<- iNZDataViewWidget$new(.self)
            ## if the list of active document changes, update the data view
            addActDocObs(function() dataViewWidget$updateWidget())
            ## if the dataSet changes, update the variable View
            getActiveDoc()$addDataObserver(
                function()
                dataViewWidget$updateVarView())
            getActiveDoc()$addSettingsObjObserver(function() updatePlot())
        },
        ## set up the buttons used for drag and drop and control of
        ## the plot; they update the plotSettings
        initializeControlWidget = function() {
            ## if plotSettings change, update the plot
            getActiveDoc()$addSettingsObserver(function() updatePlot())
            iNZControlWidget$new(.self)
        },
        ## set up the widget with the plot notebook
        initializePlotWidget = function() {
            plotWidget <<- iNZPlotWidget(.self)
        },
        ## set up the buttons under the plot to interact with the plot
        initializePlotToolbar = function(cont) {
            iNZPlotToolbar$new(.self, cont)
        },
        ## plot with the current active plot settings
        updatePlot = function() {
            curPlSet <- getActiveDoc()$getSettings()
            if(!is.null(curPlSet$x)){
                do.call(iNZightPlot, curPlSet)
            }
        },
        ## set a new iNZDocument and make it the active one
        setDocument = function(document) {
            ## add a iNZDocument to the end of the doc list
            iNZDocuments <<- c(iNZDocuments, list(document))
            ## set the active document to the one we added
            activeDoc <<- length(iNZDocuments)
            ## if the dataSet changes, update the variable View
            getActiveDoc()$addDataObserver(
                function()
                dataViewWidget$updateVarView())
            ## if plotSettings change, update the plot
            getActiveDoc()$addSettingsObserver(function() updatePlot())
        },
        getActiveDoc = function() {
            iNZDocuments[[activeDoc]]
        },
        getActiveData = function() {
            iNZDocuments[[activeDoc]]$getData()
        },
        getActiveRowData = function() {
            iNZDocuments[[activeDoc]]$getRowData()
        },
        ## add observer to the activeDoc class variable
        addActDocObs = function(FUN, ...) {
            .self$activeDocChanged$connect(FUN, ...)
        }
        )
    )
