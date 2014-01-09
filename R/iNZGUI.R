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
                   plotWidget = "ANY",
                   ## every window that modifies plot/data
                   ## this way we can ensure to only have one
                   ## open at the time
                   modWin = "ANY"
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
            ## Left side group
            gp1 <- ggroup(horizontal = FALSE, container = g)
            size(gp1) <- c(200, 200)
            ## Right side group
            gp2 <- ggroup(horizontal = FALSE, container = g, expand = TRUE)
            ## set up widgets in the left group
            ## set up the menu bar at the top
            initializeMenu(gp1)
            ## set up dataViewWidget, added below
            initializeDataView()
            ## set up buttons to switch between data/var view
            add(gp1, .self$initializeViewSwitcher()$viewGroup)
            add(gp1, dataViewWidget$dataGp, expand = TRUE)
            ## set up the drag and drop fields
            add(gp1, initializeControlWidget()$ctrlGp, expand = FALSE)
            ## set up the summary buttongs
            add(gp1, initializeSummaryBtns())
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
                import = gaction(
                    label = "Import Data", icon = "symbol_diamond",
                    tooltip = "Import a new Dataset",
                    handler = function(h, ...) iNZImportWin$new(.self)
                    ),
                export = gaction(
                    label = "Export Data", icon = "symbol_diamond",
                    handler = function(h, ...) iNZSaveWin$new(.self,
                        type = "data",
                        data = .self$getActiveData())
                    ),
                conToCat = gaction(
                    label = "Convert to Categorical",
                    icon = "symbol_diamond",
                    tooltip = "Convert a variable to a categorical type",
                    handler = function(h, ...) iNZconToCatWin$new(.self)
                    ),               
                trns = gaction(
                    label = "Transform Variables",
                    icon = "symbol_diamond",
                    tooltip = "Transform a variable using a function",
                    handler = function(h, ...) iNZtrnsWin$new(.self)
                    ),
                clps = gaction(
                    label = "Collapse Levels",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZcllpsWin$new(.self)
                    ),
                reordLvl = gaction(
                    label = "Reorder Levels",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZreorderWin$new(.self)
                    ),
                renmLvl = gaction(
                    label = "Rename Levels",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZrenameWin$new(.self)
                    ),
                rshpDf = gaction(
                    label = "Reshape Dataset",
                    icon = "symbol_diamond",
                    handler = function(h, ...) NULL
                    ),
                cmbnCat = gaction(
                    label = "Combine Categorical Variables",
                    icon = "symbol_diamond",
                    handler = function(h, ...) NULL
                    ),
                create = gaction(
                    label = "Create New Variables",
                    icon = "symbol_diamond",
                    handler = function(h, ...) NULL
                    ),
                frmInt = gaction(
                    label = "Form Class Intervals",
                    icon = "symbol_diamond",
                    handler = function(h, ...) NULL
                    ),
                renmVar = gaction(
                    label = "Rename Variables",
                    icon = "symbol_diamond",
                    handler = function(h, ...) NULL
                    ),
                stdVar = gaction(
                    label = "Standardize Variables",
                    icon = "symbol_diamond",
                    handler = function(h, ...) NULL
                    )                    
                )
            menuBarList <- list(File = actionList[1:2],
                                "Manipulate variables" = actionList[3:13]
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
                dataViewWidget$updateWidget())
            getActiveDoc()$addSettingsObjObserver(function() updatePlot())
        },
        ## set up the buttons used for drag and drop and control of
        ## the plot; they update the plotSettings
        initializeControlWidget = function() {
            ## if plotSettings change, update the plot
            getActiveDoc()$addSettingsObserver(function() updatePlot())
            iNZControlWidget$new(.self)
        },
        ## set up the summary and inference buttons under the 
        ## drag and drop fields
        initializeSummaryBtns = function() {
            sumGrp <- ggroup()
            sumBtn <- gbutton("Get Summary",
                              handler = function(h, ...) {
                                  curSet <- getActiveDoc()$getSettings()
                                  if (!is.null(curSet$x)) {
                                      w <- gwindow("Summary", width = 600, height = 400,
                                                   visible = FALSE, parent = win)
                                      g <- gtext(text = paste(do.call(
                                                     iNZightPlots:::getPlotSummary,
                                                     curSet),
                                                     collapse = "\n"),
                                                 expand = TRUE, cont = w, wrap = FALSE,
                                                 font.attr = c(family = "monospace"))
                                      visible(w) <- TRUE
                                  } else {
                                      gmessage("Please select at least one variable",
                                               parent = win)
                                  }
                              })
            infBtn <- gbutton("Get Inference",
                              handler = function(h, ...) {
                                  curSet <- getActiveDoc()$getSettings()
                                  if (!is.null(curSet$x)) {
                                      w <- gwindow("Summary", width = 600, height = 400,
                                                   visible = FALSE, parent = win)
                                      g <- gtext(text = paste(do.call(
                                                     iNZightPlots:::getPlotInference,
                                                     curSet),
                                                     collapse = "\n"),
                                                 expand = TRUE, cont = w, wrap = FALSE,
                                                 font.attr = c(family = "monospace"))
                                      visible(w) <- TRUE
                                  } else {
                                      gmessage("Please select at least one variable",
                                               parent = win)
                                  }
                              })
            font(sumBtn) <- list(weight = "bold",
                                 family = "normal",
                                 color = "navy")
            font(infBtn) <- list(weight = "bold",
                                 family = "normal",
                                 color = "navy")
            add(sumGrp, sumBtn, expand = TRUE)
            add(sumGrp, infBtn, expand = TRUE)
            sumGrp
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
