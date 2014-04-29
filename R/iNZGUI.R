#' main class that builds the iNZight GUI
#'
#' @param data an optional data.frame that is loaded
#' upon initialisation of the GUI window
#'

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
                   ## the widget handling the switching between the
                   ## 2 data views
                   viewSwitcherWidget = "ANY",
                   ## widget that handles the plot notebook
                   plotWidget = "ANY",
                   ## widget that handles the drag/drop buttons
                   ## under the dataViewWidget
                   ctrlWidget = "ANY",
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
        ## Start the iNZight GUI
        ##   data: data.frame, starts the gui with data already in it
        ##   disposerR: logical, if true R session is closed upon
        ##              closing the gui
        ## This is the main method of iNZight and calls all the other
        ## methods of the GUI class.
        initializeGui = function(data = NULL, disposeR = FALSE) {
            iNZDocuments <<- list(iNZDocument$new(data = data))
            win.title <- paste("iNZight (v",
                               packageDescription("iNZight")$Version,
                               ")", sep = "")
            win <<- gwindow(win.title, visible = FALSE, width = 870,
                            height = 600)
            g <- gpanedgroup(container = win, expand = TRUE)
            ## Left side group
            gp1 <- ggroup(horizontal = FALSE, container = g)
            size(gp1) <- c(300, 300)
            ## Right side group
            gp2 <- ggroup(horizontal = FALSE, container = g, expand = TRUE)
            ## set up widgets in the left group
            ## set up the menu bar at the top
            initializeMenu(gp1, disposeR)
            ## set up dataViewWidget, added below
            ## dataThreshold is used as maximum nr of cells
            ## before data.frame view gets deactivated
            dataThreshold <- 200000
            initializeDataView(dataThreshold)
            ## set up buttons to switch between data/var view
            add(gp1, .self$initializeViewSwitcher(dataThreshold)$viewGroup)
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
            visible(win) <<- TRUE
            ## ensures that all plot control btns are visible on startup
            svalue(g) <- 0.375
            ## first plot(empty) needs to be added after window is drawn
            ## to ensure the correct device nr
            plotWidget$addPlot()
            ## add what is done upon closing the gui
            closerHandler(disposeR)
        },
        ## set up the menu bar widget
        initializeMenu = function(cont, disposeR) {
            actionList <- list(
                import = gaction(
                  #1
                    label = "Import Data", icon = "symbol_diamond",
                    tooltip = "Import a new Dataset",
                    handler = function(h, ...) iNZImportWin$new(.self)
                    ),
                export = gaction(
                  #2
                    label = "Export Data", icon = "symbol_diamond",
                    handler = function(h, ...) iNZSaveWin$new(.self,
                        type = "data",
                        data = .self$getActiveData())
                    ),
                conToCat = gaction(
                  #3
                    label = "Convert to Categorical",
                    icon = "symbol_diamond",
                    tooltip = "Convert a variable to a categorical type",
                    handler = function(h, ...) iNZconToCatWin$new(.self)
                    ),
                trns = gaction(
                  #4
                    label = "Transform Variables",
                    icon = "symbol_diamond",
                    tooltip = "Transform a variable using a function",
                    handler = function(h, ...) iNZtrnsWin$new(.self)
                    ),
                clps = gaction(
                  #5
                    label = "Collapse Levels",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZcllpsWin$new(.self)
                    ),
                reordLvl = gaction(
                  #6
                    label = "Reorder Levels",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZreorderWin$new(.self)
                    ),
                renmLvl = gaction(
                  #7
                    label = "Rename Levels",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZrenameWin$new(.self)
                    ),
                cmbnCat = gaction(
                  #8
                    label = "Combine Categorical Variables",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZcmbCatWin$new(.self)
                    ),
                create = gaction(
                  #9
                    label = "Create New Variables",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZcrteVarWin$new(.self)
                    ),
                ## The code for displaying this window is already there
                ## just the functionality of forming the intervals is
                ## left to be implemented
                 frmInt = gaction(
                   #10
                   label = "Form Class Intervals",
                     icon = "symbol_diamond",
                     handler = function(h, ...) iNZfrmIntWin$new(.self)
                     ),
                renmVar = gaction(
                  #11
                    label = "Rename Variables",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZrnmVarWin$new(.self)
                    ),
                stdVar = gaction(
                  #12
                    label = "Standardize Variables",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZstdVarWin$new(.self)
                    ),
                slctCases = gaction(
                  #13
                    label = "Filter Dataset",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZFilterWin$new(.self)
                    ),
                rshpData = gaction(
                  #14
                    label = "Reshape Dataset",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZReshapeDataWin$new(.self)
                    ),
                rstrData = gaction(
                  #15
                    label = "Restore Dataset",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        getActiveDoc()$getModel()$updateData(
                            getActiveDoc()$getModel()$origDataSet)
                    }
                    ),
                home = gaction(
                  #16
                    label = "Home",
                    icon = "symbold_diamond",
                    handler = function(h, ...) {
                        dispose(win)
                        iNZightVIT(disposeR = disposeR)
                    }),
                tsMod = gaction(
                  #17
                    label = "Time Series",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        ign <- gwindow("...", visible = FALSE)
                        tag(ign, "dataSet") <- getActiveData()
                        e <- list(obj = ign)
                        e$win <- win
                        timeSeries(e)
                    }
                    ),
                modelFit = gaction(
                  #18
                    label = "Model Fitting",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        ign <- gwindow("...", visible = FALSE)
                        tag(ign, "dataSet") <- getActiveData()
                        e <- list(obj = ign)
                        e$win <- win
                        modelFitting(e)
                    }
                    ),
                threeDPlot = gaction(
                  #19
                    label = "3D Plot",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        ign <- gwindow("...", visible = FALSE)
                        tag(ign, "dataSet") <- getActiveData()
                        e <- list(obj = ign)
                        e$win <- win
                        plot3D(e)
                    }
                    ),
                scatterMatrix = gaction(
                  #20
                    label = "Pairs",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        iNZscatterMatrix$new(.self)
                    }
                    ),
                deleteVariables = gaction(
                  #21
                    label = "Delete Variables",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        iNZdeleteVarWin$new(.self)
                    }
                    ),
                allPlots = gaction(
                  #22
                    label = "All 1-variable Plots",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                       iNZallPlots$new(.self)
                    }
                    ),
                allSummaries = gaction(
                  #23
                    label = "All 1-variable Summaries",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        iNZallSummaries$new(.self)
                    }
                    ),
                exploreMissingness = gaction(
                  #24
                    label = "Missing Values",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        iNZExploreMissing$new(.self)
                    }
                    ),
                missToCat = gaction(
                  #25
                    label = "Missing to Categorical",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZmissCatWin$new(.self)
                    ),
                all2Plots = gaction(
                  #26
                    label = "Explore 2-variable Plots",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        iNZall2Plots$new(.self)
                    }
                    ),
                sortBy = gaction(
                  #27
                  label = "Sort data by variables",
                  icon = "symbol_diamond",
                  handler = function(h, ...){
                    iNZSortbyDataWin$new(.self) 
                  }
                ),
                agraData = gaction(
                  #28
                  label = "Aggregate data",
                  icon = "symbol_diamond",
                  handler = function(h, ...){
                      iNZAgraDataWin$new(.self)
                  }
                ),
                rankNum = gaction(
                  #29
                  label = "Rank Numerical Variables",
                  icon = "symbol_diamond",
                  handler = function(h, ...) iNZrankNumWin$new(.self)
                  ),
                convert2mc = gaction(
                  #30
                  label = "Convert to Categorial (Multiple)",
                  icon = "symbol_diamond",
                  handler = function(h, ...) iNZctocatmulWin$new(.self)
                  )
                #####################################################
                ###  big suggestion
                ###  any new update function should be placing below to match the actionList[[number]]
                ###  so next one should be #27 and placing below.
                ###################################################
                )
            ## home button is disabled if package 'vit' is not loaded
            if (!'package:vit' %in% search())
                enabled(actionList[[16]]) <- FALSE
            ## disable modules if packages are not loaded
            if (!'package:iNZightModules' %in% search())
                invisible(sapply(actionList[19:20], function(x) {
                    enabled(x) <- FALSE}))
            if (!'package:iNZightMR' %in% search())
                enabled(actionList[[24]]) <- FALSE
            menuBarList <- list(
                File = actionList[c(16, 1:2)],
                "Filter Data" = actionList[c(13, 15, 27, 28)],
                "Manipulate variables" = list(
                    actionList[[3]],
                    "Categorical Variables" = actionList[c(6,5,7,8)],
                    "Numeric Variables" = actionList[c(4,12,10, 29,30)],
                    actionList[[11]],
                    actionList[[9]],
                    actionList[[25]],
                    actionList[[14]],
                    actionList[[21]]
                    ),
                "Advanced" = list(
                    "Quick Explore" = actionList[c(24, 22, 23, 26, 20)],
                    actionList[[19]],
                    actionList[[17]],
                    actionList[[18]]
                    )
               # "Advanced" = actionList[c(19, 18, 16, 17)]
                )
            gmenu(menuBarList, container = cont)

        },
        ## set up buttons to switch between data and variable view
        initializeViewSwitcher = function(dataThreshold) {
            viewSwitcherWidget <<- iNZViewSwitcher$new(.self, dataThreshold)
            .self$viewSwitcherWidget
        },
        ## set up the widget to display/edit the loaded dataSet
        initializeDataView = function(dataThreshold) {
            ## create the widget
            dataViewWidget <<- iNZDataViewWidget$new(.self, dataThreshold)
            ## if the list of active document changes, update the data view
            addActDocObs(function() {
                dataViewWidget$updateWidget()
                viewSwitcherWidget$updateWidget()
            })
            ## if the dataSet changes, update the variable View
            getActiveDoc()$addDataObserver(
                function() {
                    dataViewWidget$updateWidget()
                    viewSwitcherWidget$updateWidget()
                    getActiveDoc()$updateSettings()
                }
                )
            ## if the settings change, redraw the plot
            getActiveDoc()$addSettingsObjObserver(function() updatePlot())
        },
        ## set up the buttons used for drag and drop and control of
        ## the plot; they update the plotSettings
        initializeControlWidget = function() {
            ## if plotSettings change, update the plot
            getActiveDoc()$addSettingsObserver(function() updatePlot())
            ctrlWidget <<- iNZControlWidget$new(.self)
        },
        ## set up the summary and inference buttons under the
        ## drag and drop fields
        initializeSummaryBtns = function() {
            sumGrp <- ggroup()
            sumBtn <- gbutton(
                "Get Summary",
                handler = function(h, ...) {
                    curSet <- getActiveDoc()$getSettings()
                    if (!is.null(curSet$x)) {
                        if (is.numeric(curSet$x) & is.numeric(curSet$y)) {
                            tmp.x <- curSet$y
                            curSet$y <- curSet$x
                            curSet$x <- tmp.x
                            v <- curSet$varnames
                            curSet$varnames$x <- v$y
                            curSet$varnames$y <- v$x
                        }

                        w <- gwindow("Summary", width = 700, height = 400,
                                     visible = FALSE, parent = win)
                        g <- gtext(text = paste(do.call(
                                       iNZightPlots:::getPlotSummary,
                                       curSet),
                                       collapse = "\n"),
                                   expand = TRUE, cont = w, wrap = FALSE,
                                   font.attr = list(family = "monospace"))
                        visible(w) <- TRUE
                    } else {
                        gmessage("Please select at least one variable",
                                 parent = win)
                    }
                })
            infBtn <- gbutton(
                "Get Inference",
                handler = function(h, ...) {
                    curSet <- getActiveDoc()$getSettings()
                    if (!is.null(curSet$x)) {
                        if (is.numeric(curSet$x) & is.numeric(curSet$y)) {
                            tmp.x <- curSet$y
                            curSet$y <- curSet$x
                            curSet$x <- tmp.x
                            v <- curSet$varnames
                            curSet$varnames$x <- v$y
                            curSet$varnames$y <- v$x
                        }

                        w <- gwindow("Choose Method", width = 100,
                                     height = 100, parent = win)
                        g <- ggroup(cont = w, horizontal = FALSE)
                        lbl <- glabel("Choose Method to \nGenerate Inference:",
                                      cont = g)
                        rd <- gradio(c("Normal", "Bootstrap"), cont = g)
                        btn <- gbutton("ok", handler = function(h, ...) {
                            sets <- curSet
                            sets <- modifyList(
                                sets,
                                list(bs.inference = (svalue(rd, index = TRUE) == 2))
                                )
                            if (svalue(rd, index = TRUE) == 2) {
                                wBoots <- gwindow("Performing Bootstrap Simulations...Please Wait",
                                               parent = win, width=600, height=400)
                                gBoots <- gtext("Currently performing bootstrap simulations. Depending on the size of your data, this may take a while.\nPlease wait...",
                                                cont = wBoots, expand = TRUE,
                                                font.attr = list(family = "monospace"))
                            }
                            dispose(w)
                            w2 <- gwindow("Summary", width = 600, height = 400,
                                          visible = FALSE, parent = win)
                            g2 <- gtext(
                                paste(
                                    do.call(
                                        iNZightPlots:::getPlotInference,
                                        sets),
                                    collapse = "\n"),
                                expand = TRUE, cont = w2, wrap = FALSE,
                                font.attr = list(family = "monospace"))
                            try(dispose(wBoots), silent = TRUE)
                            visible(w2) <- TRUE
                        }, cont = g)

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
            plotWidget <<- iNZPlotWidget$new(.self)
        },
        ## set up the buttons under the plot to interact with the plot
        initializePlotToolbar = function(cont) {
            iNZPlotToolbar$new(.self, cont)
        },
        ## if set upon gui startup, close the R sessions when
        ## the gui is closed
        closerHandler = function(disposeR) {
            addHandlerUnrealize(win, handler = function(h, ...) {
                if (disposeR) {
                    confirm <- gconfirm(
                        title = "Are you sure?",
                        msg = "Do you wish to quit iNZightVIT?",
                        icon = "question",
                        parent = win)
                    if (confirm)
                        q(save = "no")
                    else
                        FALSE
                } else {
                    dispose(win)
                    try(dev.off(), silent = TRUE)
                }
            })
        },
        ## plot with the current active plot settings
        updatePlot = function() {
            curPlSet <- getActiveDoc()$getSettings()
            if(!is.null(curPlSet$x)){
              # Switch x and y:
                if (is.numeric(curPlSet$x) & is.numeric(curPlSet$y)) {
                    x.tmp <- curPlSet$y
                    curPlSet$y <- curPlSet$x
                    curPlSet$x <- x.tmp

                    x.tmp <- curPlSet$varnames$y
                    curPlSet$varnames$y <- curPlSet$varnames$x
                    curPlSet$varnames$x <- x.tmp
                }

                do.call(iNZightPlot, curPlSet)
            } else {
                resetPlot()
            }
        },
        ## set a new iNZDocument and make it the active one
        setDocument = function(document) {
            ## reset control widget
            ctrlWidget$resetWidget()
            ## add a iNZDocument to the end of the doc list
            iNZDocuments <<- c(iNZDocuments, list(document))
            ## set the active document to the one we added
            activeDoc <<- length(iNZDocuments)
            ## if the dataSet changes, update the variable View
            ## and the settings to take into account possible
            ## change of currently selected data
            getActiveDoc()$addDataObserver(
                function() {
                    dataViewWidget$updateWidget()
                    getActiveDoc()$updateSettings()
                }
                )
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
