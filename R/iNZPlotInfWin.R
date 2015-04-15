## --------------------------------------------
## The super class for the plot modification window
## The different windows that are opened through the
## 'Inference Information' button are subclasses of this superclass
## The window that is opened depends on the variables
## currently selected in the control widget (or in the iNZDocument,
## which is the same since the two are linked together)
## --------------------------------------------

iNZPlotInfWin <- setRefClass(
    "iNZPlotInfWin",
    fields = list(
        GUI = "ANY",
        parTab = "ANY",
        metTab = "ANY",
        typTab = "ANY",
        btnTab = "ANY",
        curSet = "list"
        ),
    methods = list(
        initialize = function(gui = NULL) {
            initFields(GUI = gui)
            if (!is.null(GUI)) {
                ## close modification window if one is open
                if (length(GUI$leftMain$children) > 1) {
                    delete(GUI$leftMain, GUI$leftMain$children[[2]])
                }
                GUI$initializeModuleWindow()

                updateSettings()

                mainGrp <- gvbox(container = GUI$moduleWindow, expand = TRUE)
                mainGrp$set_borderwidth(5)

                ## Window title
                ttl <- glabel("Add Inference Information")
                font(ttl) <- list(weight = "bold", family = "normal", size = 11)
                add(mainGrp, ttl)

                ## Three layouts, one for parameter/method/type
                parTab <<- glayout()
                metTab <<- glayout()
                typTab <<- glayout()
                btnTab <<- glayout()

                ## Labels for each option
                parLab <- glabel("Parameter")
                font(parLab) <- list(weight = "bold", family = "normal", size = 9)

                metLab <- glabel("Type of Inference")
                font(metLab) <- list(weight = "bold", family = "normal", size = 9)

                typLab <- glabel("Type of Interval")
                font(typLab) <- list(weight = "bold", family = "normal", size = 9)
                
                parTab[2, 1, expand = TRUE, anchor = c(-1, 0)] <<- parLab
                metTab[2, 1, expand = TRUE, anchor = c(-1, 0)] <<- metLab
                typTab[2, 1, expand = TRUE, anchor = c(-1, 0)] <<- typLab

                ## Show interval values button
                intBtn <- gbutton("Get values", expand = FALSE,
                                  handler = function(h, ...) {
                                      displayValues()
                                  })
                btnTab[2, 1, expand = TRUE] <<- intBtn

                add(mainGrp, parTab)
                add(mainGrp, metTab)
                add(mainGrp, typTab)
                add(mainGrp, btnTab)

                addSpring(mainGrp)

                okButton <- gbutton("Close", expand = FALSE,
                                    cont = mainGrp,
                                    handler = function(h, ...) {
                                        ## delete the module window
                                        delete(GUI$leftMain, GUI$leftMain$children[[2]])
                                        ## display the default view (data, variable, etc.)
                                        visible(GUI$gp1) <<- TRUE
                                    })
            }
        },
        ## up the curSet class variable
        updateSettings = function() {
            curSet <<- GUI$getActiveDoc()$getSettings()
        },
        displayValues = function() {
            out <- c()
            lapply(names(GUI$curPlot), function(nA) {
                A <- GUI$curPlot[[nA]]
                rr <- NULL
                if (is.list(A)) {
                    ret <- lapply(names(A), function(nB) {
                        B <- A[[nB]]
                        if (class(B) %in% c("inzdot", "inzhist", "inzbar")) {
                            return(B$inference)
                        } else {
                            return(NULL)
                        }
                    })

                    o <- c()
                    if (any(!sapply(ret, is.null))) {
                        sapply(ret, function(r) {
                            sapply(names(r), function(typN) {
                                typ <- r[[typN]]
                                cat(typN, "\n")
                                sapply(names(typ), function(intN) {
                                    int <- typ[[intN]]
                                    cat("   ", intN, "\n")
                                    print(int)
                                    #o <- c(o, int)
                                })
                            })
                        })
                    }
                }

                if (!is.null(rr)) {
                    if (nA != "all")
                        out <- c(out, cat("Level ", nA, ":\n"))

                    out <- c(out, o)
                }
            })
            #print(out)
            
            gwindow(title = "Inference values", parent = GUI$win)
        })
    )

iNZBarchartInf <- setRefClass(
    "iNZBarchartInf",
    contains = "iNZPlotInfWin",
    methods = list(
        initialize = function(GUI) {
            callSuper(GUI)

            ## Parameters
            parm <- glabel("Proportions")
            
            parTab[3, 1, expand = TRUE, anchor = c(-1, 0)] <<- parm

            
            ## Methods
            mthd <- gradio(c("Normal", "Bootstrap"),
                           selected = 1)

            metTab[3, 1] <<- mthd
            
            
            ## Interval types
            compInt <- gcheckbox("Comparison Intervals",
                                 checked = "comp" %in% curSet$inference.type)
            confInt <- gcheckbox("Confidence Intervals",
                                 checked = "conf" %in% curSet$inference.type)

            typTab[3, 1] <<- confInt
            typTab[4, 1] <<- compInt            

            
            ## Add function
            addIntervals <- function() {
                ## Inference type depends on method (normal = both; bootstrap = only confidence [for now]..)
                if (svalue(compInt) | svalue(confInt))
                    inf.type <- c("comp", "conf")[c(svalue(compInt) & svalue(mthd, index = TRUE) == 1, svalue(confInt))]
                else
                    inf.type <- NULL
                
                
                bs.inf <- svalue(mthd, index = TRUE) == 2
                GUI$getActiveDoc()$setSettings(
                    list(
                        inference.type = inf.type,
                        bs.inference = bs.inf
                        )
                    )
                updateSettings()
            }

            enabler <- function() {
                visible(compInt) <- svalue(mthd, index = TRUE) == 1
                #if (svalue(mthd, index = TRUE) == 2) svalue(compInt) <- FALSE

                addIntervals()
            }

            addHandlerChanged(mthd, handler = function(h, ...) enabler())
            addHandlerChanged(compInt, handler = function(h, ...) enabler())
            addHandlerChanged(confInt, handler = function(h, ...) enabler())

            enabler()
           
        })
    )

iNZDotchartInf <- setRefClass(
    "iNZDotchartInf",
    contains = "iNZPlotInfWin",
    methods = list(
        initialize = function(GUI) {
            callSuper(GUI)

            ## Parameters
            parm <- gradio(c("Mean", "Median"), selected = 1)
            
            parTab[3, 1, expand = TRUE, anchor = c(-1, 0)] <<- parm

            
            ## Methods
            mthd <- gradio(c("Normal", "Bootstrap"),
                           selected = 1)

            metTab[3, 1] <<- mthd
            
            
            ## Interval types
            compInt <- gcheckbox("Comparison Intervals",
                                 checked = "comp" %in% curSet$inference.type)
            confInt <- gcheckbox("Confidence Intervals",
                                 checked = "conf" %in% curSet$inference.type)

            typTab[3, 1] <<- confInt
            typTab[4, 1] <<- compInt            

            
            ## Add function
            addIntervals <- function() {
                if (svalue(parm, index = TRUE) == 2 & svalue(mthd, index = TRUE) == 1) {
                    ## If median + normal, display year12 interval:
                    inf.type <- "conf"
                    inf.par <- "median"
                } else if (svalue(compInt) | svalue(confInt)) {
                    inf.type <- c("comp", "conf")[c(svalue(compInt) & !is.null(curSet$y), svalue(confInt))]
                    inf.par <- c("mean", "median")[svalue(parm, index = TRUE)]
                } else {
                    inf.type <- inf.par <- NULL
                }
                
                bs.inf <- svalue(mthd, index = TRUE) == 2
                GUI$getActiveDoc()$setSettings(
                    list(
                        inference.type = inf.type,
                        inference.par = inf.par,
                        bs.inference = bs.inf
                        )
                    )
                updateSettings()
            }

            enabler <- function(p = FALSE) {
                ## Hide comparison intervals if only one group:
                visible(compInt) <- !is.null(curSet$y)

                ## For MEAN, use NORMAL+BOOTSTRAP
                ## For MEDIAN, use YEAR12+BOOTSTRAP
                ##    YEAR12 intervals are neither CONFIDENCE nor COMPARISON, so hide that option too
                ## But only do this if user changes the parameter:
                if (p) {
                    if (svalue(parm, index = TRUE) == 2) {
                        mthd$set_items(c("Year 12", "Bootstrap"))
                        
                    } else {
                        mthd$set_items(c("Normal", "Bootstrap"))
                    }
                }

                visible(typTab) <<- svalue(parm, index = TRUE) == 1 | svalue(mthd, index = TRUE) == 2
                
                addIntervals()
            }

            addHandlerChanged(parm, handler = function(h, ...) enabler(TRUE))
            addHandlerChanged(mthd, handler = function(h, ...) enabler())
            addHandlerChanged(compInt, handler = function(h, ...) enabler())
            addHandlerChanged(confInt, handler = function(h, ...) enabler())

            enabler()
        })
    )

