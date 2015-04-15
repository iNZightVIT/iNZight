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

                add(mainGrp, parTab)
                add(mainGrp, metTab)
                add(mainGrp, typTab)
                add(mainGrp, btnTab)

                addSpring(mainGrp)

                okButton <<- gbutton("Close", expand = FALSE,
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
                if (svalue(compInt) | svalue(confInt))
                    inf.type <- c("comp", "conf")[c(svalue(compInt), svalue(confInt))]
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
                enabled(compInt) <- svalue(mthd, index = TRUE) == 1
                svalue(compInt) <- if (svalue(mthd, index = TRUE) == 1) svalue(compInt) else FALSE

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
                if (svalue(compInt) | svalue(confInt)) {
                    inf.type <- c("comp", "conf")[c(svalue(compInt), svalue(confInt))]
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

            enabler <- function() {
                enabled(compInt) <- !is.null(curSet$y)
                
                if (svalue(parm, index = TRUE) == 2) {
                    mthd$set_items(c("Year 12", "Bootstrap"))
                    visible(typTab) <- svalue(mthd, index = TRUE) == 2
                } else {
                    mthd$set_items(c("Normal", "Bootstrap"))
                    visible(typTab) <- TRUE
                }

                addIntervals()
            }

            addHandlerChanged(parm, handler = function(h, ...) enabler())
            addHandlerChanged(mthd, handler = function(h, ...) enabler())
            addHandlerChanged(compInt, handler = function(h, ...) enabler())
            addHandlerChanged(confInt, handler = function(h, ...) enabler())

            enabler()
                      
            ## callSuper(gui)
            ## parm <- gradio(c("Medians",
            ##                  "Means"),
            ##                selected = 2)
            ## intType <- gradio(c("Comparison Intervals",
            ##                     "Confidence Intervals",
            ##                     "Comparison + Confidence Intervals"),
            ##                   selected = 3)
            ## mthd <- gradio(c("Bootstrap", "Normal Theory"),
            ##                selected = 2)
            ## addButton <- gbutton(
            ##     "Add Intervals",
            ##     handler = function(h, ...) {
            ##         inf.type <- list("comp",
            ##                          "conf",
            ##                          c("comp", "conf"))[[svalue(intType,
            ##                                                     index = TRUE)]]
            ##         inf.par <- c("median", "mean")[svalue(parm, index = TRUE)]
            ##         bs.inf <- svalue(mthd, index = TRUE) == 1
            ##         GUI$getActiveDoc()$setSettings(
            ##             list(
            ##                 inference.type = inf.type,
            ##                 inference.par = inf.par,
            ##                 bs.inference = bs.inf
            ##                 )
            ##             )
            ##     })
            ## ## the different parameters (means/medians) have different
            ## ## default interval type. On change of parameter, change
            ## ## default interval type
            ## addHandlerChanged(parm, handler = function(h, ...) {
            ##     if (svalue(parm) == "Means") {
            ##         intType$set_items(c("Comparison Intervals",
            ##                             "Confidence Intervals",
            ##                             "Comparison + Confidence Intervals"))
            ##         svalue(intType) <- "Comparison + Confidence Intervals"
            ##         mthd$set_items(c("Bootstrap", "Normal Theory"))
            ##         svalue(mthd) <- "Normal Theory"
            ##     } else {
            ##         intType$set_items("Comparison Intervals")
            ##         svalue(intType) <- "Comparison Intervals"
            ##         selectedMthd <- svalue(mthd, index = TRUE)
            ##         mthd$set_items(c("Bootstrap", "Year 12"))
            ##         svalue(mthd, index = TRUE) <- selectedMthd
            ##     }
            ## })
            ## tbl[1, 2] <<- parm
            ## tbl[2, 2] <<- intType
            ## tbl[3, 2] <<- mthd
            ## tbl[4, 2, expand = TRUE, anchor = c(1, -1)] <<- addButton
        })
    )

