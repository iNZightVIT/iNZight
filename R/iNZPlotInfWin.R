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
        curSet = "list",
        intBtn = "ANY"
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
                intBtn <<- gbutton("Get values", expand = FALSE,
                                  handler = function(h, ...) {
                                      displayValues()
                                  })
                btnTab[2, 1, expand = TRUE] <<- intBtn
                visible(intBtn) <<- FALSE
                inflabl <- glabel("* iNZight may appear upresponsive while the bootstraps are performed.\nPlease be patient.")
                font(inflabl) <- list(size = 8)
                btnTab[3, 1, expand = TRUE] <<- inflabl

                add(mainGrp, parTab)
                add(mainGrp, metTab)
                add(mainGrp, typTab)
                add(mainGrp, btnTab)

                addSpring(mainGrp)

                btnGrp <- ggroup(horizontal = TRUE,
                                 expand = FALSE)

                helpButton <- gbutton("Help", expand = TRUE, fill = TRUE,
                                      cont = btnGrp,
                                      handler = function(h, ...) {
                                         browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/plot_options/?topic=plot_inference")
                                      })
                            
                okButton <- gbutton("Close", expand = TRUE, fill = TRUE,
                                    cont = btnGrp,
                                    handler = function(h, ...) {
                                        ## delete the module window
                                        delete(GUI$leftMain, GUI$leftMain$children[[2]])
                                        ## display the default view (data, variable, etc.)
                                        visible(GUI$gp1) <<- TRUE
                                    })
                
                add(mainGrp, btnGrp, expand = FALSE, fill = "x", anchor = c(0, -1))
                visible(GUI$moduleWindow) <<- TRUE
            }
        },
        ## up the curSet class variable
        updateSettings = function() {
            curSet <<- GUI$getActiveDoc()$getSettings()
        },
        displayValues = function() {
            return(NULL)
        })
    )

iNZBarchartInf <- setRefClass(
    "iNZBarchartInf",
    contains = "iNZPlotInfWin",
    methods = list(
        initialize = function(GUI) {
            callSuper(GUI)

            is.survey <- !is.null(GUI$getActiveDoc()$getModel()$getDesign())

            ## Parameters
            parm <- glabel("Proportions")
            
            parTab[3, 1, expand = TRUE, anchor = c(-1, 0)] <<- parm

            
            ## Methods
            if (is.survey)
                mthd <- gradio(c("Normal"), selected = 1)
            else
                mthd <- gradio(c("Normal", "Bootstrap *"), selected = 1)

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
                if (is.survey) {
                    ## While not available:
                    visible(compInt) <- FALSE
                } else {
                    visible(compInt) <- svalue(mthd, index = TRUE) == 1
                }
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

            is.survey <- !is.null(GUI$getActiveDoc()$getModel()$getDesign())

            ## Parameters
            if (is.survey)
                parm <- gradio(c("Mean"), selected = 1)
            else
                parm <- gradio(c("Mean", "Median"), selected = 1)
            
            parTab[3, 1, expand = TRUE, anchor = c(-1, 0)] <<- parm

            
            ## Methods
            if (is.survey)
                mthd <- gradio(c("Normal"), selected = 1)
            else
                mthd <- gradio(c("Normal", "Bootstrap *"), selected = 1)

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
                        mthd$set_items(c("Year 12", "Bootstrap *"))
                    } else {
                        mthd$set_items(c("Normal", "Bootstrap *"))
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

            visible(intBtn) <<- TRUE
        },
        displayValues = function() {
            out <- c()
            pl <- GUI$curPlot

            ## First grab G2 stuff:
            vn <- attr(pl, "varnames")
            g1 <- vn$g1
            g2 <- vn$g2
            
            if (is.null(g2)) {
                pl <- pl["all"]
            } else {
                  pl <- pl[1:(which(names(pl) == "gen") - 1)]
            }

            for (n1 in names(pl)) {
                ## Grab the level of g2:
                p1 <- pl[[n1]]
                
                if (!is.null(g2))
                    out <- c(out, paste0("## ", g2, " = ", n1))

                for (n2 in names(p1)) {
                    p2 <- p1[[n2]]

                    if (!is.null(g1))
                        out <- c(out, paste0("# ", g1, " = ", n2))

                    ## If MEDIAN and NOT BOOTSTRAP, then we show Year 12 intervals (neither COMP nor
                    ## CONF)
                    if (!is.list(p2$inference)) {
                        out <- c(out, "No inference", "")
                        next
                    }                        
                        
                    y12 <- names(p2$inference)[1] == "median" & !attr(p2$inference, "bootstrap")
                    inf <- p2$inference[[1]]  ## only take the first (mean or median)

                    if (y12)
                        inf <- inf["conf"]

                    inf <- inf[!sapply(inf, is.null)]
                    
                    if (length(inf) == 0) {
                        out <- c(out, "No values", "")
                    } else {                    
                        do.call(cbind, lapply(names(inf), function(i) {
                                                       m <- inf[[i]][, 1:2, drop = FALSE]
                                                       if (!y12)
                                                           colnames(m) <- paste(i, colnames(m), sep = ".")
                                                       m
                                                   })
                                ) -> oo
                        
                        mat <- matrix(apply(oo, 2, function(col) {
                                                    format(col, digits = 4)
                                                }), nrow = nrow(oo))
                        mat[grep("NA", mat)] <- ""
                        
                        mat <- rbind(colnames(oo), mat)
                        if (nrow(oo) > 1)
                            mat <- cbind(c("", rownames(oo)), mat)
                        
                        mat <- matrix(apply(mat, 2, function(col) {
                                                     format(col, justify = "right")
                                                 }), nrow = nrow(mat))
                        
                        mat <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
                        
                        out <- c(out, mat, "")
                    }
                }
                

                out <- c(out, "", "")
            }

            ww <- gwindow(title = "Inference values", parent = GUI$win,
                          width = 700, height = 400, visible = FALSE)
            g <- gtext(text = paste(out, collapse = "\n"),
                       expand = TRUE, cont = ww, wrap = FALSE,
                       font.attr = list(family = "monospace"))
            visible(ww) <- TRUE
        })
    )
