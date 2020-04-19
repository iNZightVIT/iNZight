#' iNZight GUI Class
#'
#' Main class that builds the iNZight GUI
#' @field iNZDocuments A list of documents containing data, plot settings, etc.
#' @field activeDoc The numeric ID of the currently active document
#' @import methods utils grDevices colorspace
#' @importFrom magrittr %>%
#' @export iNZGUI
#' @exportClass iNZGUI
iNZGUI <- setRefClass(
    "iNZGUI",
    properties(
        fields = list(
            ## list of iNZDocuments (contain data, plotSettings)
            iNZDocuments = "list",
            ## the active document of the iNZDocuments list
            activeDoc = "numeric",
            ## the main GUI window
            win = "ANY",
            menuBarWidget = "ANY",
            menubar = "ANY",

            ## left group
            leftMain = "ANY",
            moduleWindow = "ANY",
            activeModule = "ANY",
            gp1 = "ANY",
            ## middle group
            gp2 = "ANY",
            popOut = "logical",

            ## the Widget containing the 2 data views
            dataViewWidget = "ANY",
            ## the widget handling the switching between the
            ## 2 data views
            viewSwitcherWidget = "ANY",
            dataNameWidget = "ANY",
            ## widget that handles the plot notebook
            plotWidget = "ANY",
            plotToolbar = "ANY",
            ## widget that handles the drag/drop buttons
            ## under the dataViewWidget
            ctrlWidget = "ANY",
            ## Save the summary and inference buttons to allow disabling
            sumBtn = "ANY",
            infBtn = "ANY",
            ## every window that modifies plot/data
            ## this way we can ensure to only have one
            ## open at the time
            modWin = "ANY",
            ## the current plot and its type (scatter, dot, etc...)
            curPlot = "ANY",
            plotType = "ANY",
            OS = "character",
            prefs.location = "character",
            preferences = "list",
            statusbar = "ANY",
            ## allow modules to attach data to the GUI
            moduledata = "list",
            ## keep a track of R code history
            rhistory = "ANY",
            plot_history = "ANY",
            disposer = "logical",
            addonModuleDir = "character"
        ),
        prototype = list(
            activeDoc = 1,
            plotType = "none"
        )
    ),
    methods = list(
        ## Start the iNZight GUI
        ##   data: data.frame, starts the gui with data already in it
        ##   disposerR: logical, if true R session is closed upon
        ##              closing the gui
        ## This is the main method of iNZight and calls all the other
        ## methods of the GUI class.
        initializeGui = function(
            data = NULL,
            disposeR = FALSE,
            addonDir = NULL
        ) {
            "Initiates the GUI"
            iNZDocuments <<- list(iNZDocument$new(data = data))
            disposer <<- disposeR
            win.title <- paste(
                "iNZight (v",
                packageDescription("iNZight")$Version,
                ")",
                sep = ""
            )

            OS <<-
                if (.Platform$OS == "windows") "windows"
                else if (Sys.info()["sysname"] == "Darwin") "mac"
                else "linux"

            ## We must set the correct directory correctly ...
            switch(
                OS,
                "windows" = {
                    done <- FALSE
                    if (file.exists(file.path("~", "iNZightVIT"))) {
                        setwd(file.path("~", "iNZightVIT"))
                    } else {
                        ## Create it:
                        conf <- gconfirm(
                            paste("Do you want to create an iNZightVIT directory",
                                  "in your My Documents folder to save data and preferences?"),
                            title = "Create Folder", icon = "question")

                        if (conf) {
                            done <- dir.create(file.path("~", "iNZightVIT"))
                            if (!done)
                                gmessage("iNZight was unable to create the folder.")
                        }
                    }
                },
                "mac" = {
                    done <- FALSE
                    if (file.exists(file.path("~", "Documents", "iNZightVIT"))) {
                        setwd(file.path("~", "Documents", "iNZightVIT"))
                    } else {
                        ## Create it:
                        conf <- gconfirm(
                            paste("Do you want to create an iNZightVIT directory",
                                  "in your Documents folder to save data and preferences?"),
                            title = "Create Folder", icon = "question")

                        if (conf) {
                            if ( dir.create(file.path("~", "Documents", "iNZightVIT")) ) {
                                try(setwd(Sys.getenv("R_DIR")), TRUE)

                                done <- TRUE
                            }

                            if (!done)
                                gmessage("iNZight was unable to create the folder.")
                        }

                        if (!done)
                            try(setwd(Sys.getenv("R_DIR")), TRUE)
                    }
                    try({
                        dir.create(file.path("~", "Documents", "iNZightVIT", "Saved Plots"))
                        dir.create(file.path("~", "Documents", "iNZightVIT", "Saved Data"))
                    }, TRUE)

                },
                "linux" = {
                    ## no need to do anything (yet..)
                }
            )

            if (!is.null(addonDir) && dir.exists(addonDir)) {
                addonModuleDir <<- addonDir
            } else {
                addonModuleDir <<- switch(OS,
                    "windows" =
                        file.path("~", "iNZightVIT", "modules"),
                    "mac" = ,
                    "linux" =
                        file.path("~", "Documents", "iNZightVIT", "modules")
                )
                if (!dir.exists(addonModuleDir))
                    addonModuleDir <<- NULL
            }

            ## Grab settings file (or try to!)
            getPreferences()

            ## Check for updates ... need to use try incase it fails (no connection etc)
            if (preferences$check.updates) {
                try({
                    oldpkg <- old.packages(repos = "https://r.docker.stat.auckland.ac.nz")
                    if (nrow(oldpkg) > 0) {
                        win.title <- paste(win.title, " [updates available]")
                    }
                }, silent = TRUE)
            }

            popOut <<- preferences$popout

            win <<- gwindow(
                win.title,
                visible = FALSE,
                width = if (popOut) NULL else preferences$window.size[1],
                height = preferences$window.size[2]
            )

            gtop <- ggroup(horizontal = FALSE, container = win,
                           use.scrollwindow = FALSE)
            menugrp <- ggroup(container = gtop)
            initializeMenu(menugrp, disposeR)

            g <- gpanedgroup(container = gtop, expand = TRUE)

            ## Left side group
            leftMain <<- ggroup(container = g, expand = FALSE)
            if (!popOut) size(leftMain) <<- c(220, -1)
            gp1 <<- gvbox(container = leftMain, expand = TRUE)

            ## Right group
            gp2 <<- ggroup(horizontal = FALSE, container = g, expand = !popOut)


            ## set up widgets in the left group

            ## set up dataViewWidget, added below
            ## dataThreshold is used as maximum nr of cells
            ## before data.frame view gets deactivated
            dataThreshold <- 200000
            initializeDataView(dataThreshold)

            ## set up buttons to switch between data/var view
            add(gp1, .self$initializeViewSwitcher(dataThreshold)$viewGroup)

            ## display the name of the data set
            add(gp1, .self$initializeDataNameWidget()$widget)

            ## display the data
            add(gp1, dataViewWidget$dataGp, expand = TRUE)

            ## set up the drag and drop fields
            add(gp1, initializeControlWidget()$ctrlGp, expand = FALSE)

            ## set up the summary buttongs
            add(gp1, initializeSummaryBtns())

            ## set up widgets in the right group
            grpRight <- ggroup(horizontal = popOut,
                               container = gp2, expand = TRUE)
            ## set up plot notebook
            initializePlotWidget()
            if (!popOut) add(grpRight, plotWidget$plotNb, expand = TRUE)
            else addSpace(grpRight, 10)

            ## set up plot toolbar
            plotToolbar <<- ggroup(horizontal = !popOut, container = grpRight, spacing = 10)
            size(plotToolbar) <<- if (popOut) c(-1, -1) else c(-1, 45)
            initializePlotToolbar(plotToolbar)


            visible(win) <<- TRUE

            ## ensures that all plot control btns are visible on startup
            #svalue(g) <- 0.375
            ## first plot(empty) needs to be added after window is drawn
            ## to ensure the correct device nr
            if (popOut)
                iNZightTools::newdevice()
            else
                plotWidget$addPlot()

            ## draw the iNZight splash screen
            plotSplashScreen()

            ## add what is done upon closing the gui
            closerHandler(disposeR)

            ## and start tracking history
            initializeCodeHistory()

            ## init statusbar
            statusbar <<- gstatusbar("iNZight is ready")# , container = win) ## disabled

            plot_history <<- NULL

            invisible(0)
        }, ## end initialization
        ## set up the menu bar widget
        initializeMenu = function(cont, disposeR) {
            menuBarWidget <<- iNZMenuBarWidget$new(.self, cont, disposeR)

            addActDocObs(
                function() {
                    menuBarWidget$defaultMenu()
                }
            )
        },
        ## set up buttons to switch between data and variable view
        initializeViewSwitcher = function(dataThreshold) {
            viewSwitcherWidget <<- iNZViewSwitcher$new(.self, dataThreshold)
            .self$viewSwitcherWidget
        },
        ## set up the display to show the name of the data set
        initializeDataNameWidget = function() {
            ## create the widget
            dataNameWidget <<- iNZDataNameWidget$new(.self)

             ## if the list of active document changes, update the data set name
            addActDocObs(
                function() {
                    dataNameWidget$updateWidget()
                }
            )
            ## if the dataSet changes, update the data set name
            getActiveDoc()$addDataObserver(
                function() {
                    dataNameWidget$updateWidget()
                }
            )
            ## if the name changes, update the value
            getActiveDoc()$getModel()$addNameObserver(
                function() {
                    dataNameWidget$updateWidget()
                    rhistory$update()
                }
            )
            .self$dataNameWidget
        },
        ## set up the widget to display/edit the loaded dataSet
        initializeDataView = function(dataThreshold) {
            ## create the widget
            dataViewWidget <<- iNZDataViewWidget$new(.self, dataThreshold)
            ## if the list of active document changes, update the data view
            addActDocObs(
                function() {
                    dataViewWidget$updateWidget()
                    viewSwitcherWidget$updateWidget()
                }
            )
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

            ## if the list of active document changes, update the data view
            addActDocObs(
                function() {
                    ctrlWidget$updateVariables()
                }
            )
            ## if the dataSet changes, update the variable View
            getActiveDoc()$addDataObserver(
                function() {
                    ctrlWidget$updateVariables()
                }
            )

            .self$ctrlWidget
        },
        ## set up the summary and inference buttons under the
        ## drag and drop fields
        initializeSummaryBtns = function() {
            sumGrp <- ggroup()
            sumBtn <<- gbutton(
                "Get Summary",
                handler = function(h, ...) {
                    curSet <- getActiveDoc()$getSettings()
                    curSet$plottype <- NULL
                    if (!is.null(curSet$freq))
                        curSet$freq <- getActiveData()[[curSet$freq]]
                    if (!is.null(curSet$x)) {
                        if (is_num(curSet$x) & is_num(curSet$y)) {
                            tmp.x <- curSet$y
                            curSet$y <- curSet$x
                            curSet$x <- tmp.x
                            v <- curSet$varnames
                            curSet$varnames$x <- v$y
                            curSet$varnames$y <- v$x
                        }

                        ## Design or data?
                        curMod <- getActiveDoc()$getModel()
                        if (!is.null(curMod$dataDesign)) {
                            curSet$data <- NULL
                            curSet$design <- curMod$createSurveyObject()
                        }

                        w <- gwindow(
                            "Summary",
                            width = 800 * preferences$font.size / 10,
                            height = 400 * preferences$font.size / 10,
                            visible = FALSE,
                            parent = win
                        )
                        g <- gvbox(container = w)
                        txtSmry <- gtext(
                            text = paste(
                                do.call(iNZightPlots:::getPlotSummary, curSet),
                                collapse = "\n"
                            ),
                            expand = TRUE,
                            container = g,
                            wrap = FALSE,
                            font.attr = list(
                                family = "monospace",
                                size = preferences$font.size
                            )
                        )

                        ## if regression OR anova is going on:
                        if (is.null(curSet$g1) &&
                            is.null(curSet$g2) &&
                            !is.null(curSet$y) &&
                            (is_num(curSet$x) | is_num(curSet$y)) &&
                            (!is.null(curSet$trend) | curSet$smooth > 0 |
                             !is_num(curSet$x) | !is_num(curSet$y))) {
                            btngrp <- ggroup(container = g)
                            addSpace(btngrp, 5)

                            btnHandler <- function(h, ...) {
                                varType <- ifelse(
                                    grepl("residuals", svalue(h$obj)),
                                    "residual",
                                    "predict"
                                )

                                ## window asking for variable names:
                                w2 <- gwindow("Store fitted values", width = 350,
                                              parent = w, visible = FALSE)

                                g2 <- gvbox(container = w2)
                                g2$set_borderwidth(15)

                                lbl <- glabel(
                                    sprintf(
                                        "Specify names for the new variable%s",
                                        ifelse(length(curSet$trend) > 1, "s", "")),
                                    container = g2,
                                    anchor = c(-1, -1)
                                )
                                font(lbl) <- list(size = 12, weight = "bold")

                                addSpace(g2, 20)


                                tbl <- glayout(container = g2)
                                ii <- 1

                                ## Predicted values for GROUP MEANS:
                                fittedLbl <- glabel("")
                                fittedName <- gedit(
                                    sprintf(
                                        "%s.%s",
                                        curSet$varnames[[ifelse(is_num(curSet$y), "y", "x")]],
                                        varType),
                                    width = 25
                                )

                                if (is_cat(curSet$x) || is_cat(curSet$y)) { ##} || length(curSet$trend) == 1) {
                                    tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl
                                    tbl[ii, 4:6, expand = TRUE] <- fittedName
                                    ii <- ii + 1
                                }

                                ## Predicted values for LINEAR trend:
                                fittedLbl.lin <- glabel(
                                    ifelse(length(curSet$trend) > 1, "Linear :", "")
                                )
                                fittedName.lin <- gedit(
                                    sprintf("%s.%s%s", curSet$varnames$y, varType,
                                            ifelse(length(curSet$trend) > 1, ".linear", "")),
                                    width = 25
                                )
                                if (length(curSet$trend) >= 1 && "linear" %in% curSet$trend) {
                                    tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl.lin
                                    tbl[ii, 4:6, expand = TRUE] <- fittedName.lin
                                    ii <- ii + 1
                                }

                                ## Predicted values for QUADRATIC trend:
                                fittedLbl.quad <- glabel(
                                    ifelse(length(curSet$trend) > 1, "Quadratic :", "")
                                )
                                fittedName.quad <- gedit(
                                    sprintf("%s.%s%s", curSet$varnames$y, varType,
                                            ifelse(length(curSet$trend) > 1, ".quadratic", "")),
                                    width = 25
                                )
                                if (length(curSet$trend) >= 1 && "quadratic" %in% curSet$trend) {
                                    tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl.quad
                                    tbl[ii, 4:6, expand = TRUE] <- fittedName.quad
                                    ii <- ii + 1
                                }

                                ## Predicted values for CUBIC trend:
                                fittedLbl.cub <- glabel(
                                    ifelse(length(curSet$trend) > 1, "Cubic :", "")
                                )
                                fittedName.cub <- gedit(
                                    sprintf("%s.%s%s", curSet$varnames$y, varType,
                                            ifelse(length(curSet$trend) > 1, ".cubic", "")),
                                    width = 25
                                )
                                if (length(curSet$trend) >= 1 && "cubic" %in% curSet$trend) {
                                    tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl.cub
                                    tbl[ii, 4:6, expand = TRUE] <- fittedName.cub
                                    ii <- ii + 1
                                }

                                ## Predicted values for SMOOTHER:
                                fittedLbl.smth <- glabel("Smoother :")
                                fittedName.smth <- gedit(
                                    sprintf("%s.%s.smooth", curSet$varnames$y, varType),
                                    width = 25
                                )
                                if (curSet$smooth > 0 && is_num(curSet$x) &&
                                    is_num(curSet$y)) {
                                    tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl.smth
                                    tbl[ii, 4:6, expand = TRUE] <- fittedName.smth
                                    ii <- ii + 1
                                }

                                addSpring(g2)

                                okBtn <- gbutton(
                                    "Ok",
                                    icon = "save",
                                    handler = function(h, ...) {
                                        FUN <-
                                            if (varType == "predict")
                                                function(object)
                                                    predict(
                                                        object,
                                                        newdata = data.frame(x = curSet$x, stringsAsFactors = TRUE)
                                                    )
                                            else
                                                function(object)
                                                    residuals(object)

                                        pred <- NULL
                                        if (is_cat(curSet$x) || is_cat(curSet$y)) { #} || length(curSet$trend) == 1) {
                                            ## just the one
                                            fit <- with(curSet, lm(if (is_num(curSet$y)) y ~ x else x ~ y, na.action = na.exclude))
                                            pred <- data.frame(FUN(fit), stringsAsFactors = TRUE)
                                            colnames(pred) <- svalue(fittedName)
                                        } else if (length(curSet$trend) >= 1) {
                                            ## for each trend line
                                            fits <- lapply(curSet$trend, function(ord) {
                                                with(curSet, switch(ord,
                                                                    "linear"    = lm(y ~ x, na.action = na.exclude),
                                                                    "quadratic" = lm(y ~ x + I(x^2), na.action = na.exclude),
                                                                    "cubic"     = lm(y ~ x + I(x^2) + I(x^3), na.action = na.exclude)))
                                            })
                                            pred <- sapply(fits, function(f) FUN(f))
                                            colnames(pred) <- sapply(curSet$trend, function(ord) {
                                                switch(ord,
                                                       "linear" = svalue(fittedName.lin),
                                                       "quadratic" = svalue(fittedName.quad),
                                                       "cubic" = svalue(fittedName.cub))
                                            })
                                        }
                                        if (!is.null(pred))
                                            newdata <- data.frame(getActiveData(), pred, stringsAsFactors = TRUE)
                                        else
                                            newdata <- getActiveData()


                                        if (curSet$smooth > 0 && is_num(curSet$x) && is_num(curSet$y)) {
                                            tmp <- data.frame(x = curSet$x, y = curSet$y, stringsAsFactors = TRUE)
                                            fit <- with(curSet, loess(y ~ x, span = curSet$smooth, family = "gaussian", degree = 1, na.action = "na.exclude"))
                                            pred <- data.frame(FUN(fit), stringsAsFactors = TRUE)
                                            colnames(pred) <- svalue(fittedName.smth)
                                            newdata <- data.frame(newdata, pred, stringsAsFactors = TRUE)
                                        }


                                        getActiveDoc()$getModel()$updateData(newdata)

                                        dispose(w2)
                                    },
                                    container = g2)

                                visible(w2) <- TRUE
                            }

                            predBtn <- gbutton("Store fitted values", container = btngrp, handler = btnHandler)
                            residBtn <- gbutton("Store residuals", container = btngrp, handler = btnHandler)

                            addSpace(g, 0)
                        }

                        visible(w) <- TRUE
                    } else {
                        w <- gwindow("Summary",
                                     width = 800 * preferences$font.size / 10,
                                     height = 400 * preferences$font.size / 10,
                                     visible = FALSE, parent = win)
                        g <- gvbox(container = w)
                        txtSmry <- gtext(
                            text = paste(
                                iNZightPlots::getPlotSummary(data = getActiveData()),
                                collapse = "\n"
                            ),
                            expand = TRUE,
                            container = g,
                            wrap = FALSE,
                            font.attr = list(
                                family = "monospace",
                                size = preferences$font.size
                            )
                        )
                        visible(w) <- TRUE
                    }
                }
            )
            infBtn <<- gbutton(
                "Get Inference",
                handler = function(h, ...) {
                    curSet <- getActiveDoc()$getSettings()
                    curSet$plottype <- NULL
                    if (!is.null(curSet$freq))
                        curSet$freq <- getActiveData()[[curSet$freq]]
                    if (!is.null(curSet$x)) {
                        ## Figure out what type of inference will be happening:
                        xnum <- is_num(curSet$x)

                        if (is.null(curSet$y)) {
                            INFTYPE <- ifelse(xnum, "onesample-ttest", "oneway-table")
                        } else {
                            ynum <- is_num(curSet$y)
                            if (xnum && ynum) {
                                INFTYPE <- "regression"
                            } else if (xnum | ynum) {
                                M <-
                                    if (xnum) length(levels(curSet$y))
                                    else length(levels(curSet$x))
                                if (M == 2) INFTYPE <- "twosample-ttest"
                                if (M > 2) INFTYPE <- "anova"
                            } else {
                                INFTYPE <- "twoway-table"
                            }
                        }

                        if (INFTYPE == "regression") {
                            tmp.x <- curSet$y
                            curSet$y <- curSet$x
                            curSet$x <- tmp.x
                            v <- curSet$varnames
                            curSet$varnames$x <- v$y
                            curSet$varnames$y <- v$x
                        }

                        ## Design or data?
                        curMod <- getActiveDoc()$getModel()
                        is_survey <- FALSE
                        if (!is.null(curMod$dataDesign)) {
                            curSet$data <- NULL
                            curSet$design <- curMod$createSurveyObject()
                            is_survey <- TRUE
                        }

                        w <- gwindow(
                            "Get Inference",
                            width = 350,
                            #height = 400,
                            parent = win,
                            visible = FALSE
                        )
                        g <- gvbox(container = w, expand = TRUE, fill = TRUE)
                        g$set_borderwidth(5)

                        tbl <- glayout(container = g)
                        ii <- 1

                        ## Inference method
                        lbl <- glabel("Method :")
                        infMthd <- gradio(c("Normal", "Bootstrap"), horizontal = TRUE)
                        if (!is_survey) {
                            tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- lbl
                            tbl[ii, 4:6, expand = TRUE] <- infMthd
                            ii <- ii + 1
                        }

                        ii <- ii + 1

                        doHypTest <- grepl("ttest|anova|table", INFTYPE)
                        if (doHypTest &&
                            is_survey &&
                            grepl("oneway-table", INFTYPE)) {
                            ## don't do it for chi-square (yet)
                            doHypTest <- length(levels(curSet$x)) == 2
                        }
                        test.type <- switch(
                            INFTYPE,
                            "onesample-ttest" = "One Sample t-test",
                            "twosample-ttest" = "Two Sample t-test",
                            "anova"           = "ANOVA",
                            "regression"      = "Regression Analysis",
                            "oneway-table"    =
                                ifelse(is_survey && length(levels(curSet$x)) == 2,
                                    "Test proportion",
                                    "Chi-square test"
                                ),
                            "twoway-table"    = "Chi-square test"
                        )

                        ## Checkbox: perform hypothesis test? Activates hypothesis options.
                        TTEST <- grepl("ttest", INFTYPE)
                        TTEST2 <- INFTYPE == "twosample-ttest"
                        CHI2 <- grepl("twoway-table", INFTYPE) ||
                            (grepl("oneway-table", INFTYPE) && !is_survey)
                        PROPTEST <- INFTYPE == "oneway-table"
                        PROPTEST2 <- PROPTEST && length(levels(curSet$x)) == 2
                        hypTest <-
                            if (TTEST2)
                                gradio(c("None", "Two Sample t-test", "ANOVA"), horizontal = TRUE)
                            else if (PROPTEST2 && !is_survey)
                                gradio(c("None", "Chi-square test", "Test proportion"), horizontal = TRUE)
                            else
                                gcheckbox(test.type, checked = FALSE)

                        if (doHypTest) {
                            lbl <- glabel("Hypothesis Testing")
                            font(lbl) <- list(weight = "bold")
                            tbl[ii, 1:6, anchor = c(-1, 0), expand = TRUE] <- lbl
                            ii <- ii + 1

                            tbl[ii, 1:6, anchor = c(1, 0), expand = TRUE] <- hypTest
                            ii <- ii + 1
                        }

                        if (doHypTest) {
                            ## Null hypothesis value
                            lbl <- glabel("Null Value :")
                            hypVal <- gedit("0", width = 10)

                            if (TTEST) {
                                tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- lbl
                                tbl[ii, 4:6, expand = TRUE] <- hypVal
                                ii <- ii + 1
                            } else if (PROPTEST2) {
                                tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- lbl
                                svalue(hypVal) <- "0.5"
                                tbl[ii, 4:6, expand = TRUE] <- hypVal
                                ii <- ii + 1
                            }

                            ## alternative hypothesis
                            lbl <- glabel("Alternative Hypothesis :")
                            hypAlt <- gcombobox(c("two sided", "greater than", "less than"))
                            if (TTEST || PROPTEST2) {
                                tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- lbl
                                tbl[ii, 4:6, expand = TRUE] <- hypAlt
                                ii <- ii + 1
                            }

                            enabled(hypAlt) <- enabled(hypVal) <-
                                if (TTEST2) svalue(hypTest, index = TRUE) == 2
                                else if (PROPTEST2 && !is_survey) svalue(hypTest, index = TRUE) == 3
                                else svalue(hypTest)

                            ## use equal variance assumption?
                            hypEqualVar <- gcheckbox("Use equal-variance", checked = FALSE)
                            if (TTEST2 && !is_survey) {
                                tbl[ii, 4:6, expand = TRUE] <- hypEqualVar
                                ii <- ii + 1

                                enabled(hypEqualVar) <- svalue(hypTest, index = TRUE) == 2
                            }

                            hypSimPval <- gcheckbox("Simulate p-value",
                                checked = FALSE)
                            if (CHI2 && !is_survey) {
                                tbl[ii, 4:6, expand = TRUE] <- hypSimPval
                                ii <- ii + 1

                                enabled(hypSimPval) <-
                                    if (PROPTEST2) svalue(hypTest, index = TRUE) == 2
                                    else svalue(hypTest)
                            }

                            hypExactPval <- gcheckbox("Calculate exact p-value",
                                checked = FALSE)
                            if (PROPTEST2 && !is_survey) {
                                tbl[ii, 4:6, expand = TRUE] <- hypExactPval
                                ii <- ii + 1

                                enabled(hypExactPval) <- svalue(hypTest, index = TRUE) == 3
                            }
                        }

                        addHandlerChanged(hypTest, function(h, ...) {
                            if (CHI2)
                                enabled(hypSimPval) <-
                                    if (PROPTEST2 && !is_survey) svalue(hypTest, index = TRUE) == 2
                                    else svalue(hypTest)
                            enabled(hypExactPval) <-
                                if (PROPTEST2 && !is_survey) svalue(hypTest, index = TRUE) == 3
                                else FALSE
                            enabled(hypEqualVar) <-
                            enabled(hypAlt) <-
                            enabled(hypVal) <-
                                if (TTEST2) svalue(hypTest, index = TRUE) == 2
                                else if (PROPTEST2 && !is_survey) svalue(hypTest, index = TRUE) == 3
                                else svalue(h$obj)
                        })

                        if (INFTYPE == "regression") {
                            lbl <- glabel("Trend options (select at least one)")
                            font(lbl) <- list(weight = "bold")
                            tbl[ii, 1:6, anchor = c(-1, 0), expand = TRUE] <- lbl
                            ii <- ii + 1

                            ## clicking each will update plot settings
                            addCurve <- function(h, ...) {
                                newtrend <- character()
                                if (svalue(trendLinearChk)) newtrend <- c(newtrend, "linear")
                                if (svalue(trendQuadChk)) newtrend <- c(newtrend, "quadratic")
                                if (svalue(trendCubicChk)) newtrend <- c(newtrend, "cubic")
                                getActiveDoc()$setSettings(list(trend = newtrend))
                            }
                            curtrend <- curSet$trend
                            trendLinearChk <- gcheckbox(
                                "Linear trend",
                                checked = "linear" %in% curtrend,
                                handler = addCurve
                            )
                            tbl[ii, 2:6] <- trendLinearChk
                            ii <- ii + 1
                            trendQuadChk <- gcheckbox(
                                "Quadratic trend",
                                checked = "quadratic" %in% curtrend,
                                handler = addCurve
                            )
                            tbl[ii, 2:6] <- trendQuadChk
                            ii <- ii + 1
                            trendCubicChk <- gcheckbox(
                                "Cubic trend",
                                checked = "cubic" %in% curtrend,
                                handler = addCurve
                            )
                            tbl[ii, 2:6] <- trendCubicChk
                            ii <- ii + 1
                        }

                        btn <- gbutton("OK", handler = function(h, ...) {
                            infType <- svalue(infMthd, index = TRUE)
                            curSet <- getActiveDoc()$getSettings()
                            curSet$plottype <- NULL
                            curMod <- getActiveDoc()$getModel()
                            if (!is.null(curMod$dataDesign)) {
                                curSet$data <- NULL
                                curSet$design <- curMod$createSurveyObject()
                            }
                            if (!is.null(curSet$freq))
                                curSet$freq <- getActiveData()[[curSet$freq]]
                            if (!is.null(curSet$x)) {
                                if (is.numeric(curSet$x) & is.numeric(curSet$y)) {
                                    tmp.x <- curSet$y
                                    curSet$y <- curSet$x
                                    curSet$x <- tmp.x
                                    v <- curSet$varnames
                                    curSet$varnames$x <- v$y
                                    curSet$varnames$y <- v$x
                                }
                            }
                            if (is.null(curSet$g1) & !is.null(curSet$g2)) {
                                if (curSet$g2.level != "_ALL") {
                                    curSet$g1 <- curSet$g2
                                    curSet$g1.level <- curSet$g2.level
                                    curSet$varnames$g1 <- curSet$varnames$g2
                                }
                                curSet$g2 <- NULL
                                curSet$g2.level <- NULL
                                curSet$varnames$g2 <- NULL
                            }
                            curSet <- modifyList(
                                curSet,
                                list(bs.inference = infType == 2,
                                     summary.type = "inference",
                                     inference.type = "conf",
                                     inference.par = NULL)
                            )
                            if ((TTEST2 && svalue(hypTest, index = TRUE) > 1) ||
                                (PROPTEST2 && !is_survey && svalue(hypTest, index = TRUE) > 1) ||
                                (PROPTEST2 && is_survey && svalue(hypTest)) ||
                                (!TTEST2 && !PROPTEST2 && svalue(hypTest))) {
                                if (is.na(as.numeric(svalue(hypVal)))) {
                                    gmessage(
                                        "Null value must be a valid number.",
                                        title = "Invalid Value",
                                        icon = "error"
                                    )
                                    return()
                                }
                                curSet <- modifyList(
                                    curSet,
                                    list(
                                        hypothesis.value = as.numeric(svalue(hypVal)),
                                        hypothesis.alt = switch(
                                            svalue(hypAlt, index = TRUE),
                                            "two.sided", "greater", "less"
                                        ),
                                        hypothesis.var.equal = svalue(hypEqualVar),
                                        hypothesis.simulated.p.value = svalue(hypSimPval),
                                        hypothesis.use.exact = svalue(hypExactPval),
                                        hypothesis.test =
                                            if (TTEST2)
                                                switch(
                                                    svalue(hypTest, index = TRUE),
                                                    "default", "t.test", "anova"
                                                )
                                            else if (PROPTEST2 && !is_survey)
                                                switch(svalue(hypTest, index = TRUE),
                                                    "default", "chi2", "proportion"
                                                )
                                            else if (PROPTEST2 && is_survey)
                                                "proportion"
                                            else
                                                "default"
                                    )
                                )
                            } else {
                                curSet <- modifyList(
                                    curSet,
                                    list(hypothesis = NULL),
                                    keep.null = TRUE
                                )
                            }

                            if (INFTYPE == "regression") {
                                tr <- getActiveDoc()$getSettings()$trend
                                if (is.null(tr) || length(tr) == 0) {
                                    gmessage("Please choose at least one trend option", icon = "error",
                                        title = "No trend selected", parent = win)
                                    return()
                                }
                            }

                            ## Close setup window and display results
                            dispose(w)

                            infTitle <- "Inference Information"
                            if (infType == 2) {
                                ## Not sure why this acts weird. At least on Linux, the text inside `wBoots` doesn't become visible until the
                                ## function has finished.
                                wBoots <- gwindow(
                                    "Performing Bootstrap ...",
                                    parent = win,
                                    width=350,
                                    height=120,
                                    visible = FALSE
                                )
                                ggBoots <- ggroup(container = wBoots)
                                addSpace(ggBoots, 5)
                                hBoots <- gvbox(container = ggBoots, spacing = 10)
                                addSpace(hBoots, 5)
                                iBoots <- gimage(stock.id = "info", cont = hBoots, size = "dialog")
                                fBoots <- gvbox(container = ggBoots, spacing = 10)
                                fBoots$set_borderwidth(10)
                                gBoots <- glabel(
                                    "Please wait while iNZight\nperforms bootstrap simulations.",
                                    anchor = c(-1, 0),
                                    cont = fBoots
                                )
                                font(gBoots) <- list(size = 14, weight = "bold")
                                gBoots2 <- glabel(
                                    "Depending on the size of the data,\nthis could take a while.",
                                    anchor = c(-1, 0),
                                    cont = fBoots
                                )
                                visible(wBoots) <- TRUE
                            }

                            w2 <- gwindow(infTitle,
                                          width = 800 * preferences$font.size / 10,
                                          height = 400 * preferences$font.size / 10,
                                          visible = FALSE, parent = win)
                            g2 <- gtext(
                                paste(
                                    do.call(
                                        iNZightPlots:::getPlotSummary,
                                        curSet
                                    ),
                                    collapse = "\n"
                                ),
                                expand = TRUE,
                                cont = w2,
                                wrap = FALSE,
                                font.attr = list(
                                    family = "monospace",
                                    size = preferences$font.size
                                )
                            )
                            visible(w2) <- TRUE
                            try(dispose(wBoots), silent = TRUE)
                            invisible(w2)
                        })

                        ## if no hypothesis to choose from, skip window

                        addSpring(g)
                        add(g, btn)

                        # if (!doHypTest) {
                            # btn$invoke_change_handler()
                        # } else {
                            visible(w) <- TRUE
                        # }
                        invisible(w)
                    } else {
                        gmessage("Please select at least one variable",
                                 parent = win)
                    }
                })
            font(sumBtn) <<-
                list(weight = "bold", family = "sans", color = "navy")
            font(infBtn) <<-
                list(weight = "bold", family = "sans", color = "navy")
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
            plotToolbar <<- iNZPlotToolbar$new(.self, cont)
        },
        ## if set upon gui startup, close the R sessions when
        ## the gui is closed
        closerHandler = function(disposeR) {
            addHandlerUnrealize(win, handler = function(h, ...) {
                confirm <- gconfirm(
                    title = "Are you sure?",
                    msg = "Do you wish to quit iNZightVIT?",
                    icon = "question",
                    parent = win
                )
                if (confirm) {
                    if (disposeR) {
                        q(save = "no")
                    } else {
                        ##dispose(win)
                        try(dev.off(), silent = TRUE)
                        return(FALSE)
                    }
                }

                TRUE
            })
        },
        ## plot with the current active plot settings
        updatePlot = function(allow.redraw = TRUE) {
            curPlSet <- getActiveDoc()$getSettings()
            if (!is.null(curPlSet$freq))
                curPlSet$freq <- getActiveData()[[curPlSet$freq]]
            if(!is.null(curPlSet$x)) {
                # Switch x and y:
                if (is_num(curPlSet$x) & is_num(curPlSet$y)) {
                    x.tmp <- curPlSet$y
                    curPlSet$y <- curPlSet$x
                    curPlSet$x <- x.tmp

                    x.tmp <- curPlSet$varnames$y
                    curPlSet$varnames$y <- curPlSet$varnames$x
                    curPlSet$varnames$x <- x.tmp
                }
                ## Design or data?
                curMod <- getActiveDoc()$getModel()
                if (!is.null(curMod$dataDesign)) {
                    curPlSet$data <- NULL
                    curPlSet$design <- curMod$createSurveyObject()
                }

                curPlSet$data_name <- dataNameWidget$datName

                ## Suppress the warnings produced by iNZightPlot ...
                suppressWarnings({
                    ## Generate the plot ... and update the interaction button
                    curPlot <<- unclass(rawpl <- do.call(iNZightPlot, curPlSet))
                    if (allow.redraw & !is.null(attr(curPlot, "dotplot.redraw")))
                        if (attr(curPlot, "dotplot.redraw"))
                            curPlot <<- unclass(rawpl <- do.call(iNZightPlot, curPlSet))
                    enabled(plotToolbar$exportplotBtn) <<- can.interact(rawpl)
                })
                plotType <<- attr(curPlot, "plottype")
            } else {
                rawpl <- plotSplashScreen()
                curPlot <<- NULL
                plotType <<- "none"
                enabled(plotToolbar$exportplotBtn) <<- FALSE
            }
            invisible(rawpl)
        },
        saveState = function(file) {
            state <- lapply(
                seq_along(iNZDocuments),
                function(i) {
                    list(
                        document = iNZDocuments[[i]],
                        plot_settings = iNZDocuments[[i]]$getSettings()
                    )
                }
            )
            save(state, file = file)
        },
        loadState = function(file, .alert = TRUE) {
            if (!file.exists(file)) {
                if (.alert)
                    gmessage("File doesn't exist", icon = "error")
                return()
            }

            e <- new.env()
            load(file, envir = e)
            if (is.null(e$state)) {
                if (.alert)
                    gmessage("That file doesn't seem to be a valid iNZight save.",
                        icon = "error"
                    )
                return()
            }

            setState(e$state)
        },
        setState = function(state) {
            lapply(
                state,
                function(doc) {
                    setDocument(doc$document)
                    Sys.sleep(0.2)
                    getActiveDoc()$setSettings(doc$plot_settings, reset = TRUE)
                    ctrlWidget$setState(doc$plot_settings)
                }
            )
            invisible(NULL)
        },
        ## set a new iNZDocument and make it the active one
        setDocument = function(document, reset = FALSE) {
            if (reset) {
                ## delete all documents; start from scratch.
                ctrlWidget$resetWidget()
                Nk <- length(iNZDocuments)
                iNZDocuments <<- list(document)
                ## add a separator to code history
                rhistory$add(c(
                    "SEP",
                    sprintf(
                        "## Exploring the '%s' dataset",
                        attr(document$getData(), "name", exact = TRUE)
                    )
                ))
            } else {
                ## give the new document a good name
                names <- sapply(iNZDocuments, function(d) attr(d$getData(), "name", exact = TRUE))
                i <- 2
                newname <- attr(document$getData(), "name", exact = TRUE)
                while (newname %in% names) {
                    newname <- sprintf("%s_%s", newname, i)
                    i <- i + 1
                }
                attr(document$dataModel$dataSet, "name") <- newname
                ## reset control widget
                # state <- ctrlWidget$getState()
                pset <- getActiveDoc()$getSettings()
                ctrlWidget$resetWidget()
                ## add a iNZDocument to the end of the doc list
                iNZDocuments <<- c(iNZDocuments, list(document))
            }
            ## clean up any 'empty' datasets ..
            iNZDocuments <<- iNZDocuments[sapply(iNZDocuments, function(d)
                !all(dim(d$dataModel$dataSet) == 1))]
            ## set the active document to the one we added
            activeDoc <<- length(iNZDocuments)
            ## if the dataSet changes, update the variable View
            ## and the settings to take into account possible
            ## change of currently selected data
            getActiveDoc()$addDataObserver(
                function() {
                    dataViewWidget$updateWidget()
                    getActiveDoc()$updateSettings()
                    ctrlWidget$updateVariables()
                    dataNameWidget$updateWidget()
                    rhistory$update()
                }
            )
            ## if the name changes, update the value
            getActiveDoc()$getModel()$addNameObserver(
                function() {
                    dataNameWidget$updateWidget()
                    rhistory$update()
                }
            )
            ## if plotSettings change, update the plot
            getActiveDoc()$addSettingsObserver(function() updatePlot())

            if (!reset) ctrlWidget$setState(pset)
            else {
                dataViewWidget$updateWidget()
                getActiveDoc()$updateSettings()
                ctrlWidget$updateVariables()
                dataNameWidget$updateWidget()
                rhistory$update()
            }
            updatePlot()
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
        },
        view_dataset = function() {
            d <- getActiveData()
            utils::View(d, title = attr(d, "name"))
        },
        ## data check
        checkData = function(module) {
            data = .self$getActiveData()
            vars = names(data)
            ret = TRUE

            ## If dataset is empty (no data imported) display type 1 message,
            ## otherwise check whether imported data is appropriate for module
            ## (if wrong data type, display type 2 message)
            if (length(vars) == 1 && vars[1] == "empty") {
                ## check for empty data
                displayMsg(module, type = 1)
                ret = FALSE
            }

            return(ret)
        },
        restoreDataset = function() {
            setDocument(iNZDocument$new(
                data = iNZDocuments[[1]]$getModel()$origDataSet
            ))
        },
        ## delete the current dataset
        deleteDataset = function() {
            if (activeDoc == 1) {
                gmessage(
                    "Sorry, but you can't delete this dataset (it's the original, afterall!).",
                    title = "Unable to delete original data set",
                    icon = "warning",
                    parent = .self$win
                )
            } else {
                conf <- gconfirm(
                    paste0(
                        "You are about to delete (permanently!) ",
                        "the currently selected dataset:\n\n",
                        attr(iNZDocuments[[activeDoc]]$getData(), "name", exact = TRUE), "\n\n",
                        "Are you sure you want to continue?"
                    ),
                    title = "You sure you want to delete this data?",
                    icon = "question",
                    parent = .self$win
                )
                if (conf) {
                    todelete <- activeDoc
                    activeDoc <<- activeDoc - 1
                    rhistory$disabled <<- TRUE
                    iNZDocuments <<- iNZDocuments[-todelete]
                    rhistory$disabled <<- FALSE
                    dataNameWidget$updateWidget()
                }
            }
        },
        removeDesign = function() {
            getActiveDoc()$getModel()$setDesign()
            updatePlot()
            ## ENABLE A WHOLE LOT OF STUFF
            # enabled(menubar$menu_list[["Dataset"]][[3]]) <<- TRUE
            # enabled(menubar$menu_list[["Variables"]][["Numeric Variables"]][[2]]) <<- TRUE
            # enabled(menubar$menu_list[["Plot"]][[3]]) <<- TRUE
            # enabled(sumBtn) <<- TRUE
            # enabled(infBtn) <<- TRUE
        },
        ## display warning message
        displayMsg = function(module, type) {
            if (type == 1) {
                gmessage(
                    msg = paste(
                        "A dataset is required to use the",
                        module,
                        "module"
                    ),
                    title = "Empty data",
                    icon = "error"
                )
            } else if (type == 2) {
                gmessage(
                    msg = paste(
                        "Imported dataset is not appropriate for",
                        module,
                        "module"
                    ),
                    title = "Inappropriate data type",
                    icon = "error"
                )
            }
        },
        ## create a gvbox object into the module window (ie, initialize it)
        ## NOTE: should be run every time when a new module is open
        initializeModuleWindow = function(mod, title, scroll = FALSE, border = 0) {
            ## delete any old ones:
            if (length(.self$leftMain$children) > 1) {
                delete(.self$leftMain, .self$leftMain$children[[2]])
            }
            ## create a gvbox in moduleWindow
            # to improve between-module similarity, set up here with
            # -> head, body, footer (for buttons)
            modContainer <- gvbox(container = leftMain, expand = TRUE)

            moduleWindow <<-
                list(
                    header =
                        gvbox(container = modContainer),
                    body =
                        gvbox(
                            spacing = 10,
                            use.scrollwindow = ifelse(scroll, "y", FALSE),
                            container = modContainer,
                            expand = TRUE
                        ),
                    footer =
                        ggroup(container = modContainer)
                )

            if (!missing(title)) {
                title <- glabel(title)
                font(title) <- list(weight = "bold", size = 12)
                add(moduleWindow$header, title, anchor = c(0, 0))
            }

            if (border > 0) moduleWindow$body$set_borderwidth(border)

            visible(gp1) <<- FALSE

            if (!missing(mod))
                activeModule <<- mod

            invisible(moduleWindow)
        },
        initializeCodeHistory = function() {
            rhistory <<- iNZcodeWidget$new(.self)

            addActDocObs(
                function() {
                    rhistory$update()
                }
            )
            getActiveDoc()$addDataObserver(
                function() {
                    rhistory$update()
                }
            )
        },
        initializePlotHistory = function() {
            if (is.null(plot_history)) {
                plot_history <<- iNZplothistory(.self)
            }
        },
        ## --- PREFERENCES SETTINGS and LOCATIONS etc ...
        defaultPrefs = function() {
            ## The default iNZight settings:
            list(
                check.updates = TRUE,
                window.size = c(1250, 850),
                popout = FALSE,
                font.size = 10,
                dev.features = FALSE
            )
        },
        checkPrefs = function(prefs) {
            allowed.names <- c(
                "check.updates",
                "window.size",
                "popout",
                "font.size",
                "dev.features"
            )

            ## Only keep allowed preferences --- anything else is discarded
            prefs <- prefs[names(prefs) %in% allowed.names]
            defs <- defaultPrefs()

            ## check.updates = TRUE | FALSE
            prefs$check.updates <-
                if (is.null(prefs$check.updates)) defs$check.updates
                else if (!is.na(prefs$check.updates) & is.logical(prefs$check.updates)) prefs$check.updates
                else defs$check.updates

            ## window.size = c(WIDTH, HEIGHT)
            prefs$window.size <-
                if (is.null(prefs$window.size)) defs$window.size
                else if (length(prefs$window.size) != 2) defs$window.size
                else if (is_num(prefs$window.size)) prefs$window.size
                else defs$window.size

            ## pop-out layout = FALSE
            prefs$popout <-
                if (is.null(prefs$popout)) defs$popout
                else if (is.logical(prefs$popout)) prefs$popout
                else defs$popout

            ## font size
            prefs$font.size <-
                if (is.null(prefs$font.size) || !is_num(prefs$font.size)) defs$font.size
                else prefs$font.size

            prefs$dev.features <-
                if (is.null(prefs$dev.features) || !is.logical(prefs$dev.features)) defs$dev.features
                else prefs$dev.features

            prefs

        },
        getPreferences = function() {
            ## --- GET THE PREFERENCES
            ## Windows: the working directory will be set as $INSTDIR (= C:\Program Files (x86) by default)
            ##      NOTE: "~" -> C:\Users\<user>\Documents on windows!!!!!
            ##     1. ~\iNZightVIT\.inzight -> this is where it goes for Most users
            ##     2. ~\.inzight -> fallback for R users
            ##
            ## Mac: the working directory will be set as /Applications/iNZightVIT/
            ##     1. ~/Documents/iNZightVIT/.inzight -> again, default
            ##     2. ~/.inzight -> fallback for R users
            ##
            ## Linux: user installs manually, at least for now, working directory will be wherever they run R from ...
            ##     1. ~/.inzight
            ##     2. $(pwd)/.inzight -> overrides (1) if present

            ## If Windows or Mac, set the working directory to Documents/iNZightVIT if possible ...

            prefs.location <<-switch(
                OS,
                "windows" = {
                    if (file.exists(file.path("~", "iNZightVIT"))) {
                        path <- file.path("~", "iNZightVIT", ".inzight")
                        # on new windows installer, nest prefs file one deeper
                        if (dir.exists(path))
                            path <- file.path(path, ".inzight")
                    } else {
                        path <- file.path("~", ".inzight")
                    }

                    path
                },
                "mac" = {
                    if (file.exists(file.path("~", "Documents", "iNZightVIT"))) {
                        path <- file.path("~", "Documents", "iNZightVIT", ".inzight")
                    } else {
                        path <- file.path("~", ".inzight")
                    }

                    path
                },
                "linux" = {
                    path <- file.path("~", ".inzight")

                    if (file.exists(".inzight"))
                        path <- file.path(".inzight")

                    path
                }
            )

            tt <- try({
                preferences <<-
                    if (file.exists(prefs.location)) {
                        checkPrefs(dget(prefs.location))
                    } else {
                        defaultPrefs()
                    }
            }, TRUE)

            if (inherits(tt, "try-error"))
                preferences <<- defaultPrefs()
        },
        savePreferences = function() {
            ## attempt to save the preferences in the expected location:
            tt <- try(dput(preferences, prefs.location), silent = TRUE)
            if (inherits(tt, "try-error")) {
                gmessage(
                    paste(
                        "iNZight was unable to save your preferences, so",
                        "they won't carry over into the next session."
                    ),
                    title = "Unable to save preferences",
                    icon = "warning"
                )
            }
        },
        plotSplashScreen = function() {
            if (requireNamespace("png", quietly = TRUE)) {
                img <- png::readPNG(
                    system.file("images/inzight_transp.png", package = "iNZight")
                )
                grid::grid.newpage()
                grid::pushViewport(grid::viewport(
                    height = unit(0.8, "npc"),
                    layout = grid::grid.layout(nrow = 3, ncol = 1,
                                               heights = unit.c(unit(0.2, "npc"),
                                                                unit(2.5, "lines"),
                                                                unit(1, "null")))
                ))

                grid::pushViewport(grid::viewport(layout.pos.row = 1))
                grid::grid.raster(img)
                grid::upViewport()

                grid::pushViewport(grid::viewport(layout.pos.row = 2))
                grid::grid.text(sprintf("Version %s", packageVersion('iNZight')),
                                x = unit(0.8, "npc"), y = unit(0.75, "npc"),
                                just = 'right')
                grid::grid.text(sprintf("Release date: %s",
                                        format(as.Date(packageDescription('iNZight')$Date),
                                               '%d %b %Y')),
                                x = unit(0.8, "npc"), y = unit(0.25, "npc"),
                                just = 'right', gp = gpar(fontsize = 9))
                grid::upViewport()

                grid::pushViewport(grid::viewport(layout.pos.row = 3))
                grid::pushViewport(grid::viewport(
                    y = unit(0.45, "npc"),
                    width = unit(0.8, "npc"), height = unit(0.9, "npc")))

                if (all(dim(getActiveData()) == 1)) {
                    grid::grid.text(
                        "Kia ora and welcome! To get started, import some data.",
                        y = 1, x = 0, just = c("left", "top"),
                        gp = gpar(fontsize = 12, fontface = 'bold')
                    )
                    grid::grid.text(
                        paste0(
                            "Not sure where to go? Try the File menu!\n",
                            "There are some example datasets there ",
                            "if you just want to explore the program."
                        ),
                        y = unit(1, "npc") - unit(3, "lines"), x = 0, just = c("left", "top"),
                        gp = gpar(fontsize = 11)
                    )

                    grid::grid.text(
                        paste(
                            readLines(
                                system.file("splash", "whatsnew.txt",
                                    package = "iNZight"
                                )
                            ),
                            collapse = "\n"
                        ),
                        y = unit(1, "npc") - unit(8, "lines"),
                        x = 0, c(just = "left", "top"),
                        gp = gpar(fontsize = 11)
                    )

                } else {
                    grid::grid.text(
                        "That's some fine looking data ... ",
                        y = 1, x = 0, just = c("left", "top"),
                        gp = gpar(fontsize = 12, fontface = 'bold')
                    )

                    grid::grid.text(
                        paste(
                            readLines(
                                system.file("splash", "instructions.txt",
                                    package = "iNZight"
                                )
                            ),
                            collapse = "\n"
                        ),
                        y = unit(1, "npc") - unit(3, "lines"),
                        x = 0, c(just = "left", "top"),
                        gp = gpar(fontsize = 12)
                    )
                }

                grDevices::dev.flush()
            }
        },
        showHistory = function() {
            wh <- gwindow("R Code History", parent = .self$win,
                          width = 800, height = 500)
            gh <- gvbox(container = wh)
            th <- gtext(container = gh, expand = TRUE, fill = TRUE, wrap = FALSE)
            insert(th, rhistory$get(), font.attr = list(family = "monospace"))
        },
        close = function() {
            if (disposer) q(save = "no") else dispose(win)
        })
    )
