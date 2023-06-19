#' iNZight GUI Class
#'
#' Main class that builds the iNZight GUI
#'
#' @field iNZDocuments A list of documents containing data, plot settings, etc.
#' @field activeDoc The numeric ID of the currently active document
#' @field win The main GUI window
#' @field menuBarWidget the widget component containing the menu bar
#' @field leftMain the left-hand-side panel containing data widgets and controls
#' @field moduleWindow new modules are inserted into this component
#' @field activeModule an indicator pointing at the currently active module
#' @field gp1 containing within the left panel
#' @field gp2 container within middle group
#' @field popOut logical, indicates if the graphics window will be embedded or separate (TRUE)
#' @field dataViewWidget a widget that displays the data to the user
#' @field dataToolbarWidget the widget which lets user switch between data and variable view
#' @field dataNameWidget displays the name of the current dataset, and allows users to switch between datasets
#' @field plotWidget the widget containing the main plot window
#' @field plotToolbar the widget in the bottom-right containing plot control buttons
#' @field ctrlWidget the drop-down boxes allowing users to choose variables
#' @field modWin a container for the current module window
#' @field curPlot the current plot object is returned and stored in this field
#' @field plotType the type of the current plot
#' @field OS the name of the user's operating system
#' @field prefs.location the location user preferences are saved
#' @field preferences the current user preferences
#' @field statusbar (unused currently) a statusbar widget
#' @field moduledata data for the current module, so they can preserve state if closed
#' @field rhistory a module containing the R history for the user's session
#' @field plot_history contains history of gg_* plot types
#' @field disposer the function called when iNZight closes
#' @field addonModuleDir the path where modules are installed
#' @field code_env the environment in which R code is executed
#' @field code_panel the interactive code widget at the bottom of the iNZight window
#' @field is_initialized logical, indicates if iNZight is initialised or not
#' @field stop_loading logical, indicates if iNZight should stop loading
#' @field ui_env the environment in which the GUI is created. Used as a base env
#'  for loading modules.
#'
#' @md
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
            dbcon = "ANY",

            ## the active document of the iNZDocuments list
            activeDoc = "numeric",
            ## the main GUI window
            win = "ANY",
            ## Accelerator key map
            key_map = "ANY",
            ## Menu bar
            menuBarWidget = "ANY",
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
            dataToolbarWidget = "ANY",
            dataNameWidget = "ANY",
            ## widget that handles the plot notebook
            plotWidget = "ANY",
            plotToolbar = "ANY",
            ## widget that handles the drag/drop buttons
            ## under the dataViewWidget
            ctrlWidget = "ANY",
            subsetFilter = "ANY",
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
            disposer = "ANY",
            addonModuleDir = "character",
            activeModules = "list",
            ## This will be used to store the dataset, design, etc..
            ## rather than passing around the full object.
            code_env = "ANY",
            code_panel = "ANY",
            ## loading flags
            is_initialized = "logical",
            stop_loading = "logical",
            ui_env = "ANY"
        ),
        prototype = list(
            activeDoc = 1,
            plotType = "none"
        )
    ),
    methods = list(
        ## Start the iNZight GUI
        ## This is the main method of iNZight and calls all the other
        ## methods of the GUI class.
        initializeGui = function(data = NULL,
                                 dispose_fun = NULL,
                                 addonDir = NULL,
                                 show = TRUE,
                                 stop_loading = FALSE,
                                 ...,
                                 disposer = NULL,
                                 ui_env = parent.frame()) {
            "Initiates the GUI"
            initFields(
                is_initialized = FALSE,
                disposer = disposer,
                activeModules = list(),
                ui_env = ui_env
            )

            if (is.null(disposer)) {
                if (!is.null(dispose_fun) && is.function(dispose_fun)) {
                    disposer <<- function() dispose_fun(...)
                } else {
                    disposer <<- function() {}
                }
            }

            win.title <- paste(
                "iNZight (v",
                packageDescription("iNZight")$Version,
                ")",
                sep = ""
            )

            OS <<-
                if (.Platform$OS == "windows") {
                    "windows"
                } else if (Sys.info()["sysname"] == "Darwin") {
                    "mac"
                } else {
                    "linux"
                }

            ## Grab settings file (or try to!)
            getPreferences()
            if (!is.null(addonDir) && dir.exists(addonDir)) {
                addonModuleDir <<- addonDir
            } else if (!is.null(preferences$module_dir)) {
                addonModuleDir <<- preferences$module_dir
            } else {
                addonModuleDir <<- Sys.getenv("INZIGHT_MODULES_DIR")
            }

            if (dir.exists(addonModuleDir)) load_addons()

            popOut <<- preferences$popout

            win <<- gwindow(
                win.title,
                visible = FALSE,
                width = if (popOut) NULL else preferences$window.size[1],
                height = preferences$window.size[2]
            )

            ## initialize accelerator - do this first so other widgets can use it
            key_map <<- list(accel = RGtk2::gtkAccelGroup())
            win$widget$addAccelGroup(key_map$accel)

            if (!is.null(data) && is.null(attr(data, "name", exact = TRUE))) {
                attr(data, "name") <- deparse(substitute(data))
            }
            iNZDocuments <<- list(iNZDocument$new(data = data, preferences = .self$preferences))

            ## Check for updates ... need to use try incase it fails (no connection etc)
            if (preferences$check.updates && !getOption("inzight.lock.packages", FALSE)) {
                oldpkg <- try(
                    old.packages(
                        repos = "https://r.docker.stat.auckland.ac.nz"
                        # repos = "https://cran.rstudio.com"
                    ),
                    silent = TRUE
                )
                if (!inherits(oldpkg, "try-error") && !is.null(oldpkg) && nrow(oldpkg) > 0) {
                    win.title <- paste(win.title, " [updates available]")
                    win$set_value(win.title)
                }
            }

            gtop <- ggroup(
                horizontal = FALSE,
                container = win,
                use.scrollwindow = FALSE
            )
            menugrp <- ggroup(container = gtop, fill = TRUE)
            initializeMenu(menugrp)

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

            ## What I want this to do is essentially ...
            # 1. layout all the things
            # 2. display "load data" and other options if no data is loaded
            # 3. display the dataset + controls if it is
            # -- so, one single 'DATA' widgets?

            ## display the name of the data set
            add(gp1, .self$initializeDataNameWidget()$widget)

            gtb <- ggroup(container = gp1)
            # addSpace(gtb, 5)
            ## set up buttons to switch between data/var view
            add(gtb, .self$initializeDataToolbar(dataThreshold)$viewGroup)
            addSpring(gtb)
            ## column switcher
            add(gtb, .self$dataViewWidget$colPageGp)

            ## display the data
            add(gp1, dataViewWidget$widget, expand = TRUE)

            ## set up the drag and drop fields
            if (preferences$multiple_x) {
                aLbl <- glabel("CTRL+1 to add selected vars to existing Variable 1 box")
                font(aLbl) <- list(size = 8)
                add(gp1, aLbl, anchor = c(-1, 0))
            }

            add(gp1, initializeControlWidget()$ctrlGp, expand = FALSE)

            ## set up widgets in the right group
            grpRight <- ggroup(
                horizontal = popOut,
                container = gp2, expand = TRUE
            )
            ## set up plot notebook
            initializePlotWidget()
            if (!popOut) {
                add(grpRight, plotWidget$plotNb, expand = TRUE)
            } else {
                addSpace(grpRight, 10)
            }

            ## set up plot toolbar
            plotToolbar <<- ggroup(
                horizontal = !popOut,
                container = grpRight,
                spacing = 10
            )
            size(plotToolbar) <<- if (popOut) c(-1, -1) else c(-1, 45)
            initializePlotToolbar(plotToolbar)
            if (popOut) {
                addSpace(grpRight, 10)
            }

            ## code panel for latest R function call
            code_panel <<- iNZCodePanel$new(.self)
            if (preferences$dev.features && preferences$show.code) {
                add(gtop, code_panel$panel, fill = TRUE)
            }

            set_visible(show)

            ## ensures that all plot control btns are visible on startup
            # svalue(g) <- 0.375
            ## first plot(empty) needs to be added after window is drawn
            ## to ensure the correct device nr
            if (popOut) {
                iNZightTools::newdevice()
            } else {
                tryCatch(plotWidget$addPlot(),
                    error = function(e) {
                        gmessage(
                            "Unable to load built-in graphics device. iNZight will try reloading in dual-window mode.\n\nClick 'close' to continue.",
                            title = "Unable to load graphics device",
                            icon = "error"
                        )

                        # change mode
                        preferences$popout <<- TRUE
                        savePreferences()

                        # reload (and return from initisalize())
                        reload()
                        stop_loading <<- TRUE
                    }
                )
                if (stop_loading) {
                    stop_loading <<- FALSE
                    return()
                }
            }

            ## draw the iNZight splash screen
            plotSplashScreen()

            ## add what is done upon closing the gui
            closerHandler(disposer)

            ## and start tracking history
            initializeCodeHistory()

            ## init statusbar
            statusbar <<- gstatusbar("iNZight is ready") # , container = win) ## disabled

            plot_history <<- NULL
            code_env <<- new.env()

            is_initialized <<- TRUE
            invisible(0)
        }, ## end initialization
        ## set up the menu bar widget
        initializeMenu = function(cont) {
            "Initializes the menu bar at the top of iNZight"
            menuBarWidget <<- iNZMenuBarWidget$new(.self, cont)

            addActDocObs(
                function() {
                    menuBarWidget$defaultMenu()
                }
            )
        },
        ## set up buttons to switch between data and variable view
        initializeDataToolbar = function(dataThreshold) {
            "Initializes the data toolbar widget"
            dataToolbarWidget <<- iNZDataToolbar$new(.self, dataThreshold)
            .self$dataToolbarWidget
        },
        ## set up the display to show the name of the data set
        initializeDataNameWidget = function() {
            "Initializes the data name widget"
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
            "Initializes the data view widget"
            ## create the widget
            dataViewWidget <<- iNZDataViewWidget$new(.self, dataThreshold)
            ## if the list of active document changes, update the data view
            addActDocObs(
                function() {
                    dataViewWidget$updateWidget()
                    dataToolbarWidget$updateWidget()
                }
            )
            ## if the dataSet changes, update the variable View
            getActiveDoc()$addDataObserver(
                function() {
                    dataViewWidget$updateWidget()
                    dataToolbarWidget$updateWidget()
                    getActiveDoc()$updateSettings()
                }
            )
            ## if the settings change, redraw the plot
            getActiveDoc()$addSettingsObjObserver(function() updatePlot())
        },
        ## set up the buttons used for drag and drop and control of
        ## the plot; they update the plotSettings
        initializeControlWidget = function() {
            "Initializes the control panel widget"
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
        ## set up the widget with the plot notebook
        initializePlotWidget = function() {
            "Initializes the plot panel widget"
            plotWidget <<- iNZPlotWidget$new(.self)
        },
        ## set up the buttons under the plot to interact with the plot
        initializePlotToolbar = function(cont) {
            "Initializes the plot toolbar"
            plotToolbar <<- iNZPlotToolbar$new(.self, cont)
        },
        closerHandler = function(disposer) {
            "Adds a close handler that is called when the user closes the iNZight window"
            addHandlerUnrealize(win,
                handler = function(h, ...) {
                    confirm <- gconfirm(
                        title = "Are you sure?",
                        msg = "Do you wish to quit iNZightVIT?",
                        icon = "question",
                        parent = win
                    )
                    if (!confirm) {
                        return(TRUE)
                    }

                    try(dev.off(), silent = TRUE)
                    disposer()
                    FALSE
                }
            )
            addHandlerDestroy(
                win,
                function(h, ...) {
                    # clean up GDF in dataViewWidget
                    if (!is.null(.self$dataViewWidget$dfWidget)) {
                        .self$dataViewWidget$dfView$remove_child(
                            .self$dataViewWidget$dfWidget
                        )
                    }
                }
            )
        },
        ## plot with the current active plot settings
        updatePlot = function(allow.redraw = TRUE) {
            "Updates the plot using the user's chosen variables and other settings"
            if (!is_initialized || !visible(win)) {
                return()
            }

            # disable drag-and-drop until plot rendered
            enabled(dataViewWidget$dfView) <<- enabled(dataViewWidget$varView) <<- FALSE
            on.exit(enabled(dataViewWidget$dfView) <<- enabled(dataViewWidget$varView) <<- TRUE)

            # if plot is NOT `inzplotoutput`, AND code widget is turned on, run the code instead
            if (!is.null(plotType) &&
                plotType == "custom" &&
                preferences$dev.features &&
                preferences$show.code
            ) {
                code_panel$run_code()
                return()
            }

            curPlSet <- getActiveDoc()$getSettings()

            .dataset <- getActiveData(lazy = TRUE)
            .design <- NULL
            curPlSet$data <- quote(.dataset)

            # if (!is.null(curPlSet$freq))
            #     curPlSet$freq <- getActiveData( )[[curPlSet$freq]]
            if (!is.null(curPlSet$x)) {
                if (length(as.character(curPlSet$x)) > 1) {
                    xx <- paste(as.character(curPlSet$x)[-1], collapse = " + ")
                    xx <- strsplit(xx, " + ", fixed = TRUE)[[1]]
                    varx <- iNZightTools::as_tibble(
                        iNZightTools::select(.dataset, xx)
                    )
                    xtypes <- iNZightTools::vartypes(varx)
                } else if (grepl("+", as.character(curPlSet$x), fixed = TRUE)) {
                    xx <- strsplit(as.character(curPlSet$x), " + ", fixed = TRUE)[[1]]
                    varx <- iNZightTools::as_tibble(
                        iNZightTools::select(.dataset, xx)
                    )
                    xtypes <- iNZightTools::vartypes(varx)
                } else {
                    varx <- .dataset[[curPlSet$x]]
                    xtypes <- iNZightTools::vartype(varx)
                }
                vary <- if (!is.null(curPlSet$y)) .dataset[[curPlSet$y]] else NULL

                if (!is.null(vary) && is_cat(vary) && any(xtypes == "cat")) {
                    # if both x and y are categorical - two-way bar graph
                    # -> requires specifying colour palette!
                    if (is.null(curPlSet$col.fun)) {
                        curPlSet$col.fun <- "contrast"
                    }
                    curPlSet["colby"] <- list(NULL)
                } else if (is.null(curPlSet$colby)) {
                    # otherwise, no colby set? remove col.fun
                    curPlSet["col.fun"] <- list(NULL)
                }

                ## Design or data?
                curMod <- getActiveDoc()$getModel()
                if (!is.null(curMod$dataDesign)) {
                    curPlSet$data <- NULL
                    .design <- curMod$createSurveyObject()
                    curPlSet$design <- .design
                }

                curPlSet$data_name <- dataNameWidget$datName
                e <- new.env()
                e$.dataset <- .dataset
                e$.design <- .design

                ## Suppress the warnings produced by iNZightPlot ...
                dop <- try(
                    {
                        ## Generate the plot ... and update the interaction button
                        vartypes <- list(
                            x = xtypes[1],
                            y = NULL
                        )
                        if (!is.null(curPlSet$y)) {
                            vartypes$y <- iNZightTools::vartype(.dataset[[curPlSet$y]])
                        }
                        plot_call <- construct_call(curPlSet, curMod, vartypes)
                        rawpl <- eval(plot_call, e)
                        curPlot <<- unclass(rawpl)
                        if (allow.redraw & !is.null(attr(curPlot, "dotplot.redraw"))) {
                            if (attr(curPlot, "dotplot.redraw")) {
                                curPlot <<- unclass(rawpl <- eval(plot_call, e))
                            }
                        }
                    },
                    silent = TRUE
                )

                if (inherits(dop, "try-error")) {
                    ## Oops!

                    # specific issues we can handle:
                    msg <- attr(dop, "condition")$message
                    if (grepl("following variables in the survey design", msg)) {
                        vars <- strsplit(msg, "\n[ ]+")[[1]][3]
                        plotMessage(
                            heading = "Unable to plot chosen variables",
                            message = paste(
                                sep = "\n",
                                "iNZight cannot plot factor variables in a survey design",
                                "with only one level.",
                                "",
                                "Please remove the following variables: ",
                                paste("   ", vars)
                            ),
                            type = ""
                        )
                        return(invisible(NULL))
                    }

                    message("Call: ")
                    message(attr(dop, "condition")$call)
                    message("\nMessage: ")
                    message(attr(dop, "condition")$message)

                    n_max <- 4
                    err_call <- capture.output(attr(dop, "condition")$call)
                    if (length(err_call) > n_max) {
                        err_call <- c(
                            err_call[1:n_max],
                            sprintf(
                                "+ %i more lines (printed to R console)",
                                length(err_call) - n_max
                            )
                        )
                    }
                    err_msg <- capture.output(cat(attr(dop, "condition")$message))
                    if (length(err_msg) > n_max) {
                        err_msg <- c(
                            err_msg[1:n_max],
                            sprintf(
                                "+ %i more lines (printed to R console)",
                                length(err_msg) - n_max
                            )
                        )
                    }
                    plotMessage(
                        heading = "Oops ... that plot isn't working!",
                        message = paste(
                            "Call: \n",
                            paste(collapse = "\n ", err_call),
                            "\n\nError: \n",
                            paste(collapse = "\n ", err_msg)
                        ),
                        footer = paste(
                            sep = "\n",
                            "If you continue to experience this problem, please send a bug report to",
                            "inzight_support@stat.auckland.ac.nz including",
                            "- a screenshot of the current window",
                            "- a copy of the contents of the R Console",
                            "- a copy of the data (if possible)"
                        )
                    )
                    return(invisible(NULL))
                }

                if (!is.null(attr(curPlot, "code"))) {
                    attr(curPlot, "gg_code") <<- mend_call(attr(curPlot, "code"), .self)
                }
                code <- mend_call(plot_call, .self)
                attr(curPlot, "code") <<- code

                code_panel$set_input(code)
                enabled(plotToolbar$exportplotBtn) <<- can.interact(rawpl)
                plotType <<- attr(curPlot, "plottype")
                return(invisible(rawpl))
            }

            # only runs if unsuccessful
            rawpl <- plotSplashScreen()
            curPlot <<- NULL
            plotType <<- "none"
            enabled(plotToolbar$exportplotBtn) <<- FALSE
            invisible(rawpl)
        },
        removeSignals = function() {
            "Removes signals attached to the active document"
            for (i in seq_along(listeners(activeDocChanged))) {
                activeDocChanged$disconnect(1)
            }
        },
        getState = function() {
            "Returns the current state of the GUI"
            lapply(
                seq_along(iNZDocuments),
                function(i) {
                    list(
                        document = iNZDocuments[[i]],
                        plot_settings = iNZDocuments[[i]]$getSettings()
                    )
                }
            )
        },
        saveState = function(file) {
            "Saves the state of the GUI in `file`"
            state <- getState()
            save(state, file = file)
        },
        loadState = function(file, .alert = TRUE) {
            "Loads the state from a file called `file`"
            if (!file.exists(file)) {
                if (.alert) {
                    gmessage("File doesn't exist", icon = "error")
                }
                return()
            }

            e <- new.env()
            load(file, envir = e)
            if (is.null(e$state)) {
                if (.alert) {
                    gmessage("That file doesn't seem to be a valid iNZight save.",
                        icon = "error"
                    )
                }
                return()
            }

            setState(e$state)
        },
        setState = function(state) {
            "Sets the GUI to the provided state"
            # removeSignals()
            lapply(
                seq_along(state),
                function(i) {
                    doc <- state[[i]]
                    doc$document$removeSignals()
                    setDocument(doc$document, reset = i == 1)
                    Sys.sleep(0.5)
                    while (!is_initialized) {
                        Sys.sleep(0.1)
                    }
                    getActiveDoc()$setSettings(doc$plot_settings, reset = TRUE)
                    Sys.sleep(0.5)
                    ctrlWidget$setState(doc$plot_settings)
                    first <- FALSE
                }
            )
            menuBarWidget$defaultMenu()
            invisible(TRUE)
        },
        create_db_connection = function(name) {
            t <- tempfile(name, fileext = "sqlite")
            dbcon <<- DBI::dbConnect(
                RSQLite::SQLite(),
                t
            )
            invisible(TRUE)
        },
        ## set a new iNZDocument and make it the active one
        setDocument = function(document, reset = FALSE) {
            "Sets the current document"
            is_initialized <<- FALSE

            if (reset) {
                ## delete all documents; start from scratch.
                ctrlWidget$resetWidget()
                Nk <- length(iNZDocuments)
                iNZDocuments <<- list(document)
                ## add a separator to code history
                rhistory$add(
                    c(
                        "SEP",
                        sprintf(
                            "## Exploring the '%s' dataset",
                            attr(document$getData(), "name", exact = TRUE)
                        )
                    )
                )
            } else {
                ## give the new document a good name
                names <- sapply(
                    iNZDocuments,
                    function(d) attr(d$getData(), "name", exact = TRUE)
                )
                i <- 2L
                newname <- attr(document$getData(), "name", exact = TRUE)
                while (newname %in% names) {
                    newname <- sprintf("%s_%s", newname, i)
                    i <- i + 1L
                }
                # TODO: this may cause issue ...
                attr(document$dataModel$dataSet, "name") <- newname
                ## reset control widget
                # state <- ctrlWidget$getState()
                pset <- getActiveDoc()$getSettings()
                ctrlWidget$resetWidget()
                ## add a iNZDocument to the end of the doc list
                iNZDocuments <<- c(iNZDocuments, list(document))

                ## clean up any 'empty' datasets ..
                nonempty_docs <- sapply(
                    iNZDocuments,
                    function(d) {
                        !all(dim(d$dataModel$getData()) == 1)
                    }
                )
                iNZDocuments <<- iNZDocuments[nonempty_docs]
            }

            ## set the active document to the one we added
            activeDoc <<- length(iNZDocuments)
            ## if the dataSet changes, update the variable View
            ## and the settings to take into account possible
            ## change of currently selected data
            getActiveDoc()$addDataObserver(
                function() {
                    dataViewWidget$updateWidget()
                    dataToolbarWidget$updateWidget()
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

            if (!reset) {
                ctrlWidget$setState(pset)
            } else {
                dataViewWidget$updateWidget()
                dataToolbarWidget$updateWidget()
                getActiveDoc()$updateSettings()
                ctrlWidget$updateVariables()
                rhistory$update()
            }
            dataNameWidget$updateWidget()
            dataToolbarWidget$updateWidget()
            dataViewWidget$updateWidget()
            is_initialized <<- TRUE
            updatePlot()
        },
        new_document = function(data, suffix, name) {
            "Create a new document based on the existing one (`getActiveDoc()`)"
            data_name <- ifelse(
                missing(name),
                iNZightTools::add_suffix(.self$dataNameWidget$datName, suffix),
                name
            )
            spec <- .self$getActiveDoc()$getModel()$getDesign()
            prev_name <- ifelse(
                !is.null(spec),
                .self$getActiveDoc()$getModel()$dataDesignName,
                .self$dataNameWidget$datName
            )
            code <- gsub(".dataset", prev_name, attr(data, "code"))
            attr(data, "code") <- code
            if (!is.null(spec) && iNZightTools::is_survey(data)) {
                spec$design <- data
                spec$data <- data$variables
                attr(spec$data, "name") <- data_name
                attr(spec$data, "code") <- attr(data, "code")
                class(spec) <- "inzsvyspec"
                data <- spec
            } else {
                attr(data, "name") <- data_name
            }
            .self$setDocument(iNZDocument$new(data = data, preferences = .self$preferences))
        },
        update_document = function(data) {
            "Update the existing document with new data or survey design"
            spec <- .self$getActiveDoc()$getModel()$getDesign()
            if (!is.null(spec) && iNZightTools::is_survey(data)) {
                spec$design <- data
                spec$data <- data$variables
                attr(spec$data, "name") <- .self$getActiveDoc()$getModel()$name
                attr(spec$data, "code") <- attr(data, "code")
                class(spec) <- "inzsvyspec"
                data <- spec
            } else {
                attr(data, "name") <- .self$getActiveDoc()$getModel()$name
            }
            .self$getActiveDoc()$getModel()$updateData(data)
        },
        getActiveDoc = function() {
            "Returns the currently active document"
            if (activeDoc) iNZDocuments[[activeDoc]] else NULL
        },
        getActiveData = function(lazy = FALSE) {
            "Returns the current dataset (if `lazy` is TRUE, will not be converted to data.frame)"
            if (activeDoc) iNZDocuments[[activeDoc]]$getData(lazy = lazy) else NULL
        },
        ## add observer to the activeDoc class variable
        addActDocObs = function(FUN, ...) {
            "Adds an observer to the active document"
            .self$activeDocChanged$connect(FUN, ...)
        },
        get_data_object = function(nrow, lazy = FALSE) {
            "Returns the current dataset or survey design, if it exists"
            curMod <- .self$getActiveDoc()$getModel()
            if (!is.null(curMod$dataDesign)) {
                res <- curMod$dataDesign$design
            } else {
                res <- .self$getActiveData(lazy = lazy)
            }
            if (!missing(nrow)) {
                res <- res[seq_len(min(nrow, nrow(.self$getActiveData(lazy = TRUE)))), ]
            }
            res
        },
        view_dataset = function() {
            "Views the dataset using the `View()` function"
            d <- getActiveData(lazy = FALSE)
            utils::View(d, title = attr(d, "name"))
        },
        ## data check
        checkData = function(module) {
            "Checks that data is loaded (used before opening modules that require data)"
            data <- .self$getActiveData(lazy = TRUE)
            vars <- names(data)
            ret <- TRUE

            ## If dataset is empty (no data imported) display type 1 message,
            ## otherwise check whether imported data is appropriate for module
            ## (if wrong data type, display type 2 message)
            if (length(vars) == 1 && vars[1] == "empty") {
                ## check for empty data
                displayMsg(module, type = 1)
                ret <- FALSE
            }

            return(ret)
        },
        restoreDataset = function() {
            "Restores the original (first) dataset"
            setDocument(
                iNZDocument$new(
                    data = iNZDocuments[[1]]$getModel()$origDataSet,
                    preferences = .self$preferences
                )
            )
        },
        ## delete the current dataset
        deleteDataset = function(ask = TRUE) {
            "Deletes the current dataset"
            if (!ask) {
                if (activeDoc > 0) do_delete_dataset()
                return()
            }

            if (activeDoc == 0) {
                gmessage(
                    "Sorry, but you can't delete this dataset (it's the original, afterall!).",
                    title = "Unable to delete original data set",
                    icon = "warning",
                    parent = .self$win
                )
            } else {
                conf <- gconfirm(
                    paste0(
                        "This will remove the following dataset from iNZight,\n",
                        "and any unsaved changes to the data will be lost:\n\n",
                        attr(getActiveData(lazy = TRUE), "name", exact = TRUE),
                        "\n\n",
                        "Are you sure you want to continue?"
                    ),
                    title = "You sure you want to delete this data?",
                    icon = "question",
                    parent = .self$win
                )
                if (conf) do_delete_dataset()
            }
        },
        do_delete_dataset = function() {
            "Does the dataset deletion"
            todelete <- activeDoc
            rhistory$disabled <<- TRUE
            if (length(iNZDocuments) == 1L) {
                .self$setDocument(iNZDocument$new(preferences = .self$preferences), reset = TRUE)
                return()
            }

            iNZDocuments <<- iNZDocuments[-todelete]
            activeDoc <<- max(1L, activeDoc - 1L)
            dataNameWidget$updateWidget()
            rhistory$disabled <<- FALSE
            dataViewWidget$updateWidget()
            dataToolbarWidget$updateWidget()
            pset <- getActiveDoc()$getSettings()
            ctrlWidget$setState(pset)
        },
        removeDesign = function() {
            "Removes the survey design associated with a dataset"
            if (getActiveDoc()$getModel()$design_only) {
                conf <- gconfirm(
                    paste0(
                        "This design was loaded directly, so removing it will delete",
                        "the associated data. Are you sure you want to continue?"
                    ),
                    title = "Are you sure you want to delete this survey?",
                    icon = "question",
                    parent = .self$win
                )
                if (!conf) {
                    return()
                }
                # delete the current document
                do_delete_dataset()
            } else {
                getActiveDoc()$getModel()$setDesign(gui = .self)
            }

            updatePlot()
            dataNameWidget$updateWidget()
        },
        ## display warning message
        displayMsg = function(module, type) {
            "Displays a message about data requirements for the chosen module"
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
        load_addons = function() {
            mod_dirs <- list.dirs(addonModuleDir, recursive = FALSE)
            nmod <- length(mod_dirs)
            cli::cli_progress_bar("Loading modules", total = nmod)

            start <- proc.time()
            activeModules <<- vector("list", nmod)
            for (i in seq_len(nmod)) {
                activeModules[[i]] <<- load_module(mod_dirs[[i]], ui_env)
                cli::cli_progress_update()
            }
            cli::cli_progress_done()
            end <- proc.time()
            ptime <- end - start
            cli::cli_alert_info("Loaded {nmod} module{?s} ({round(ptime[3], 1)}s)")
        },
        ## create a gvbox object into the module window (ie, initialize it)
        ## NOTE: should be run every time when a new module is open
        initializeModuleWindow = function(mod, title, scroll = FALSE, border = 0,
                                          code = FALSE) {
            "Initializes a module window in the left-hand panel"
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
            moduleWindow$footer$set_borderwidth(4)

            if (!missing(title)) {
                title <- glabel(title)
                font(title) <- list(weight = "bold", size = 12)
                add(moduleWindow$header, title, anchor = c(0, 0))
            }

            if (border > 0) moduleWindow$body$set_borderwidth(border)

            visible(gp1) <<- FALSE

            if (code) .self$code_panel$show() else .self$code_panel$hide()

            if (!missing(mod)) {
                activeModule <<- mod
            }

            invisible(moduleWindow)
        },
        close_module = function() {
            "Closes the current module, and re-displays the default control panel"
            activeModule <<- NULL
            if (length(leftMain$children) <= 1) {
                return()
            }
            ## delete the module window
            delete(leftMain, leftMain$children[[2]])
            ## display the default view (data, variable, etc.)
            visible(gp1) <<- TRUE

            code_panel$show()
        },
        initializeCodeHistory = function() {
            "Initializes the R code history widget"
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
            "Initializes the plot history widget"
            if (is.null(plot_history)) {
                plot_history <<- iNZplothistory(.self)
            }
        },
        ## --- PREFERENCES SETTINGS and LOCATIONS etc ...
        defaultPrefs = function() {
            "Returns the default preferences"
            ## The default iNZight settings:
            list(
                check.updates = !getOption("inzight.lock.packages", FALSE),
                window.size = getOption("inzight.default.window.size", c(1250, 850)),
                popout = getOption("inzight.default.popout", FALSE),
                font.size = getOption("inzight.default.font.size", 10),
                dev.features = getOption("inzight.default.dev.features", FALSE),
                show.code = getOption("inzight.default.show.code", FALSE),
                language = getOption("inzight.default.langauge", "en"),
                module_dir = getOption("inzight.default.module_dir", NULL),
                multiple_x = getOption("inzight.default.multiple_x", FALSE),
                gg_theme = getOption("inzight.default.gg_theme", NULL)
            )
        },
        checkPrefs = function(prefs) {
            "Checks the provided preferences are valid, returning a valid list"
            allowed.names <- c(
                "check.updates",
                "window.size",
                "popout",
                "font.size",
                "dev.features",
                "show.code",
                "language",
                "module_dir",
                "multiple_x",
                "gg_theme"
            )

            ## Only keep allowed preferences --- anything else is discarded
            prefs <- prefs[names(prefs) %in% allowed.names]
            defs <- defaultPrefs()

            ## check.updates = TRUE | FALSE
            prefs$check.updates <-
                if (is.null(prefs$check.updates)) {
                    defs$check.updates
                } else if (!is.na(prefs$check.updates) & is.logical(prefs$check.updates)) {
                    prefs$check.updates
                } else {
                    defs$check.updates
                }

            ## window.size = c(WIDTH, HEIGHT)
            prefs$window.size <-
                if (is.null(prefs$window.size)) {
                    defs$window.size
                } else if (length(prefs$window.size) != 2) {
                    defs$window.size
                } else if (is_num(prefs$window.size)) {
                    prefs$window.size
                } else {
                    defs$window.size
                }

            ## pop-out layout = FALSE
            prefs$popout <-
                if (is.null(prefs$popout)) {
                    defs$popout
                } else if (is.logical(prefs$popout)) {
                    prefs$popout
                } else {
                    defs$popout
                }

            ## font size
            prefs$font.size <-
                if (is.null(prefs$font.size) || !is_num(prefs$font.size)) {
                    defs$font.size
                } else {
                    prefs$font.size
                }

            prefs$dev.features <-
                if (is.null(prefs$dev.features) || !is.logical(prefs$dev.features)) {
                    defs$dev.features
                } else {
                    prefs$dev.features
                }

            prefs$show.code <-
                if (is.null(prefs$show.code) || !is.logical(prefs$show.code)) {
                    defs$show.code
                } else {
                    prefs$show.code
                }

            prefs$language <-
                if (is.null(prefs$language) || !is.character(prefs$language)) {
                    defs$language
                } else {
                    prefs$language[1]
                }

            prefs <- modifyList(prefs,
                list(
                    module_dir =
                        if (is.null(prefs$module_dir) || prefs$module_dir == "" || !dir.exists(prefs$module_dir)) {
                            defs$module_dir
                        } else {
                            prefs$module_dir[1]
                        }
                ),
                keep.null = TRUE
            )

            prefs$multiple_x <-
                if (is.null(prefs$multiple_x) || !is.logical(prefs$multiple_x)) {
                    defs$multiple_x
                } else {
                    prefs$multiple_x
                }

            prefs <- modifyList(prefs,
                list(
                    gg_theme =
                        if (is.character(prefs$gg_theme) && prefs$gg_theme %in% AVAILABLE_THEMES) {
                            prefs$gg_theme
                        } else if (is.list(prefs$gg_theme) || inherits(prefs$gg_theme, "theme")) {
                            prefs$gg_theme
                        } else {
                            defs$gg_theme
                        }
                ),
                keep.null = TRUE
            )


            prefs
        },
        getPreferences = function() {
            "Gets the user's preferences from a file"
            prefs.location <<- file.path(
                tools::R_user_dir("iNZight", "config"),
                "preferences.R"
            )
            preferences <<- defaultPrefs()

            if (file.exists(prefs.location)) {
                prefs <- try(checkPrefs(dget(prefs.location)), silent = TRUE)

                if (!inherits(prefs, "try-error")) {
                    preferences <<- prefs
                }
            }
        },
        savePreferences = function() {
            "Saves the users preferences in a file"
            preferences <<- checkPrefs(preferences)
            if (!dir.exists(dirname(prefs.location))) {
                if (!interactive()) {
                    return()
                }
                # ask user to create config directory to save GUI preferences:
                make_dir <- gconfirm(
                    sprintf(
                        paste(
                            sep = "\n",
                            "iNZight needs to create the following directory/ies. Is that OK?",
                            "", "+ %s"
                        ),
                        dirname(prefs.location)
                    )
                )
                if (make_dir) {
                    dir.create(dirname(prefs.location), recursive = TRUE)
                }
            }

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
        plotMessage = function(heading, message, footer, type = "error") {
            "Displays a message to the user using the plot panel"
            curPlot <<- NULL
            plotType <<- "none"
            enabled(plotToolbar$exportplotBtn) <<- FALSE

            if (missing(heading) || missing(message)) {
                plotSplashScreen()
            }

            grid::grid.newpage()
            grid::pushViewport(
                grid::viewport(
                    height = unit(0.9, "npc"),
                    layout = grid::grid.layout(
                        nrow = 3,
                        ncol = 1,
                        heights = unit.c(
                            unit(0.1, "npc"),
                            unit(1, "null"),
                            unit(5, "lines")
                        )
                    )
                )
            )

            if (requireNamespace("png", quietly = TRUE)) {
                img <- png::readPNG(
                    system.file("images/inzight_transp.png", package = "iNZight")
                )
                grid::pushViewport(grid::viewport(layout.pos.row = 1))
                grid::grid.raster(img,
                    x = unit(0.1, "npc"),
                    y = unit(1, "npc"),
                    hjust = 0, vjust = 1
                )
                grid::upViewport()
            }

            grid::pushViewport(grid::viewport(layout.pos.row = 2))
            grid::pushViewport(
                grid::viewport(
                    y = unit(0.45, "npc"),
                    width = unit(0.8, "npc"),
                    height = unit(0.9, "npc")
                )
            )
            grid::grid.text(
                heading,
                y = 1, x = 0, just = c("left", "top"),
                gp = gpar(fontsize = 12, fontface = "bold")
            )

            switch(type,
                "error" = {
                    grid::grid.text(
                        paste0(
                            "The following error message was reported:"
                        ),
                        y = unit(1, "npc") - unit(3, "lines"),
                        x = 0, just = c("left", "top"),
                        gp = gpar(fontsize = 11)
                    )
                }
            )

            grid::grid.text(
                message,
                y = unit(1, "npc") - unit(6, "lines"),
                x = 0, just = c("left", "top"),
                gp = gpar(fontsize = 10, fontfamily = "monospace")
            )
            grid::upViewport()

            if (!missing(footer)) {
                grid::pushViewport(grid::viewport(layout.pos.row = 3))
                grid::pushViewport(
                    grid::viewport(
                        y = unit(0.45, "npc"),
                        width = unit(0.8, "npc"),
                        height = unit(0.9, "npc")
                    )
                )
                grid::grid.text(
                    footer,
                    y = 0, x = 0, just = c("left", "bottom"),
                    gp = gpar(fontsize = 10)
                )
                grid::upViewport()
            }

            grDevices::dev.flush()
        },
        plotSplashScreen = function() {
            "The default splash screen"
            if (!visible(win)) {
                return()
            }
            if (requireNamespace("png", quietly = TRUE)) {
                img <- png::readPNG(
                    system.file("images/inzight_transp.png", package = "iNZight")
                )
                grid::grid.newpage()
                grid::pushViewport(
                    grid::viewport(
                        height = unit(0.9, "npc"),
                        layout = grid::grid.layout(
                            nrow = 3,
                            ncol = 1,
                            heights = unit.c(
                                unit(0.15, "npc"),
                                unit(2.2, "lines"),
                                unit(1, "null")
                            )
                        )
                    )
                )

                grid::pushViewport(grid::viewport(layout.pos.row = 1))
                grid::grid.raster(img)
                grid::upViewport()

                grid::pushViewport(grid::viewport(layout.pos.row = 2))
                grid::grid.text(
                    sprintf("Version %s", packageVersion("iNZight")),
                    x = unit(0.8, "npc"),
                    y = unit(0.75, "npc"),
                    just = "right"
                )
                grid::grid.text(
                    sprintf(
                        "Release date: %s",
                        format(
                            as.Date(packageDescription("iNZight")$Date),
                            "%d %b %Y"
                        )
                    ),
                    x = unit(0.8, "npc"),
                    y = unit(0.25, "npc"),
                    just = "right",
                    gp = gpar(fontsize = 9)
                )
                grid::upViewport()

                grid::pushViewport(grid::viewport(layout.pos.row = 3))
                grid::pushViewport(
                    grid::viewport(
                        y = unit(0.45, "npc"),
                        width = unit(0.8, "npc"),
                        height = unit(0.9, "npc")
                    )
                )

                if (all(dim(getActiveData(lazy = TRUE)) == 1)) {
                    grid::grid.text(
                        "Kia ora and welcome! To get started, import some data.",
                        y = 1, x = 0, just = c("left", "top"),
                        gp = gpar(fontsize = 12, fontface = "bold")
                    )
                    grid::grid.text(
                        paste0(
                            "Not sure where to go? Try the File menu!\n",
                            "There are some example datasets there ",
                            "if you just want to explore the program."
                        ),
                        y = unit(1, "npc") - unit(3, "lines"),
                        x = 0,
                        just = c("left", "top"),
                        gp = gpar(fontsize = 9)
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
                        gp = gpar(fontsize = 9)
                    )
                } else {
                    grid::grid.text(
                        "That's some fine looking data ... ",
                        y = 1, x = 0, just = c("left", "top"),
                        gp = gpar(fontsize = 12, fontface = "bold")
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
            "Displays the code history"
            wh <- gwindow("R Code History",
                parent = .self$win,
                width = 800,
                height = 500
            )
            gh <- gvbox(container = wh)
            th <- gtext(container = gh, expand = TRUE, fill = TRUE, wrap = FALSE)
            insert(th, rhistory$get(),
                font.attr = list(family = "monospace")
            )
        },
        close = function() {
            "Closes the iNZight window, calling the user-supplied disposer function"
            dispose(win)
            disposer()
        },
        reload = function() {
            "Reloads iNZight"
            # first, get middle of iNZight window ..
            ipos <- RGtk2::gtkWindowGetPosition(.self$win$widget)

            rwin <- gwindow("Reloading iNZight ...",
                width = 300,
                height = 80,
                visible = FALSE
            )
            rg <- gvbox(container = rwin)
            addSpring(rg)
            rl <- glabel("Please wait while iNZight reloads ...",
                container = rg
            )
            font(rl) <- list(weight = "bold")
            addSpring(rg)

            s <- (size(.self$win) - size(rwin)) / 2
            gtkWindowMove(
                rwin$widget,
                ipos$root.x + s[1],
                ipos$root.y + s[2]
            )

            visible(rwin) <- TRUE
            on.exit(dispose(rwin))
            Sys.sleep(0.5)

            # save the document
            has_data <-
                nrow(.self$getActiveData(lazy = TRUE)) > 1L ||
                    ncol(.self$getActiveData(lazy = TRUE)) > 1L ||
                    names(.self$getActiveData(lazy = TRUE)) != "empty"
            state <- .self$getState()
            dispose(.self$win)
            if (popOut) try(grDevices::dev.off(), TRUE)
            .self$initializeGui(disposer = .self$disposer, show = FALSE)
            Sys.sleep(0.5)
            while (!is_initialized) {
                Sys.sleep(0.1)
            }
            if (has_data) {
                res <- .self$setState(state)
            }

            gtkWindowMove(.self$win$widget, ipos$root.x, ipos$root.y)
            .self$set_visible()
        },
        set_visible = function(visible = TRUE) {
            "Makes the iNZight window visible, and updates the plot"
            visible(win) <<- visible
            updatePlot()
        },
        show = function() {
            cat("An iNZGUI object\n")
        }
    )
)
