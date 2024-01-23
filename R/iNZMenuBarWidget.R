iNZMenuBarWidget <- setRefClass(
    "iNZMenuBarWidget",
    fields = list(
        GUI = "ANY", container = "ANY",
        menubar = "ANY",
        plotmenu = "ANY",
        modules_installed = "logical",
        can_install = "logical"
    ),
    methods = list(
        initialize = function(gui, container) {
            initFields(
                GUI = gui,
                container = container,
                can_install = !getOption("inzight.lock.packages", FALSE)
            )

            ## this is trickier, because it depends on a bunch of things
            plotmenu <<- placeholder("Plot")
            menubar <<- gmenu(list(), container = container, expand = TRUE)

            hasModules()

            defaultMenu()
        },
        hasData = function() {
            !all(dim(GUI$getActiveData(lazy = TRUE)) == 1)
        },
        hasModules = function() {
            modules_installed <<- suppressMessages(
                requireNamespace("iNZightModules", quietly = TRUE)
            )
        },
        placeholder = function(name) {
            x <- gaction(name)
            enabled(x) <- FALSE
            x
        },
        defaultMenu = function() {
            setMenu(
                File = FileMenu(),
                Dataset = DataMenu(),
                Variables = VariablesMenu(),
                Plot = PlotMenu(),
                Modules = ModuleMenu(),
                Advanced = AdvancedMenu(),
                Help = HelpMenu()
            )
        },
        setMenu = function(...) {
            svalue(menubar) <<- list(...)
        },
        updateMenu = function(what, with) {
            svalue(menubar)[[what]] <<- with
        },
        FileMenu = function() {
            m <- list(
                import =
                    gaction("Import data ...",
                        icon = "cdrom",
                        tooltip = "Import a new dataset",
                        handler = function(h, ...) iNZImportWin$new(GUI)
                    ),
                export =
                    gaction("Export data ...",
                        icon = "save-as",
                        handler = function(h, ...) iNZExportWin$new(GUI)
                    ),
                gseparator(),
                Clipboard = list(
                    paste =
                        gaction("Paste from ...",
                            icon = "paste",
                            tooltip = "Import data by pasting/clipboard",
                            handler = function(h, ...) {
                                iNZClipboard$new(GUI, type = "paste")
                            }
                        ),
                    copy =
                        gaction("Copy to ...",
                            icon = "copy",
                            tooltip = "Copy data to clipboard",
                            handler = function(h, ...) {
                                iNZCopyToClipboard$new(GUI)
                            }
                        )
                ),
                gseparator(),
                example =
                    gaction("Example data ...",
                        icon = "dataframe",
                        tooltip = "Load an example dataset",
                        handler = function(h, ...) iNZImportExampleWin$new(GUI)
                    ),
                gseparator(),
                preferences =
                    gaction("Preferences ...",
                        icon = "preferences",
                        tooltip = "Customise iNZight",
                        handler = function(h, ...) iNZPrefsWin$new(GUI)
                    ),
                reload =
                    gaction("Reload iNZight",
                        icon = "refresh",
                        handler = function(h, ...) GUI$reload()
                    ),
                exit =
                    gaction("Exit",
                        icon = "quit",
                        handler = function(h, ...) GUI$close()
                    )
            )
            if (GUI$preferences$dev.features) {
                m <- c(
                    list(
                        load = gaction("Load [beta]",
                            icon = "open",
                            tooltip = "Load a saved iNZight session",
                            handler = function(h, ...) {
                                f <- gfile(
                                    text = "Load [beta]",
                                    type = "open",
                                    filter = list(
                                        "iNZight save files (*.inzsave)" = list(patterns = c("*.inzsave")),
                                        "All files" = list(patterns = "*")
                                    )
                                )
                                GUI$loadState(f)
                                invisible(NULL)
                            }
                        ),
                        save = gaction("Save [beta]",
                            icon = "save",
                            tooltip = "Save the current iNZight session",
                            handler = function(h, ...) {
                                f <- gfile(
                                    text = "Save [beta]",
                                    type = "save",
                                    initial.filename = "untitled.inzsave",
                                    filter = list(
                                        "iNZight save files (*.inzsave)" = list(patterns = c(".inzsave")),
                                        "All files" = list(patterns = "*")
                                    )
                                )
                                if (length(f) == 0) {
                                    return()
                                }
                                if (tools::file_ext(f) != "inzsave") {
                                    f <- paste(f, "inzsave", sep = ".")
                                }
                                GUI$saveState(f)
                                gmessage("Your session has been saved.",
                                    title = "Session saved",
                                    icon = "info"
                                )
                            }
                        ),
                        gseparator()
                    ),
                    m
                )
            }
            if (!hasData()) {
                enabled(m$export) <- FALSE
                if (!is.null(m$save)) enabled(m$save) <- FALSE
            }

            mods <- mod_menu_items("File")
            if (!is.null(mods)) {
                menu <- modifyList(menu, mods, keep.null = TRUE)
            }

            m
        },
        DataMenu = function() {
            if (!hasData()) {
                return(placeholder("Dataset"))
            }
            menu <- list(
                filter =
                    gaction("Filter ...",
                        icon = "subset",
                        handler = function(h, ...) iNZFilterWin$new(GUI)
                    ),
                sort =
                    gaction("Sort by variable(s) ...",
                        icon = "sort-ascending",
                        handler = function(h, ...) iNZSortWin$new(GUI)
                    ),
                aggregate =
                    gaction("Aggregate ...",
                        icon = "dnd-multiple",
                        handler = function(h, ...) iNZAggregateWin$new(GUI)
                    ),
                stack =
                    gaction("Stack ...",
                        icon = "dnd-multiple",
                        handler = function(h, ...) iNZStackWin$new(GUI)
                    ),
                "Dataset operation" = list(
                    reshape =
                        gaction("Reshape dataset ...",
                            icon = "dataframe",
                            tooltip = "Transform from wide- to long-form data",
                            handler = function(h, ...) iNZReshapeWin$new(GUI)
                        ),
                    separate =
                        gaction("Separate column ...",
                            icon = "dataframe",
                            tooltip = "Separate columns",
                            handler = function(h, ...) iNZSeparateWin$new(GUI)
                        ),
                    unite =
                        gaction("Unite columns ...",
                            icon = "dataframe",
                            tooltip = "Unite columns",
                            handler = function(h, ...) iNZUniteWin$new(GUI)
                        )
                ),
                report =
                    if (requireNamespace("dataMaid", quietly = TRUE) &&
                        requireNamespace("rmarkdown", quietly = TRUE) &&
                        rmarkdown::pandoc_available()) {
                        report <-
                            gaction(
                                "Generate data report ...",
                                icon = "select-all",
                                handler = function(h, ...) iNZDataReportWin$new(GUI)
                            )
                    } else {
                        NULL
                    },
                validate =
                    gaction("Validate ...",
                        icon = "apply",
                        handler = function(h, ...) iNZValidateWin$new(GUI)
                    ),
                reorder =
                    gaction("Reorder and select variables ...",
                        icon = "sort-ascending",
                        handler = function(h, ...) iNZReorderVarsWin$new(GUI)
                    ),
                gseparator(),
                view =
                    gaction("View full dataset",
                        icon = "datasheet",
                        handler = function(h, ...) GUI$view_dataset()
                    ),
                rename =
                    gaction("Rename dataset ...",
                        icon = "editor",
                        handler = function(h, ...) iNZRenameDataWin$new(GUI)
                    ),
                restore =
                    gaction("Restore original dataset",
                        icon = "revert-to-saved",
                        handler = function(h, ...) GUI$restoreDataset()
                    ),
                delete =
                    gaction("Delete current dataset",
                        icon = "delete",
                        handler = function(h, ...) GUI$deleteDataset()
                    ),
                "Merge/Join datasets" = list(
                    joinbycol =
                        gaction("Join by column values",
                            icon = "copy",
                            handler = function(h, ...) iNZJoinWin$new(GUI)
                        ),
                    appendrows =
                        gaction("Append new rows",
                            icon = "edit",
                            handler = function(h, ...) iNZAppendRowsWin$new(GUI)
                        )
                ),
                gseparator(),
                "Survey design" = list(
                    surveydesign =
                        gaction("Specify design ...",
                            tooltip = "Specify survey design information for the data",
                            icon = "new",
                            handler = function(h, ...) {
                                iNZSurveyDesign$new(GUI, type = "survey")
                            }
                        ),
                    repdesign =
                        gaction("Specify replicate design ...",
                            icon = "new",
                            handler = function(h, ...) {
                                iNZSurveyDesign$new(GUI, type = "replicate")
                            }
                        ),
                    poststrat =
                        gaction("Post stratify ...",
                            icon = "edit",
                            handler = function(h, ...) iNZSurveyPostStrat$new(GUI)
                        ),
                    removedesign =
                        gaction("Remove design",
                            tooltip = "Remove survey design from data",
                            icon = "delete",
                            handler = function(h, ...) GUI$removeDesign()
                        )
                ),
                "Frequency tables" = list(
                    expandtable =
                        gaction("Expand table",
                            icon = "datasheet",
                            handler = function(h, ...) iNZexpandTblWin$new(GUI)
                        ),
                    setfrequency =
                        gaction("Specify frequency column",
                            icon = "datasheet",
                            handler = function(h, ...) {
                                iNZSurveyDesign$new(GUI, type = "frequency")
                            }
                        ),
                    dropfrequency =
                        gaction("Remove frequency column",
                            icon = "delete",
                            handler = function(h, ...) {
                                GUI$getActiveDoc()$setSettings(list(freq = NULL))
                            }
                        )
                ),
                gseparator(),
                "Data Dictionary" = list(
                    load_dd =
                        gaction("Load ...",
                            icon = "datasheet",
                            handler = function(h, ...) iNZDataDict$new(GUI)
                        ),
                    view_dd =
                        gaction("View",
                            icont = "datasheet",
                            handler = function(h, ...) iNZDDView$new(GUI)
                        )
                )
            )
            if (is.null(menu$report)) menu$report <- NULL
            if (!is.null(GUI$getActiveDoc()$getModel()$getDesign())) {
                # disable some items for surveys
                enabled(menu$stack) <- FALSE
                enabled(menu[["Dataset operation"]]$reshape) <- FALSE
                enabled(menu[["Merge/Join datasets"]]$appendrows) <- FALSE

                menu[["Frequency tables"]] <- gaction("Frequency tables", enabled = FALSE)
                enabled(menu[["Frequency tables"]]) <- FALSE

                survey_type <- GUI$getActiveDoc()$getModel()$getDesign()$spec$survey_type
                if (survey_type == "survey") {
                    svalue(menu[["Survey design"]]$surveydesign) <- "Modify design ..."
                    menu[["Survey design"]]$repdesign <- NULL
                }
                if (survey_type == "replicate") {
                    svalue(menu[["Survey design"]]$repdesign) <- "Modify replicate design ..."
                    menu[["Survey design"]]$surveydesign <- NULL
                }
            } else {
                # disable some items for non-surveys
                menu[["Survey design"]]$poststrat <- NULL
                menu[["Survey design"]]$removedesign <- NULL
            }

            mods <- mod_menu_items("Dataset")
            if (!is.null(mods)) {
                menu <- modifyList(menu, mods, keep.null = TRUE)
            }

            menu
        },
        VariablesMenu = function() {
            if (!hasData()) {
                return(placeholder("Variables"))
            }
            menu <- list(
                cont2cat =
                    gaction("Convert to categorical ...",
                        icon = "convert",
                        tooltip = "Convert a variable to a categorical type",
                        handler = function(h, ...) iNZConToCatWin$new(GUI)
                    ),
                "Categorical Variables" = list(
                    reorder =
                        gaction("Reorder levels ...",
                            icon = "sort-ascending",
                            tooltip = "Reorder the levels of a categorical variable",
                            handler = function(h, ...) iNZReorderLevelsWin$new(GUI)
                        ),
                    collapse =
                        gaction("Collapse levels ...",
                            icon = "dnd-multiple",
                            tooltip = "Collapse two or more levels into one",
                            handler = function(h, ...) iNZCollapseWin$new(GUI)
                        ),
                    rename =
                        gaction("Rename levels ...",
                            icon = "edit",
                            tooltip = "Rename a categorical variable's levels",
                            handler = function(h, ...) iNZRenameFactorLevelsWin$new(GUI)
                        ),
                    combine =
                        gaction("Combine categorical variables ...",
                            icon = "dnd-multiple",
                            tooltip = "Combine two or more categorical variables",
                            handler = function(h, ...) iNZUniteWin$new(GUI, cat_only = TRUE)
                        )
                ),
                "Numeric Variables" = list(
                    transform =
                        gaction("Transform ...",
                            icon = "convert",
                            tooltip = "Transform a variable using a function",
                            handler = function(h, ...) iNZTransformWin$new(GUI)
                        ),
                    standardise =
                        gaction("Standardise ...",
                            icon = "convert",
                            tooltip = "Standardise a numeric variable",
                            handler = function(h, ...) iNZStandardiseWin$new(GUI)
                        ),
                    class =
                        gaction("Form class intervals ...",
                            icon = "convert",
                            tooltip = "Convert numeric variable into categorical intervals",
                            handler = function(h, ...) iNZFormClassIntervalsWin$new(GUI)
                        ),
                    rank =
                        gaction("Rank numeric variables ...",
                            icon = "sort-ascending",
                            tooltip = "Create an ordering variable",
                            handler = function(h, ...) iNZRankWin$new(GUI)
                        ),
                    cat =
                        gaction("Convert to categorical (multiple) ...",
                            icon = "convert",
                            tooltip = "Convert multiple numeric variables to categorical",
                            handler = function(h, ...) iNZConToCatMultiWin$new(GUI)
                        )
                ),
                "Dates and Times" = list(
                    convert =
                        gaction("Convert to ...",
                            icon = "date",
                            tooltip = "Convert a variable to a dates and times type",
                            handler = function(h, ...) iNZConToDtWin$new(GUI)
                        ),
                    extract =
                        gaction("Extract from ...",
                            icon = "date",
                            tooltip = "Extract parts from a dates and times variable",
                            handler = function(h, ...) iNZExtFromDtWin$new(GUI)
                        ),
                    aggregation =
                        gaction("Aggregate to ...",
                            icon = "date",
                            tooltip = "Aggregate date-time into monthly or quarterly",
                            handler = function(h, ...) iNZAggDtWin$new(GUI)
                        )
                ),
                rename =
                    gaction("Rename variables ...",
                        icon = "edit",
                        tooltip = "Rename a variable",
                        handler = function(h, ...) iNZRenameVarWin$new(GUI)
                    ),
                create =
                    gaction("Create new variable ...",
                        icon = "new",
                        tooltip = "Create a new variable using a formula",
                        handler = function(h, ...) iNZCreateVarWin$new(GUI)
                    ),
                miss2cat =
                    gaction("Missing to categorical ...",
                        icon = "index",
                        tooltip = "Create a variable to include missingness information",
                        handler = function(h, ...) iNZMissToCatWin$new(GUI)
                    ),
                delete =
                    gaction("Delete variables ...",
                        icon = "delete",
                        tooltip = "Permanently delete a variable",
                        handler = function(h, ...) iNZDeleteVarWin$new(GUI)
                    )
            )
            if (!is.null(GUI$getActiveDoc()$getModel()$getDesign())) {
                # disable some items for surveys
                # enabled(menu[["Numeric Variables"]]$class) <- FALSE
                menu[["Dates and Times"]] <- gaction("Dates and Times", enabled = FALSE)
                enabled(menu[["Dates and Times"]]) <- FALSE
            }

            mods <- mod_menu_items("Variables")
            if (!is.null(mods)) {
                menu <- modifyList(menu, mods, keep.null = TRUE)
            }

            menu
        },
        PlotMenu = function() {
            if (!hasData()) {
                return(placeholder("Plot"))
            }
            mods <- mod_menu_items("Plot")
            if (!is.null(mods)) {
                modifyList(plotmenu, mods, keep.null = TRUE)
            } else {
                plotmenu
            }
        },
        setPlotMenu = function(menu) {
            plotmenu <<- menu
            updateMenu("Plot", PlotMenu())
        },
        ModuleMenu = function() {
            mods <- lapply(
                GUI$activeModules,
                function(m) {
                    if (!is.null(m$menu$Modules)) {
                        # transform menu item into menu actions
                        convert_menu_items(m$menu$Modules, GUI, m)
                    } else {
                        list(
                            gaction(m$info$title,
                                handler = function(h, ...) {
                                    run_module(GUI, m)
                                }
                            )
                        )
                    }
                }
            )
            mods <- c(
                mods,
                list(
                    gseparator(),
                    gaction("Manage ...", handler = function(h, ...) NewModuleManager$new(GUI)),
                    gaction("Reload",
                        handler = function(h, ...) {
                            GUI$load_addons()
                            defaultMenu()
                        }
                    )
                )
            )
            do.call(c, mods)
        },
        AdvancedMenu = function() {
            if (!hasData() && modules_installed) {
                ## just provide the ability to install modules
                adv <-
                    list(
                        installmaps =
                            gaction("Install Maps",
                                icon = "symbol_diamond",
                                "tooltip" = "Install the Maps module",
                                handler = function(h, ...) InstallMaps(GUI)
                            )
                    )

                return(adv)
            }

            if (modules_installed) {
                adv <- list(
                    "Quick Explore" = list(
                        missing =
                            gaction("Missing values",
                                icon = "symbol_diamond",
                                tooltip = "Explore missing values",
                                handler = function(h, ...) iNZExploreMissing$new(GUI)
                            ),
                        all1varplot =
                            gaction("All 1-variable plots",
                                icon = "symbol_diamond",
                                tooltip = "Click through a plot of each variable",
                                handler = function(h, ...) iNZallPlots$new(GUI)
                            ),
                        all2varsmry =
                            gaction("All 1-variable summaries",
                                icon = "symbol_diamond",
                                tooltip = "Get a summary of all variables",
                                handler = function(h, ...) iNZallSummaries$new(GUI)
                            ),
                        all2var =
                            gaction("Explore 2-variable plots ...",
                                icon = "symbol_diamond",
                                tooltip = "Click through all 2-variable plots",
                                handler = function(h, ...) iNZall2Plots$new(GUI)
                            ),
                        pairs =
                            gaction("Pairs ...",
                                icon = "symbol_diamond",
                                tooltip = "See a pairs plot matrix",
                                handler = function(h, ...) iNZscatterMatrix$new(GUI)
                            )
                    ),
                    plot3d =
                        gaction("3D plot ...",
                            icon = "3dcontour",
                            tooltip = "Start the 3D plotting module",
                            handler = function(h, ...) {
                                ign <- gwindow("...", visible = FALSE)
                                tag(ign, "dataSet") <- GUI$getActiveData(lazy = FALSE)
                                e <- list(obj = ign)
                                e$win <- GUI$win
                                iNZightModules::plot3D(e)
                            }
                        ),
                    timeseries =
                        gaction("Time series ...",
                            icon = "ts",
                            tooltip = "Start the time series module",
                            handler = function(h, ...) {
                                res <- gconfirm(
                                    "This module is being deprecated. You can install the new version from the 'Modules' menu.\n\nWe will continue to support this module for the time being, but it will be removed in a future release.\n\nClick 'OK' to continue to the old Time Series module.",
                                    title = "Deprecation Warning",
                                    icon = "warning",
                                    parent = GUI$win
                                )
                                if (!res) {
                                    return()
                                }
                                iNZightModules::iNZightTSMod$new(GUI)
                            }
                        ),
                    modelfitting =
                        gaction("Model fitting ...",
                            icon = "lines",
                            tooltip = "Start the model fitting module",
                            handler = function(h, ...) iNZightModules::iNZightRegMod$new(GUI)
                        ),
                    multires =
                        gaction("Multiple response ...",
                            icon = "hist",
                            tooltip = "Start the multiple response module",
                            handler = function(h, ...) iNZightModules::iNZightMultiRes$new(GUI)
                        ),
                    maps =
                        gaction("Maps ...",
                            icon = "plot1",
                            handler = function(h, ...) iNZightModules::iNZightMapLanding$new(GUI)
                        ),
                    gseparator(),
                    manage =
                        gaction("Manage modules ...",
                            icon = "execute",
                            tooltip = "Add, update, and remove add-on modules.",
                            handler = function(h, ...) {
                                iNZightModules::ModuleManager$new(GUI)
                            }
                        )
                )
            } else if (can_install) {
                adv <- list(
                    install_modules =
                        gaction("Install the Modules package ...",
                            icon = "execute",
                            tooltip = "Install the iNZightModules R package to access add-on modules",
                            handler = function(h, ...) {
                                c <- gconfirm("You are about to install the iNZightModules R package. Are you sure you want to continue?",
                                    parent = GUI$win
                                )
                                if (!c) {
                                    return()
                                }
                                e <- "utils::install.packages('iNZightModules',
                                    repos = c(
                                        'https://r.docker.stat.auckland.ac.nz',
                                        'https://cran.rstudio.com'
                                    )
                                )"

                                w <- gwindow("Installing packages",
                                    width = 300, height = 100,
                                    visible = FALSE,
                                    parent = GUI$win
                                )
                                visible(w) <- FALSE
                                gg <- gvbox(container = w)
                                addSpace(gg, 10)
                                ggg <- ggroup(spacing = 15, container = gg)
                                addSpace(ggg, 0)
                                gimage(stock.id = "gtk-info", size = "dialog", cont = ggg)
                                glabel("Please wait while the package and its dependencies are installed...",
                                    container = ggg,
                                    anchor = c(-1, 1)
                                )
                                addSpace(ggg, 10)
                                addSpace(gg, 10)
                                visible(w) <- TRUE
                                Sys.sleep(0.1)

                                eval(parse(text = e))

                                Sys.sleep(0.1)
                                dispose(w)

                                gmessage("Install complete.",
                                    title = "Installing packages complete",
                                    parent = GUI$win
                                )

                                hasModules()

                                .self$defaultMenu()
                            }
                        )
                )
            } else {
                adv <- list()
            }

            adv <- c(
                adv,
                list(
                    gseparator(),
                    rcode =
                        gaction("R code history [beta] ...",
                            icon = "rlogo",
                            tooltip = "Show the R code history for your session",
                            handler = function(h, ...) GUI$showHistory()
                        )
                )
            )
            adv
        },
        HelpMenu = function() {
            guides <- list(
                user_guides.basics = "The Basics",
                user_guides.interface = "The Interface",
                user_guides.plot_options = "Plot Options",
                user_guides.variables = "Variables menu",
                user_guides.data_options = "Dataset menu",
                user_guides.add_ons = "Advanced"
            )
            list(
                about =
                    gaction("About",
                        icon = "about",
                        tooltip = "",
                        handler = function(h, ...) iNZAboutWidget$new(GUI)
                    ),
                "User Guides" = lapply(
                    names(guides),
                    function(n) {
                        gaction(
                            guides[[n]],
                            icon = "help_topic",
                            tooltip = "",
                            handler = function(h, ...) {
                                help_page(gsub(".", "/", n, fixed = TRUE))
                            }
                        )
                    }
                ),
                transition =
                    gaction("Version 4.2 Transition Guide",
                        icon = "file",
                        tooltip = "",
                        handler = function(h, ...) {
                            help_page("docs/transition-to-4.2/")
                        }
                    ),
                change =
                    gaction("Change history",
                        icon = "file",
                        tooltip = "",
                        handler = function(h, ...) {
                            help_page("support/changelog/?pkg=iNZight")
                        }
                    ),
                faq =
                    gaction("FAQ",
                        icon = "find",
                        tooltip = "",
                        handler = function(h, ...) {
                            help_page("support/faq/")
                        }
                    ),
                contact =
                    gaction("Contact us or Report a Bug",
                        icon = "help",
                        tooltip = "",
                        handler = function(h, ...) {
                            help_page("support/contact/")
                        }
                    )
            )
        },
        mod_menu_items = function(x) {
            mods <- lapply(
                GUI$activeModules,
                function(m) {
                    if (is.null(m$menu)) {
                        return(NULL)
                    }
                    # transform menu item into menu actions
                    convert_menu_items(m$menu[[x]], GUI, m)
                }
            )
            if (length(mods)) do.call(c, mods) else NULL
        }
    )
)

InstallMaps <- function(gui) {
    check.maps <- 'requireNamespace("iNZightMaps", quietly = TRUE)'
    if (eval(parse(text = check.maps))) {
        gmessage("The maps package is already installed!",
            parent = gui$win
        )
        return()
    }

    svalue(gui$statusbar) <- "Installing maps module ..."
    utils::install.packages(
        "iNZightMaps",
        repos = c(
            "https://r.docker.stat.auckland.ac.nz",
            "https://cran.stat.auckland.ac.nz"
        ),
        dependencies = TRUE
    )

    if (!eval(parse(text = check.maps))) {
        svalue(gui$statusbar) <- "Error installing the maps module"
        gmessage(
            "Unable to install package. Please check the website.",
            parent = gui$win
        )
        return()
    }

    ## reload the menu ...?
    svalue(gui$statusbar) <- "Maps module installed successfully"
    gui$menuBarWidget$defaultMenu()
    gmessage("The Maps package has been installed.",
        parent = gui$win
    )
}
