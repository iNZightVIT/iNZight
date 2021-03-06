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
                can_install = TRUE
            )

            if (Sys.getenv("LOCK_PACKAGES") != "")
                can_install <<- !as.logical(Sys.getenv("LOCK_PACKAGES"))

            ## this is trickier, because it depends on a bunch of things
            plotmenu <<- placeholder("Plot")
            menubar <<- gmenu(list(), container = container, expand = TRUE)

            hasModules()

            defaultMenu()
        },
        hasData = function() {
            !all(dim(GUI$getActiveData()) == 1)
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
                        handler = function(h, ...) iNZImportWin$new(GUI)),
                export =
                    gaction("Export data ...",
                        icon = "save-as",
                        handler = function(h, ...) iNZSaveWin$new(GUI, type = "data", data = GUI$getActiveData())),
                gseparator(),
                paste =
                    gaction("Paste from ...",
                        icon = "paste",
                        tooltip = "Import data by pasting/clipboard",
                        handler = function(h, ...)
                            iNZClipboard$new(GUI, type = "paste")
                    ),
                gseparator(),
                example =
                    gaction("Example data ...",
                        icon = "dataframe",
                        tooltip = "Load an example dataset",
                        handler = function(h, ...) iNZImportExampleWin$new(GUI)),
                gseparator(),
                preferences =
                    gaction ("Preferences ...",
                        icon = "preferences",
                        tooltip = "Customise iNZight",
                        handler = function(h, ...) iNZPrefsWin$new(GUI)),
                reload =
                    gaction("Reload iNZight",
                        icon = "refresh",
                        handler = function(h, ...) GUI$reload()
                    ),
                exit =
                    gaction("Exit",
                        icon = "quit",
                        handler = function(h, ...) GUI$close())
            )
            if (GUI$preferences$dev.features) {
                m <- c(
                    list(
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
                                if (length(f) == 0) return()
                                if (tools::file_ext(f) != "inzsave")
                                    f <- paste(f, "inzsave", sep =  ".")
                                GUI$saveState(f)
                                gmessage("Your session has been saved.",
                                    title = "Session saved",
                                    icon = "info"
                                )
                            }
                        ),
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
                        gseparator()
                    ),
                    m
                )
            }
            m
        },
        DataMenu = function() {
            if (!hasData()) return(placeholder("Dataset"))
            menu <- list(
                filter =
                    gaction("Filter ...",
                        icon = "subset",
                        handler = function(h, ...) iNZFilterWin$new(GUI)),
                sort =
                    gaction("Sort by variable(s) ...",
                        icon = "sort-ascending",
                        handler = function(h, ...) iNZSortbyDataWin$new(GUI)),
                aggregate =
                    gaction("Aggregate ...",
                        icon = "dnd-multiple",
                        handler = function(h, ...) iNZAggregateWin$new(GUI)),
                stack =
                    gaction("Stack ...",
                        icon = "dnd-multiple",
                        handler = function(h, ...) iNZstackVarWin$new(GUI)),
                "Dataset operation" = list(
                    reshape =
                        gaction("Reshape dataset ...",
                            icon = "dataframe",
                            tooltip = "Transform from wide- to long-form data",
                            handler = function(h, ...) iNZReshapeDataWin$new(GUI)),
                    separate =
                        gaction("Separate column ...",
                            icon = "dataframe",
                            tooltip = "Separate columns",
                            handler = function(h, ...) iNZSeparateDataWin$new(GUI)),
                    unite =
                        gaction("Unite columns ...",
                            icon = "dataframe",
                            tooltip = "Unite columns",
                            handler = function(h, ...) iNZUniteDataWin$new(GUI))
                ),
                report =
                    if (requireNamespace("dataMaid", quietly = TRUE) &&
                        requireNamespace("rmarkdown", quietly = TRUE) &&
                        rmarkdown::pandoc_available()) {
                        report =
                            gaction(
                                "Generate data report ...",
                                icon = "select-all",
                                handler = function(h, ...) iNZDataReportWin$new(GUI)
                            )
                    } else NULL,
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
                    gaction("Rename ...",
                        icon = "editor",
                        handler = function(h, ...) iNZrenameDataWin$new(GUI)),
                restore =
                    gaction("Restore original dataset",
                        icon = "revert-to-saved",
                        handler = function(h, ...) GUI$restoreDataset()),
                delete =
                    gaction("Delete current dataset",
                        icon = "delete",
                        handler = function(h, ...) GUI$deleteDataset()),
                "Merge/Join datasets" = list(
                    joinbycol =
                        gaction("Join by column values",
                            icon = "copy",
                            handler = function(h, ...) iNZjoinDataWin$new(GUI)),
                    appendrows =
                        gaction("Append new rows",
                            icon = "edit",
                            handler = function(h, ...) iNZappendrowWin$new(GUI))
                ),
                gseparator(),
                "Survey design" = list(
                    surveydesign =
                        gaction("Specify design ...",
                            icon = "new",
                            handler = function(h, ...)
                                iNZSurveyDesign$new(GUI, type = "survey")
                        ),
                    repdesign =
                        gaction("Specify replicate design ...",
                            icon = "new",
                            handler = function(h, ...)
                                iNZSurveyDesign$new(GUI, type = "replicate")
                        ),
                    poststrat =
                        gaction("Post stratify ...",
                            icon = "edit",
                            handler = function(h, ...) iNZSurveyPostStrat$new(GUI)
                        ),
                    removedesign =
                        gaction("Remove design",
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
                            handler = function(h, ...)
                                iNZSurveyDesign$new(GUI, type = "frequency")
                        ),
                    dropfrequency =
                        gaction("Remove frequency column",
                            icon = "delete",
                            handler = function(h, ...) {
                                GUI$getActiveDoc()$setSettings(list(freq = NULL))
                            }
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
            }
            menu
        },
        VariablesMenu = function() {
            if (!hasData()) return(placeholder("Variables"))
            menu <- list(
                cont2cat =
                    gaction("Convert to categorical ...",
                        icon = "convert",
                        tooltip = "Convert a variable to a categorical type",
                        handler = function(h, ...) iNZconToCatWin$new(GUI)),
                "Categorical Variables" = list(
                    reorder =
                        gaction("Reorder levels ...",
                            icon = "sort-ascending",
                            tooltip = "Reorder the levels of a categorical variable",
                            handler = function(h, ...) iNZreorderWin$new(GUI)),
                    collapse =
                        gaction("Collapse levels ...",
                            icon = "dnd-multiple",
                            tooltip = "Collapse two or more levels into one",
                            handler = function(h, ...) iNZcllpsWin$new(GUI)),
                    rename =
                        gaction("Rename levels ...",
                            icon = "edit",
                            tooltip = "Rename a categorical variable's levels",
                            handler = function(h, ...) iNZrenameWin$new(GUI)),
                    combine =
                        gaction("Combine categorical variables ...",
                            icon = "dnd-multiple",
                            tooltip = "Combine two or more categorical variables",
                            handler = function(h, ...) iNZcmbCatWin$new(GUI))
                ),
                "Numeric Variables" = list(
                    transform =
                        gaction("Transform ...",
                            icon = "convert",
                            tooltip = "Transform a variable using a function",
                            handler = function(h, ...) iNZtrnsWin$new(GUI)),
                    standardise =
                        gaction("Standardise ...",
                            icon = "convert",
                            tooltip = "Standardise a numeric variable",
                            handler = function(h, ...) iNZstdVarWin$new(GUI)),
                    class =
                        gaction("Form class intervals ...",
                            icon = "convert",
                            tooltip = "Convert numeric variable into categorical intervals",
                            handler = function(h, ...) iNZformClassIntervals$new(GUI)),
                    rank =
                        gaction("Rank numeric variables ...",
                            icon = "sort-ascending",
                            tooltip = "Create an ordering variable",
                            handler = function(h, ...) iNZrankNumWin$new(GUI)),
                    cat =
                        gaction("Convert to categorical (multiple) ...",
                            icon = "convert",
                            tooltip = "Convert multiple numeric variables to categorical",
                            handler = function(h, ...) iNZctocatmulWin$new(GUI))
                ),
                "Dates and Times" = list(
                  convert =
                    gaction("Convert to ...",
                            icon = "date",
                            tooltip = "Convert a variable to a dates and times type",
                            handler = function(h, ...) iNZconTodtWin$new(GUI)),
                  extract =
                    gaction("Extract from ...",
                            icon = "date",
                            tooltip = "Extract parts from a dates and times variable",
                            handler = function(h, ...) iNZExtfromdtWin$new(GUI)),
                  aggregation =
                    gaction("Aggregate to ...",
                            icon = "date",
                            tooltip = "Aggregate date-time into monthly or quarterly",
                            handler = function(h, ...) iNZAggregatedtWin$new(GUI))
                ),
                rename =
                    gaction("Rename variables ...",
                        icon = "edit",
                        tooltip = "Rename a variable",
                        handler = function(h, ...) iNZrnmVarWin$new(GUI)),
                create =
                    gaction("Create new variables ...",
                        icon = "new",
                        tooltip = "Create a new variable using a formula",
                        handler = function(h, ...) iNZcrteVarWin$new(GUI)),
                miss2cat =
                    gaction("Missing to categorical ...",
                        icon = "index",
                        tooltip = "Create a variable to include missingness information",
                        handler = function(h, ...) iNZmissCatWin$new(GUI)),
                delete =
                    gaction("Delete variables ...",
                        icon = "delete",
                        tooltip = "Permanently delete a variable",
                        handler = function(h, ...) iNZdeleteVarWin$new(GUI))
            )
            if (!is.null(GUI$getActiveDoc()$getModel()$getDesign())) {
                # disable some items for surveys
                # enabled(menu[["Numeric Variables"]]$class) <- FALSE
                menu[["Dates and Times"]] <- gaction("Dates and Times", enabled = FALSE)
                enabled(menu[["Dates and Times"]]) <- FALSE
            }
            menu
        },
        PlotMenu = function() {
            if (!hasData()) return(placeholder("Plot"))
            plotmenu
        },
        setPlotMenu = function(menu) {
            plotmenu <<- menu
            updateMenu("Plot", PlotMenu())
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
                            ),
                        manage =
                            gaction("Manage modules ...",
                                icon = "execute",
                                tooltip = "Add, update, and remove add-on modules.",
                                handler = function(h, ...)
                                    iNZightModules::ModuleManager$new(GUI)
                            )
                    )

                if (!can_install) adv <- adv["manage"]

                return(adv)
            }

            if (modules_installed) {
                adv <- list(
                    "Quick Explore" = list(
                        missing =
                            gaction("Missing values",
                                icon = "symbol_diamond",
                                tooltip = "Explore missing values",
                                handler = function(h, ...) iNZExploreMissing$new(GUI)),
                        all1varplot =
                            gaction("All 1-variable plots",
                                icon = "symbol_diamond",
                                tooltip = "Click through a plot of each variable",
                                handler = function(h, ...) iNZallPlots$new(GUI)),
                        all2varsmry =
                            gaction("All 1-variable summaries",
                                icon = "symbol_diamond",
                                tooltip = "Get a summary of all variables",
                                handler = function(h, ...) iNZallSummaries$new(GUI)),
                        all2var =
                            gaction("Explore 2-variable plots ...",
                                icon = "symbol_diamond",
                                tooltip = "Click through all 2-variable plots",
                                handler = function(h, ...) iNZall2Plots$new(GUI)),
                        pairs =
                            gaction("Pairs ...",
                                icon = "symbol_diamond",
                                tooltip = "See a pairs plot matrix",
                                handler = function(h, ...) iNZscatterMatrix$new(GUI))
                    ),
                    plot3d =
                        gaction("3D plot ...",
                            icon = "3dcontour",
                            tooltip = "Start the 3D plotting module",
                            handler = function(h, ...) {
                                ign <- gwindow("...", visible = FALSE)
                                tag(ign, "dataSet") <- GUI$getActiveData()
                                e <- list(obj = ign)
                                e$win <- GUI$win
                                iNZightModules::plot3D(e)
                            }
                        ),
                    timeseries =
                        gaction("Time series ...",
                            icon = "ts",
                            tooltip = "Start the time series module",
                            handler = function(h, ...) iNZightModules::iNZightTSMod$new(GUI)),
                    modelfitting =
                        gaction("Model fitting ...",
                            icon = "lines",
                            tooltip = "Start the model fitting module",
                            handler = function(h, ...) iNZightModules::iNZightRegMod$new(GUI)),
                    multires =
                        gaction("Multiple response ...",
                            icon = "hist",
                            tooltip = "Start the multiple response module",
                            handler = function(h, ...) iNZightModules::iNZightMultiRes$new(GUI)),
                    maps =
                        gaction("Maps ...",
                            icon = "plot1",
                            handler = function(h, ...) iNZightModules::iNZightMapLanding$new(GUI)),
                    gseparator(),
                    manage =
                        gaction("Manage modules ...",
                            icon = "execute",
                            tooltip = "Add, update, and remove add-on modules.",
                            handler = function(h, ...)
                                iNZightModules::ModuleManager$new(GUI))
                )
            } else if (can_install) {
                adv <- list(
                    install_modules =
                        gaction("Install the Modules package ...",
                            icon = "execute",
                            tooltip = "Install the iNZightModules R pacakge to access add-on modules",
                            handler = function(h, ...) {
                                c <- gconfirm("You are about to install the iNZightModules R package. Are you sure you want to continue?",
                                    parent = GUI$win)
                                if (!c) return()
                                e <- "utils::install.packages('iNZightModules',
                                    repos = c(
                                        'https://r.docker.stat.auckland.ac.nz',
                                        'https://cran.rstudio.com'
                                    )
                                )"

                                w <- gwindow("Installing packages",
                                    width = 300, height = 100,
                                    visible = FALSE,
                                    parent = GUI$win)
                                visible(w) <- FALSE
                                gg <- gvbox(container = w)
                                addSpace(gg, 10)
                                ggg <- ggroup(spacing = 15, container = gg)
                                addSpace(ggg, 0)
                                gimage(stock.id = "gtk-info", size="dialog", cont=ggg)
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
                                    parent = GUI$win)

                                hasModules()

                                .self$defaultMenu()
                            }
                        )
                )
            } else {
                adv <- list()
            }

            adv <- c(adv,
                list(
                    gseparator(),
                    rcode =
                        gaction("R code history [beta] ...",
                            icon = "rlogo",
                            tooltip = "Show the R code history for your session",
                            handler = function(h, ...) GUI$showHistory())
                )
            )
            if (modules_installed && !is.null(GUI$addonModuleDir)) {
                modules <- iNZightModules:::getModules(GUI$addonModuleDir)
                if (length(modules)) {
                    instindex <- which(names(adv) == "maps") + 1
                    mods <- lapply(modules,
                        function(mod) {
                            gaction(mod$display_name,
                                handler = function(h, ...) {
                                    x <- sprintf("mod$%s$new(GUI, name = '%s')",
                                        mod$name,
                                        mod$display_name
                                    )
                                    eval(parse(text = x))
                                }
                            )
                        }
                    )
                    adv <- c(adv[1:(instindex-1)], mods, adv[instindex:length(adv)])
                }
            }
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
                            handler = function(h, ...)
                                help_page(gsub(".", "/", n, fixed = TRUE))
                        )
                    }
                ),
                change =
                    gaction("Change history",
                        icon = "file",
                        tooltip = "",
                        handler = function(h, ...)
                            help_page('support/changelog/?pkg=iNZight')
                    ),
                faq =
                    gaction("FAQ",
                        icon = "find",
                        tooltip = "",
                        handler = function(h, ...)
                            help_page("support/faq/")),
                contact =
                    gaction("Contact us or Report a Bug",
                        icon = "help",
                        tooltip = "",
                        handler = function(h, ...)
                            help_page("support/contact/")
                    )
            )
        }
    )
)


iNZAboutWidget <- setRefClass(
    "iNZAboutWidget",
    fields = list(
        GUI = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            initFields(GUI = gui)

            window_width <- 500
            w <- gwindow("About iNZight",
                width = window_width,
                height = 600,
                visible = TRUE,
                parent = GUI$win
            )
            g <- gvbox(expand = FALSE, cont = w, spacing = 5)
            g$set_borderwidth(10)

            mainlbl <- glabel("iNZight", container = g)
            font(mainlbl) <- list(
                weight = "bold",
                family = "sans",
                size = 20
            )
            verlbl <- glabel(
                sprintf("Version %s - Released %s",
                    packageDescription("iNZight")$Version,
                    format(
                        as.POSIXct(
                            packageDescription("iNZight")$Date
                        ),
                        "%d %B, %Y"
                    )
                ),
                container = g
            )
            font(verlbl) <- list(
                weight = "normal",
                family = "sans",
                size = 10
            )
            rverlbl <- glabel(
                sprintf("Running on R version %s", getRversion()),
                container = g
            )
            font(rverlbl) <- list(
                weight = "normal",
                family = "sans",
                size = 10
            )

            addSpace(g, 10)

            ## sponsors
            sponsorlbl <- glabel("SPONSORS",
                container = g)
            font(sponsorlbl) <- list(
                weight = "bold",
                family = "sans",
                size = 10
            )

            g_sponsors <- gvbox(container = g)
            g_sponsors$set_borderwidth(5)

            logo_path <- function(logo)
                system.file(
                    file.path("sponsors", sprintf("%s_logo50.png", logo)),
                    package = "iNZight"
                )
            sponsors <- list(
                uoa = "https://auckland.ac.nz",
                statsnz = "https://stats.govt.nz",
                minedu = "https://minedu.govt.nz",
                abs = "https://abs.gov.au",
                terourou = "https://terourou.org",
                inzan = NULL
            )

            max_width <- window_width - 20
            row_width <- 0
            g_row <- ggroup(container = g_sponsors)
            addSpring(g_row)

            for (sponsor in names(sponsors)) {
                url <- sponsors[[sponsor]]
                logo <- logo_path(sponsor)
                if (!file.exists(logo)) next

                img <- png::readPNG(logo)

                if (row_width > 0 && row_width + dim(img)[2] > max_width) {
                    # create a new row
                    addSpring(g_row)
                    addSpace(g, 5)
                    g_row <- ggroup(container = g_sponsors)
                    addSpring(g_row)
                    row_width <- 0
                }

                # add image
                gimagebutton(logo,
                    container = g_row,
                    handler = function(h, ...)
                        if (!is.null(url)) browseURL(url)
                )
                addSpring(g_row)

                # add space
                row_width <- row_width + dim(img)[2] + 10
            }

            addSpace(g, 10)

            licenselbl <- glabel("LICENSE",
                container = g)
            font(licenselbl) <- list(
                weight = "bold",
                family = "sans",
                size = 10
            )
            addSpace(g, 5)

            gpltxt <- gtext(expand = TRUE, cont = g, wrap = TRUE)
            l1 <- insert(gpltxt,
                paste(
                    "\nThis program is free software; you can redistribute it and/or",
                    "modify it under the terms of the GNU General Public License",
                    "as published by the Free Software Foundation; either version 2",
                    "of the License, or (at your option) any later version.\n"
                ),
                font.attr = list(size = 9)
            )
            l2 <- insert(gpltxt,
                paste(
                    "This program is distributed in the hope that it will be useful,",
                    "but WITHOUT ANY WARRANTY; without even the implied warranty of",
                    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
                    "GNU General Public License for more details.\n"
                ),
                font.attr = list(size = 9)
            )
            l3 <- insert(gpltxt,
                paste(
                    "You should have received a copy of the GNU General Public License",
                    "along with this program; if not, write to the Free Software",
                    "Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.\n"
                ),
                font.attr = list(size = 9)
            )
            l4 <- insert(gpltxt,
                paste(
                    "You can view the full licence here:\nhttp://www.gnu.org/licenses/gpl-2.0-standalone.html"
                ),
                font.attr = list(size = 9)
            )
            addSpace(g, 5)
            contactlbl <- glabel(
                "For help, contact inzight_support@stat.auckland.ac.nz",
                container = g
            )
            font(contactlbl) <- list(
                weight = "normal",
                family = "sans",
                size = 8
            )
            visible(w) <- TRUE
        }
    )
)

InstallMaps <- function(gui) {
    check.maps <- 'requireNamespace("iNZightMaps", quietly = TRUE)'
    if (eval(parse(text = check.maps))) {
        gmessage("The maps package is already installed!",
            parent = gui$win)
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
        parent = gui$win)
}
