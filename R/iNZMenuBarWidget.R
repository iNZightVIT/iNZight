iNZMenuBarWidget <- setRefClass(
    "iNZMenuBarWidget",
    fields = list(
        GUI = "ANY", container = "ANY", disposeR = "logical",
        menubar = "ANY",
        plotmenu = "ANY",
        modules_installed = "logical"
    ),
    methods = list(
        initialize = function(gui, container, disposeR) {
            initFields(
                GUI = gui,
                container = container,
                disposeR = disposeR
            )

            ## this is trickier, because it depends on a bunch of things
            plotmenu <<- placeholder("menu_plot")
            menubar <<- gmenu(list(), container = container)

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
            x <- gaction(tr(name))
            enabled(x) <- FALSE
            x
        },
        defaultMenu = function() {
            m <- list(
                menu_file = FileMenu(),
                menu_data = DataMenu(),
                menu_vars = VariablesMenu(),
                menu_plot = PlotMenu(),
                menu_adv = AdvancedMenu(),
                menu_help = HelpMenu()
            )
            names(m) <- tr(names(m))
            z <- do.call(.self$setMenu, m)
            invisible(NULL)
        },
        setMenu = function(...) {
            svalue(menubar) <<- list(...)
        },
        updateMenu = function(what, with) {
            what <- tr(what)
            svalue(menubar)[[what]] <<- with
        },
        FileMenu = function() {
            m <- list(
                import =
                    gaction(paste(tr("menu_file_import"), "..."),
                        icon = "cdrom",
                        tooltip = "Import a new dataset",
                        handler = function(h, ...) iNZImportWin$new(GUI)),
                export =
                    gaction(paste(tr("menu_file_export"), "..."),
                        icon = "save-as",
                        handler = function(h, ...) iNZSaveWin$new(GUI, type = "data", data = GUI$getActiveData())),
                gseparator(),
                paste =
                    gaction(paste(tr("menu_file_paste"), "..."),
                        icon = "paste",
                        tooltip = "Import data by pasting/clipboard",
                        handler = function(h, ...)
                            iNZClipboard$new(GUI, type = "paste")
                    ),
                gseparator(),
                example =
                    gaction(paste(tr("menu_file_examples"), "..."),
                        icon = "dataframe",
                        tooltip = "Load an example dataset",
                        handler = function(h, ...) iNZImportExampleWin$new(GUI)),
                gseparator(),
                preferences =
                    gaction (paste(tr("menu_file_prefs"), "..."),
                        icon = "preferences",
                        tooltip = "Customise iNZight",
                        handler = function(h, ...) iNZPrefsWin$new(GUI)),
                reload =
                    gaction(tr("menu_file_reload"),
                        icon = "refresh",
                        handler = function(h, ...) GUI$reload()
                    ),
                exit =
                    gaction(tr("menu_file_exit"),
                        icon = "quit",
                        handler = function(h, ...) GUI$close())
            )
            if (GUI$preferences$dev.features) {
                m <- c(
                    list(
                        save = gaction(paste(tr("menu_file_save"), "..."),
                            icon = "save",
                            tooltip = "Save the current iNZight session",
                            handler = function(h, ...) {
                                f <- gfile(
                                    text = tr("menu_file_size"),
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
                        load = gaction(paste(tr("menu_file_load"), "..."),
                            icon = "open",
                            tooltip = "Load a saved iNZight session",
                            handler = function(h, ...) {
                                f <- gfile(
                                    text = tr("menu_file_load"),
                                    type = "open",
                                    filter = list(
                                        "iNZight save files (*.inzsave)" = list(patterns = c("*.inzsave")),
                                        "All files" = list(patterns = "*")
                                    )
                                )
                                # ga <- galert("Please wait while session loads",
                                #     title = "Loading session",
                                #     delay = -1,
                                #     parent = GUI$win
                                # )
                                # Sys.sleep(0.1)
                                GUI$loadState(f)
                                # ga$FUN()
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
            if (!hasData()) return(placeholder("menu_data"))
            menu <- list(
                filter =
                    gaction(paste(tr("menu_data_filter"), "..."),
                        icon = "subset",
                        handler = function(h, ...) iNZFilterWin$new(GUI)),
                sort =
                    gaction(paste(tr("menu_data_sort"), "..."),
                        icon = "sort-ascending",
                        handler = function(h, ...) iNZSortbyDataWin$new(GUI)),
                aggregate =
                    gaction(paste(tr("menu_data_agg"), "..."),
                        icon = "dnd-multiple",
                        handler = function(h, ...) iNZAggregateWin$new(GUI)),
                stack =
                    gaction(paste(tr("menu_data_stack"), "..."),
                        icon = "dnd-multiple",
                        handler = function(h, ...) iNZstackVarWin$new(GUI)),
                "DATAOP" = list(
                    reshape =
                        gaction(paste(tr("menu_data_reshape"), "..."),
                            icon = "dataframe",
                            tooltip = "Transform from wide- to long-form data",
                            handler = function(h, ...) iNZReshapeDataWin$new(GUI)),
                    separate =
                        gaction(paste(tr("menu_data_separate"), "..."),
                            icon = "dataframe",
                            tooltip = "Separate columns",
                            handler = function(h, ...) iNZSeparateDataWin$new(GUI)),
                    unite =
                        gaction(paste(tr("menu_data_unite"), "..."),
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
                                paste(tr("menu_data_report"), "..."),
                                icon = "select-all",
                                handler = function(h, ...) iNZDataReportWin$new(GUI)
                            )
                    } else NULL,
                validate =
                    gaction(paste(tr("menu_data_validate"), "..."),
                        icon = "apply",
                        handler = function(h, ...) iNZValidateWin$new(GUI)
                    ),
                reorder =
                    gaction(paste(tr("menu_data_reorder"), "..."),
                        icon = "sort-ascending",
                        handler = function(h, ...) iNZReorderVarsWin$new(GUI)
                    ),
                gseparator(),
                view =
                    gaction(tr("menu_data_view"),
                        icon = "datasheet",
                        handler = function(h, ...) GUI$view_dataset()
                    ),
                rename =
                    gaction(paste(tr("menu_data_rename"), "..."),
                        icon = "editor",
                        handler = function(h, ...) iNZrenameDataWin$new(GUI)),
                restore =
                    gaction(tr("menu_data_restore"),
                        icon = "revert-to-saved",
                        handler = function(h, ...) GUI$restoreDataset()),
                delete =
                    gaction(tr("menu_data_delete"),
                        icon = "delete",
                        handler = function(h, ...) GUI$deleteDataset()),
                "MERGEJOIN" = list(
                    joinbycol =
                        gaction(paste(tr("menu_data_joincols"), "..."),
                            icon = "copy",
                            handler = function(h, ...) iNZjoinDataWin$new(GUI)),
                    appendrows =
                        gaction(paste(tr("menu_data_appendrows"), "..."),
                            icon = "edit",
                            handler = function(h, ...) iNZappendrowWin$new(GUI))
                ),
                gseparator(),
                "SURVEY" = list(
                    surveydesign =
                        gaction(paste(tr("menu_data_svyspec"), "..."),
                            icon = "new",
                            handler = function(h, ...)
                                iNZSurveyDesign$new(GUI, type = "survey")
                        ),
                    repdesign =
                        gaction(paste(tr("menu_data_svyrep"), "..."),
                            icon = "new",
                            handler = function(h, ...)
                                iNZSurveyDesign$new(GUI, type = "replicate")
                        ),
                    poststrat =
                        gaction(paste(tr("menu_data_svypost"), "..."),
                            icon = "edit",
                            handler = function(h, ...) iNZSurveyPostStrat$new(GUI)
                        ),
                    removedesign =
                        gaction(tr("menu_data_svyremove"),
                            icon = "delete",
                            handler = function(h, ...) GUI$removeDesign()
                        )
                ),
                FREQS = list(
                    expandtable =
                        gaction(tr("menu_data_expandtbl"),
                            icon = "datasheet",
                            handler = function(h, ...) iNZexpandTblWin$new(GUI)
                        ),
                    setfrequency =
                        gaction(paste(tr("menu_data_setfreq"), "..."),
                            icon = "datasheet",
                            handler = function(h, ...)
                                iNZSurveyDesign$new(GUI, type = "frequency")
                        ),
                    dropfrequency =
                        gaction(tr("menu_data_dropfreq"),
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
                enabled(menu[["DATAOP"]]$reshape) <- FALSE
                enabled(menu[["MERGEJOIN"]]$appendrows) <- FALSE

                menu[["FREQS"]] <- gaction(tr("menu_data_freq"))
                enabled(menu[["FREQS"]]) <- FALSE
            }
            names(menu)[names(menu) == "DATAOP"] <- tr("menu_data_operation")
            names(menu)[names(menu) == "MERGEJOIN"] <- tr("menu_data_mergejoin")
            names(menu)[names(menu) == "SURVEY"] <- tr("menu_data_svy")
            names(menu)[names(menu) == "FREQS"] <- tr("menu_data_freq")
            menu
        },
        VariablesMenu = function() {
            if (!hasData()) return(placeholder("menu_vars"))
            menu <- list(
                cont2cat =
                    gaction(paste(tr("menu_vars_convert2cat"), "..."),
                        icon = "convert",
                        tooltip = "Convert a variable to a categorical type",
                        handler = function(h, ...) iNZconToCatWin$new(GUI)),
                CATVARS = list(
                    reorder =
                        gaction(paste(tr("menu_vars_reorderlevels"), "..."),
                            icon = "sort-ascending",
                            tooltip = "Reorder the levels of a categorical variable",
                            handler = function(h, ...) iNZreorderWin$new(GUI)),
                    collapse =
                        gaction(paste(tr("menu_vars_collapse"), "..."),
                            icon = "dnd-multiple",
                            tooltip = "Collapse two or more levels into one",
                            handler = function(h, ...) iNZcllpsWin$new(GUI)),
                    rename =
                        gaction(paste(tr("menu_vars_renamelevels"), "..."),
                            icon = "edit",
                            tooltip = "Rename a categorical variable's levels",
                            handler = function(h, ...) iNZrenameWin$new(GUI)),
                    combine =
                        gaction(paste(tr("menu_vars_combinecatvars"), "..."),
                            icon = "dnd-multiple",
                            tooltip = "Combine two or more categorical variables",
                            handler = function(h, ...) iNZcmbCatWin$new(GUI))
                ),
                NUMVARS = list(
                    transform =
                        gaction(paste(tr("menu_vars_transform"), "..."),
                            icon = "convert",
                            tooltip = "Transform a variable using a function",
                            handler = function(h, ...) iNZtrnsWin$new(GUI)),
                    standardise =
                        gaction(paste(tr("menu_vars_standardise"), "..."),
                            icon = "convert",
                            tooltip = "Standardise a numeric variable",
                            handler = function(h, ...) iNZstdVarWin$new(GUI)),
                    class =
                        gaction(paste(tr("menu_vars_classint"), "..."),
                            icon = "convert",
                            tooltip = "Convert numeric variable into categorical intervals",
                            handler = function(h, ...) iNZfrmIntWin$new(GUI)),
                    rank =
                        gaction(paste(tr("menu_vars_rank"), "..."),
                            icon = "sort-ascending",
                            tooltip = "Create an ordering variable",
                            handler = function(h, ...) iNZrankNumWin$new(GUI)),
                    cat =
                        gaction(paste(tr("menu_vars_convert2catmulti"), "..."),
                            icon = "convert",
                            tooltip = "Convert multiple numeric variables to categorical",
                            handler = function(h, ...) iNZctocatmulWin$new(GUI))
                ),
                DATES = list(
                  convert =
                    gaction(paste(tr("menu_vars_convert2dt"), "..."),
                            icon = "date",
                            tooltip = "Convert a variable to a dates and times type",
                            handler = function(h, ...) iNZconTodtWin$new(GUI)),
                  extract =
                    gaction(paste(tr("menu_vars_extractdt"), "..."),
                            icon = "date",
                            tooltip = "Extract parts from a dates and times variable",
                            handler = function(h, ...) iNZExtfromdtWin$new(GUI)),
                  aggregation =
                    gaction(paste(tr("menu_vars_aggregate2dt"), "..."),
                            icon = "date",
                            tooltip = "Aggregate date-time into monthly or quarterly",
                            handler = function(h, ...) iNZAggregatedtWin$new(GUI))
                ),
                rename =
                    gaction(paste(tr("menu_vars_rename"), "..."),
                        icon = "edit",
                        tooltip = "Rename a variable",
                        handler = function(h, ...) iNZrnmVarWin$new(GUI)),
                create =
                    gaction(paste(tr("menu_vars_create"), "..."),
                        icon = "new",
                        tooltip = "Create a new variable using a formula",
                        handler = function(h, ...) iNZcrteVarWin$new(GUI)),
                miss2cat =
                    gaction(paste(tr("menu_vars_miss2cat"), "..."),
                        icon = "index",
                        tooltip = "Create a variable to include missingness information",
                        handler = function(h, ...) iNZmissCatWin$new(GUI)),
                delete =
                    gaction(paste(tr("menu_vars_delete"), "..."),
                        icon = "delete",
                        tooltip = "Permanently delete a variable",
                        handler = function(h, ...) iNZdeleteVarWin$new(GUI))
            )
            if (!is.null(GUI$getActiveDoc()$getModel()$getDesign())) {
                # disable some items for surveys
                enabled(menu$NUMVARS$class) <- FALSE
                menu$DATES <- gaction(tr("menu_vars_dates"), enabled = FALSE)
                enabled(menu$DATES) <- FALSE
            }
            names(menu)[names(menu) == "NUMVARS"] <- tr("menu_vars_numvars")
            names(menu)[names(menu) == "CATVARS"] <- tr("menu_vars_catvars")
            names(menu)[names(menu) == "DATES"] <- tr("menu_vars_dates")
            menu
        },
        PlotMenu = function() {
            if (!hasData()) return(placeholder("menu_plot"))
            plotmenu
        },
        setPlotMenu = function(menu) {
            plotmenu <<- menu
            updateMenu("menu_plot", PlotMenu())
        },
        AdvancedMenu = function() {
            if (!hasData() && modules_installed) {
                ## just provide the ability to install modules
                return(
                    list(
                        installmaps =
                            gaction(paste(tr("menu_adv_mapsinstall"), "..."),
                                icon = "symbol_diamond",
                                "tooltip" = "Install the Maps module",
                                handler = function(h, ...) InstallMaps(GUI)
                            ),
                        manage =
                            gaction(paste(tr("menu_adv_manage"), "..."),
                                icon = "execute",
                                tooltip = "Add, update, and remove add-on modules.",
                                handler = function(h, ...)
                                    iNZightModules::ModuleManager$new(GUI)
                            )
                    )
                )
            }

            # ---- this should all be unnecessary now --- #
            ## As of R 3.6.?, overwriting s3 methods is a verbose message
            ## when loading a package namespace. This prevents those messages
            ## from showing up.
            ## Info: it's because iNZightRegression and iNZightMR both define
            ## moecalc methods - not sure why/which is more up to date, either ...
            # suppressMessages(requireNamespace("iNZightModules", quietly = TRUE))
            # ---- to here --- #

            if (modules_installed) {
                adv <- list(
                    quick_explore = list(
                        missing =
                            gaction(paste(tr("menu_adv_missing"), "..."),
                                icon = "symbol_diamond",
                                tooltip = "Explore missing values",
                                handler = function(h, ...) iNZExploreMissing$new(GUI)),
                        all1varplot =
                            gaction(paste(tr("menu_adv_plot1var"), "..."),
                                icon = "symbol_diamond",
                                tooltip = "Click through a plot of each variable",
                                handler = function(h, ...) iNZallPlots$new(GUI)),
                        all2varsmry =
                            gaction(paste(tr("menu_adv_smry1var"), "..."),
                                icon = "symbol_diamond",
                                tooltip = "Get a summary of all variables",
                                handler = function(h, ...) iNZallSummaries$new(GUI)),
                        all2var =
                            gaction(paste(tr("menu_adv_plot2var"), "..."),
                                icon = "symbol_diamond",
                                tooltip = "Click through all 2-variable plots",
                                handler = function(h, ...) iNZall2Plots$new(GUI)),
                        pairs =
                            gaction(paste(tr("menu_adv_pairs"), "..."),
                                icon = "symbol_diamond",
                                tooltip = "See a pairs plot matrix",
                                handler = function(h, ...) iNZscatterMatrix$new(GUI))
                    ),
                    plot3d =
                        gaction(paste(tr("menu_adv_plot3d"), "..."),
                            icon = "3dcontour",
                            tooltip = "Start the 3D plotting module",
                            handler = function(h, ...) {
                                ign <- gwindow("...", visible = FALSE)
                                tag(ign, "dataSet") <- GUI$getActiveData()
                                e <- list(obj = ign)
                                e$win <- GUI$win
                                iNZightModules::plot3D(e)
                            }),
                    timeseries =
                        gaction(paste(tr("menu_adv_timeseries"), "..."),
                            icon = "ts",
                            tooltip = "Start the time series module",
                            handler = function(h, ...) iNZightModules::iNZightTSMod$new(GUI)),
                    modelfitting =
                        gaction(paste(tr("menu_adv_modfit"), "..."),
                            icon = "lines",
                            tooltip = "Start the model fitting module",
                            handler = function(h, ...) iNZightModules::iNZightRegMod$new(GUI)),
                    multires =
                        gaction(paste(tr("menu_adv_multires"), "..."),
                            icon = "hist",
                            tooltip = "Start the multiple response module",
                            handler = function(h, ...) iNZightModules::iNZightMultiRes$new(GUI)),
                    maps =
                        gaction(paste(tr("menu_adv_maps"), "..."),
                            icon = "plot1",
                            handler = function(h, ...) iNZightModules::iNZightMapLanding$new(GUI)),
                    gseparator(),
                    manage =
                        gaction(paste(tr("menu_adv_manage"), "..."),
                            icon = "execute",
                            tooltip = "Add, update, and remove add-on modules.",
                            handler = function(h, ...)
                                iNZightModules::ModuleManager$new(GUI))
                )
            } else {
                adv <- list(
                    install_modules =
                        gaction(paste(tr("menu_adv_install"), "..."),
                            icon = "execute",
                            tooltip = "Install the iNZightModules R pacakge to access add-on modules",
                            handler = function(h, ...) {
                                c <- gconfirm("You are about to install the iNZightModules R package. Are you sure you want to continue?",
                                    parent = GUI$win)
                                if (!c) return()
                                e <- "utils::install.packages('iNZightModules',
                                    repos = c('https://r.docker.stat.auckland.ac.nz', 'https://cran.rstudio.com'))"

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
                                glabel("Please wait while the package and its dependencies are installed...", container = ggg,
                                    anchor = c(-1, 1))
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
            }

            adv <- c(adv,
                list(
                    gseparator(),
                    rcode =
                        gaction(paste(tr("menu_adv_code"), "..."),
                            icon = "rlogo",
                            tooltip = "Show the R code history for your session",
                            handler = function(h, ...) GUI$showHistory())
                )
            )
            if (modules_installed && !is.null(GUI$addonModuleDir)) {
                modules <- iNZightModules:::getModules(GUI$addonModuleDir)
                if (length(modules)) {
                    instindex <- which(names(adv) == "maps") + 1
                    mods <- lapply(modules, function(mod) {
                        # at this point, `tr(name, __DICTIONARY__)` where __DICTIONARY__ comes from module
                        gaction(tr(mod$display_name),
                            handler = function(h, ...) {
                                x <- sprintf("mod$%s$new(GUI, name = '%s')",
                                    mod$name,
                                    mod$display_name
                                )
                                eval(parse(text = x))
                            }
                        )
                    })
                    adv <- c(adv[1:(instindex-1)], mods, adv[instindex:length(adv)])
                }
            }
            names(adv)[names(adv) == "quick_explore"] <- tr("menu_adv_quick")
            adv
        },
        HelpMenu = function() {
            guides <- list(
                user_guides.basics = tr("menu_help_guide_basics"),
                user_guides.interface = tr("menu_help_guide_interface"),
                user_guides.plot_options = tr("menu_help_guide_plot"),
                user_guides.variables = tr("menu_help_guide_vars"),
                user_guides.data_options = tr("menu_help_guide_data"),
                user_guides.add_ons = tr("menu_help_guide_advanced")
            )
            menu <- list(
                about =
                    gaction(tr("menu_help_about"),
                        icon = "about",
                        tooltip = "",
                        handler = function(h, ...) iNZAboutWidget$new(GUI)),
                GUIDES = lapply(
                    names(guides),
                    function(n) {
                        gaction(
                            guides[[n]],
                            icon = "help_topic",
                            tooltip = "",
                            handler = function(h, ...) help_page(gsub(".", "/", n, fixed = TRUE))
                        )
                    }
                ),
                change =
                    gaction(tr("menu_help_history"),
                        icon = "file",
                        tooltip = "",
                        handler = function(h, ...)
                            help_page('support/changelog/?pkg=iNZight')),
                faq =
                    gaction(tr("menu_help_faq"),
                        icon = "find",
                        tooltip = "",
                        handler = function(h, ...)
                            help_page("support/faq/")),
                contact =
                    gaction(tr("menu_help_contact"),
                        icon = "help",
                        tooltip = "",
                        handler = function(h, ...)
                            help_page("support/contact/"))
            )
            names(menu)[names(menu) == "GUIDES"] <- tr("menu_help_guides")
            menu
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

            w <- gwindow("About iNZight", width = 500, height = 400, visible = TRUE, parent = GUI$win)
            g <- gvbox(expand = FALSE, cont = w, spacing = 5)
            g$set_borderwidth(10)
            mainlbl <- glabel("iNZight", container = g)
            font(mainlbl) <- list(weight = "bold", family = "sans", size = 20)
            verlbl <- glabel(sprintf("Version %s - Released %s",
                                     packageDescription("iNZight")$Version,
                                     format(as.POSIXct(packageDescription("iNZight")$Date),
                                            "%d %B, %Y")), container = g)
            font(verlbl) <- list(weight = "normal", family = "sans", size = 10)
            rverlbl <- glabel(sprintf("Running on R version %s", getRversion()), container = g)
            font(rverlbl) <- list(weight = "normal", family = "sans", size = 10)
            addSpace(g, 10)
            gpltxt <- gtext(expand = TRUE, cont = g, wrap = TRUE)
            insert(gpltxt, paste("\n\nThis program is free software; you can redistribute it and/or",
                                 "modify it under the terms of the GNU General Public License",
                                 "as published by the Free Software Foundation; either version 2",
                                 "of the License, or (at your option) any later version.\n"),
                   font.attr = list(size = 9)) -> l1
            insert(gpltxt, paste("This program is distributed in the hope that it will be useful,",
                                 "but WITHOUT ANY WARRANTY; without even the implied warranty of",
                                 "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
                                 "GNU General Public License for more details.\n"),
                   font.attr = list(size = 9)) -> l2
            insert(gpltxt, paste("You should have received a copy of the GNU General Public License",
                                 "along with this program; if not, write to the Free Software",
                                 "Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.\n"),
                   font.attr = list(size = 9)) -> l3
            insert(gpltxt, paste("You can view the full licence here:\nhttp://www.gnu.org/licenses/gpl-2.0-standalone.html"),
                   font.attr = list(size = 9)) -> l4
            addSpace(g, 5)
            contactlbl <- glabel("For help, contact inzight_support@stat.auckland.ac.nz", container = g)
            font(contactlbl) <- list(weight = "normal", family = "sans", size = 8)
            visible(w) <- TRUE
        }
    )
)

InstallMaps <- function(gui) {
    check.maps <- 'requireNamespace("iNZightMaps", quietly = TRUE)'
    if (eval(parse(text = check.maps))) {
        gmessage("The maps package is already installed!", parent = gui$win)
        return()
    }

    svalue(gui$statusbar) <- "Installing maps module ..."
    utils::install.packages(
        "iNZightMaps",
        repos = c("https://r.docker.stat.auckland.ac.nz", "https://cran.stat.auckland.ac.nz"),
        dependencies = TRUE
    )

    if (!eval(parse(text = check.maps))) {
        svalue(gui$statusbar) <- "Error installing the maps module"
        gmessage("Unable to install package. Please check the website.", parent = gui$win)
        return()
    }

    ## reload the menu ...?
    svalue(gui$statusbar) <- "Maps module installed successfully"
    gui$menuBarWidget$defaultMenu()
    gmessage("The Maps package has been installed.", parent = gui$win)
}
