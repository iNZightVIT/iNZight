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
    properties(fields = list(
                   ## list of iNZDocuments (contain data, plotSettings)
                   iNZDocuments = "list",
                   ## the active document of the iNZDocuments list
                   activeDoc = "numeric",
                   ## the main GUI window
                   win = "ANY",
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
                   ## allow modules to attach data to the GUI
                   moduledata = "list",
                   ## keep a track of R code history
                   rhistory = "ANY"
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
        initializeGui = function(data = NULL, disposeR = FALSE) {
            "Initiates the GUI"
            iNZDocuments <<- list(iNZDocument$new(data = data))
            win.title <- paste("iNZight (v",
                               packageDescription("iNZight")$Version,
                               ")", sep = "")

            OS <<- if (.Platform$OS == "windows") "windows" else if (Sys.info()["sysname"] == "Darwin") "mac" else "linux"

            ## We must set the correct directory correctly ...
            switch(OS,
                   "windows" = {
                       done <- FALSE
                       if (file.exists(file.path("~", "iNZightVIT"))) {
                           setwd(file.path("~", "iNZightVIT"))

                           ## Now check to see if there is a library in there ...
                           if (!file.exists("modules"))
                               dir.create("modules")
                       } else {
                           ## Create it:
                           conf <- gconfirm(paste("Do you want to create an iNZightVIT directory",
                                                  "in your My Documents folder to save data and preferences?"),
                                            title = "Create Folder", icon = "question")

                           if (conf) {
                               if ( dir.create(file.path("~", "iNZightVIT")) ) {
                                   ## copy the Data folder:
                                   ##try(file.copy("Data.lnk", file.path("~", "iNZightVIT")), TRUE)
                                   ##try(file.symlink("data", file.path("~", "iNZightVIT")), TRUE)

                                   ##setwd(file.path("~", "iNZightVIT"))

                                   dir.create(file.path("~", "iNZightVIT", "modules"))

                                   done <- TRUE
                               }

                               if (!done)
                                   gmessage("iNZight was unable to create the folder.")
                           }
                       }

                       ## Set the library path if it exists
                       if (file.exists(file.path("~", "iNZightVIT", "modules")))
                           .libPaths(file.path("~", "iNZightVIT", "modules"))
                   },
                   "mac" = {
                       done <- FALSE
                       if (file.exists(file.path("~", "Documents", "iNZightVIT"))) {
                           setwd(file.path("~", "Documents", "iNZightVIT"))
                       } else {
                           ## Create it:
                           conf <- gconfirm(paste("Do you want to create an iNZightVIT directory",
                                                  "in your Documents folder to save data and preferences?"),
                                            title = "Create Folder", icon = "question")

                           if (conf) {
                               if ( dir.create(file.path("~", "Documents", "iNZightVIT")) ) {
                                   dir.create(file.path("~", "Documents", "iNZightVIT", "modules"))
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
                   })


            ## Grab settings file (or try to!)
            getPreferences()

            ## Check for updates ... need to use try incase it fails (no connection etc)
            if (preferences$check.updates) {
                try({
                    oldpkg <- old.packages(repos = "http://r.docker.stat.auckland.ac.nz/R")
                    if (nrow(oldpkg) > 0) {
                        win.title <- paste(win.title, " [updates available]")
                    }
                }, silent = TRUE)
            }

            popOut <<- preferences$popout

            win <<- gwindow(win.title, visible = FALSE,
                            width = if (popOut) NULL else preferences$window.size[1],
                            height = preferences$window.size[2])

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
        }, ## end initialization
        ## set up the menu bar widget
        initializeMenu = function(cont, disposeR) {
            actionList <- list(
                import = gaction(
                  #1
                    label = "Import Data...", icon = "symbol_diamond",
                    tooltip = "Import a new Dataset",
                    handler = function(h, ...) iNZImportWin$new(.self)
                    ),
                export = gaction(
                  #2
                    label = "Export Data...", icon = "symbol_diamond",
                    handler = function(h, ...) iNZSaveWin$new(.self,
                        type = "data",
                        data = .self$getActiveData())
                    ),
                conToCat = gaction(
                  #3
                    label = "Convert to Categorical...",
                    icon = "symbol_diamond",
                    tooltip = "Convert a variable to a categorical type",
                    handler = function(h, ...) iNZconToCatWin$new(.self)
                    ),
                trns = gaction(
                  #4
                    label = "Transform Variables...",
                    icon = "symbol_diamond",
                    tooltip = "Transform a variable using a function",
                    handler = function(h, ...) iNZtrnsWin$new(.self)
                    ),
                clps = gaction(
                  #5
                    label = "Collapse Levels...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZcllpsWin$new(.self)
                    ),
                reordLvl = gaction(
                  #6
                    label = "Reorder Levels...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZreorderWin$new(.self)
                    ),
                renmLvl = gaction(
                  #7
                    label = "Rename Levels...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZrenameWin$new(.self)
                    ),
                cmbnCat = gaction(
                  #8
                    label = "Combine Categorical Variables...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZcmbCatWin$new(.self)
                    ),
                create = gaction(
                  #9
                    label = "Create New Variables...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZcrteVarWin$new(.self)
                    ),
                ## The code for displaying this window is already there
                ## just the functionality of forming the intervals is
                ## left to be implemented
                 frmInt = gaction(
                   #10
                   label = "Form Class Intervals...",
                     icon = "symbol_diamond",
                     handler = function(h, ...) iNZfrmIntWin$new(.self)
                     ),
                renmVar = gaction(
                  #11
                    label = "Rename Variables...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZrnmVarWin$new(.self)
                    ),
                stdVar = gaction(
                  #12
                    label = "Standardize Variables...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZstdVarWin$new(.self)
                    ),
                slctCases = gaction(
                  #13
                    label = "Filter Dataset...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZFilterWin$new(.self)
                    ),
                rshpData = gaction(
                  #14
                    label = "Reshape Dataset...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZReshapeDataWin$new(.self)
                    ),
                rstrData = gaction(
                  #15
                    label = "Restore Dataset",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        ## NOTE: look into this - best way of 'restoring'? (why not just revert activeDoc??)
                        ## code should just start using `data` instead of `dataX`
                        setDocument(iNZDocument$new(data = iNZDocuments[[1]]$getModel()$origDataSet))
                    }
                    ),
                home = gaction(
                  #16
                    label = "iNZightVIT Home",
                    icon = "symbold_diamond",
                    handler = function(h, ...) {
                        dispose(win)
                        iNZightVIT(disposeR = disposeR)
                    }),
                tsMod = gaction(
                  #17
                    label = "Time Series...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        iNZightModules::iNZightTSMod$new(.self)
                    }
                    ),
                modelFit = gaction(
                  #18
                    label = "Model Fitting...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        ign <- gwindow("...", visible = FALSE)

                        tag(ign, "dataSet") <- getActiveData()
                        curMod <- getActiveDoc()$getModel()
                        if (!is.null(curMod$dataDesign))
                            tag(ign, "design") <- curMod$createSurveyObject()
                        e <- list(obj = ign)
                        e$win <- win
                        iNZightModules::modelFitting(e)
                    }
                    ),
                threeDPlot = gaction(
                  #19
                    label = "3D Plot...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        ign <- gwindow("...", visible = FALSE)
                        tag(ign, "dataSet") <- getActiveData()
                        e <- list(obj = ign)
                        e$win <- win
                        iNZightModules::plot3D(e)
                    }
                    ),
                scatterMatrix = gaction(
                  #20
                    label = "Pairs...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        iNZscatterMatrix$new(.self)
                    }
                    ),
                deleteVariables = gaction(
                  #21
                    label = "Delete Variables...",
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
                    label = "Missing to Categorical...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZmissCatWin$new(.self)
                    ),
                all2Plots = gaction(
                  #26
                    label = "Explore 2-variable Plots...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        iNZall2Plots$new(.self)
                    }
                    ),
                sortBy = gaction(
                  #27
                  label = "Sort data by variables...",
                  icon = "symbol_diamond",
                  handler = function(h, ...){
                    iNZSortbyDataWin$new(.self)
                  }
                ),
                agraData = gaction(
                  #28
                  label = "Aggregate data...",
                  icon = "symbol_diamond",
                  handler = function(h, ...){
                      iNZAgraDataWin$new(.self)
                  }
                ),
                rankNum = gaction(
                  #29
                  label = "Rank Numerical Variables...",
                  icon = "symbol_diamond",
                  handler = function(h, ...) iNZrankNumWin$new(.self)
                  ),
                convert2mc = gaction(
                  #30
                  label = "Convert to Categorial (Multiple)...",
                  icon = "symbol_diamond",
                  handler = function(h, ...) iNZctocatmulWin$new(.self)
                  ),
                stackVar = gaction(
                  #31
                    label = "Stack variables...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZstackVarWin$new(.self)
                ),
                multipleResponse = gaction(
                    ## 32
                    label = "Multiple Response...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        iNZightModules::iNZightMultiRes$new(.self)
                    }
                ),
                aboutiNZight = gaction(
                    ## 33
                    label = "About",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        w <- gwindow("About iNZight", width = 500, height = 400, visible = TRUE, parent = win)
                        g <- gvbox(expand = FALSE, cont = w, spacing = 5)
                        g$set_borderwidth(10)
                        mainlbl <- glabel("iNZight", container = g)
                        font(mainlbl) <- list(weight = "bold", family = "normal", size = 20)
                        verlbl <- glabel(sprintf("Version %s - Released %s",
                                                 packageDescription("iNZight")$Version,
                                                 format(as.POSIXct(packageDescription("iNZight")$Date),
                                                        "%d %B, %Y")), container = g)
                        font(verlbl) <- list(weight = "normal", family = "normal", size = 10)
                        rverlbl <- glabel(sprintf("Running on R version %s", getRversion()), container = g)
                        font(rverlbl) <- list(weight = "normal", family = "normal", size = 10)
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
                        font(contactlbl) <- list(weight = "normal", family = "normal", size = 8)
                        visible(w) <- TRUE
                    }
                ),
                faqPage = gaction(
                    ## 34
                    label = "FAQ",
                    icon  = "symbol_diamond",
                    handler = function(h, ...) {
                        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/support/faq/")
                    }
                ),
                contactPage = gaction(
                    ## 35
                    label = "Contact Support/Report a Bug",
                    icon  = "symbol_diamond",
                    handler = function(h, ...) {
                        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/support/contact/")
                    }
                ),
                prefs = gaction (
                    ## 36
                    label = "Preferences",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZPrefsWin$new(.self)
                ),
                exit = gaction(
                    ## 37
                    label = "Exit",
                    icon = "symbold_diamond",
                    handler = function(h, ...) if (disposeR) q(save = "no") else dispose(win)
                ),
                aboutiNZight = gaction(
                    ## 38
                    label = "Privacy",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        w <- gwindow("Privacy", width = 500, height = 250, visible = TRUE, parent = win)
                        g <- gvbox(expand = FALSE, cont = w, spacing = 5)
                        g$set_borderwidth(10)

                        mainlbl <- glabel("iNZight User Privacy", container = g)
                        font(mainlbl) <- list(weight = "bold", family = "normal", size = 20)

                        gpltxt <- gtext(expand = TRUE, cont = g, wrap = TRUE)
                        insert(gpltxt, paste("\n\nTo provide us with information about our users and their behaviour,",
                                             "we collect ANONYMOUS information about iNZight, such as the version number",
                                             "you are using, the operating system, and how frequently iNZight is updated.",
                                             "We collect ABSOLUTELY NO personal information from your computer. We place",
                                             "a unique cookie that allows us to identify unique users, however this is not",
                                             "in any way connected to your personal information.\n"),
                               font.attr = list(size = 9)) -> l1
                        insert(gpltxt, paste("If you wish to remain completely anonymous, then you can turn off the sharing of",
                                             "usage information by going to 'File' > 'Preferences' and deselecting the appropriate box.\n"),
                               font.attr = list(size = 9)) -> l2
                        visible(w) <- TRUE
                    }
                ),
                guideBasics = gaction(
                    ## 39
                    label = "The Basics",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/basics/")
                    }
                ),
                guideInterface = gaction(
                    ## 40
                    label = "The Interface",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/interface/")
                    }
                ),
                guidePlotOptions = gaction(
                    ## 41
                    label = "Plot Options",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/plot_options/")
                    }
                ),
                guideVariables = gaction(
                    ## 42
                    label = "Variables Menu",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/")
                    }
                ),
                guideDataOptions = gaction(
                    ## 43
                    label = "Dataset Menu",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/data_options/")
                    }
                ),
                guideAdditional = gaction(
                    ## 44
                    label = "Advanced Menu",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/add_ons/")
                    }
                ),
                specifyDesign = gaction(
                    ## 45
                    label = "[BETA] Specify Survey Design ...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZSurveyDesign$new(.self)
                ),
                removeDesign = gaction(
                    ## 46
                    label = "Remove Design",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        .self$getActiveDoc()$getModel()$setDesign()
                        ## ENABLE A WHOLE LOT OF STUFF
                        enabled(menubar$menu_list[["Dataset"]][[3]]) <<- TRUE
                        enabled(menubar$menu_list[["Variables"]][["Numeric Variables"]][[2]]) <<- TRUE
                        enabled(menubar$menu_list[["Plot"]][[3]]) <<- TRUE
                        enabled(sumBtn) <<- TRUE
                        enabled(infBtn) <<- TRUE
                    }
                ),
                ############ MAPS ############
                maps = gaction(
                    ## 47
                    label = "Maps...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        iNZightModules::iNZightMapMod$new(.self)
                    }
                ),
                import = gaction(
                    ## 48
                    label = "Example data...", icon = "symbol_diamond",
                    tooltip = "Load Example Data",
                    handler = function(h, ...) iNZImportExampleWin$new(.self)
                ),
                convTbl = gaction(
                    ## 49
                    label = "Expand Table ...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZexpandTblWin$new(.self)
                ),
                load = gaction(
                    ## 50
                    label = "Load ...", icon = "symbol_diamond",
                    handler = function(h, ...) iNZLoadSaveWin$new(.self, action = "load")
                ),
                save = gaction(
                    ## 51
                    label = "Save ...", icon = "save",
                    handler = function(h, ...) iNZLoadSaveWin$new(.self, action = "save")
                ),
                importBeta = gaction(
                    ## 52
                    label = "Import Data (Beta) ...", icon = "symbol_diamond",
                    tooltip = "Import a new Dataset (new interface)",
                    handler = function(h, ...) iNZImportWinBeta$new(.self)
                ),
                modelFittingDev = gaction(
                    ## 53
                    label = "[Beta Version] Model Fitting ...", icon = "symbol_diamond",
                    tooltip = "Fit regression models",
                    handler = function(h, ...) iNZightModules::iNZightRegMod$new(.self)
                ),
                showRhistory = gaction(
                    ## 54
                    label = "[Beta] Show R Code History", icon = "symbol_diamond",
                    tooltip = "Show R history to reproduce results from R",
                    handler = function(h, ...) {
                        showHistory()
                    }
                ),
                newMapsModule = gaction(
                    ## 55
                    label = "[Beta] New Maps Module", icon = "symbol_diamond",
                    tooltip = "Load the new Maps module",
                    handler = function(h, ...) iNZightModules::iNZightMap2Mod$new(.self)
                )
            )
            ## home button is disabled if package 'vit' is not loaded
            if (!'package:vit' %in% search())
                enabled(actionList[[16]]) <- FALSE

            ## disable modules if packages are not loaded
            if (!requireNamespace("iNZightModules", quietly = TRUE)) {
                invisible(sapply(actionList[c(19,17,18,32,47)], function(x) {
                                     enabled(x) <- FALSE}))
            }
            if (!requireNamespace("iNZightMR", quietly = TRUE))
                enabled(actionList[[24]]) <- FALSE

            ## if R version is lower than 3.3, disable new maps module
            if (getRversion() < numeric_version(3.3))
                enabled(actionList[[55]]) <- FALSE

            menuBarList <- list(
                "File" = list(
                    actionList[[50]],
                    actionList[[51]],
                    gseparator(),
                    actionList[[16]],
                    gseparator(),
                    actionList[[1]],
                    actionList[[2]],
                    gseparator(),
                    actionList[[48]],
                    gseparator(),
                    actionList[[52]],
                    gseparator(),
                    actionList[[36]],
                    actionList[[37]]
                    ),
                "Dataset" = list(
                    actionList[[13]],
                    actionList[[27]],
                    actionList[[28]],
                    actionList[[31]],
                    actionList[[15]],
                    gseparator(),
                    actionList[[45]],
                    actionList[[46]],
                    gseparator(),
                    actionList[[49]]
                    ),
                "Variables" = list(
                    actionList[[3]],
                    "Categorical Variables" = actionList[c(6,5,7,8)],
                    "Numeric Variables" = actionList[c(4,12,10, 29,30)],
                    actionList[[11]],
                    actionList[[9]],
                    actionList[[25]],
                    actionList[[14]],
                    actionList[[21]]
                    ),
                "Plot" = list(
                    ),
                "Advanced" = list(
                    ## This will be automated in future
                    "Quick Explore" = actionList[c(24, 22, 23, 26, 20)],
                    actionList[[19]],
                    actionList[[17]],
                    actionList[[18]],
                    actionList[[32]],
                    actionList[[47]],
                    ## The new iNZightModelFitting module (under development)
                    gseparator(),
                    actionList[[53]],
                    actionList[[54]],
                    actionList[[55]]
                    ),
                "Help" = list(
                    actionList[[33]],
                    actionList[[38]],
                    "User Guides" = actionList[39:44],
                    actionList[[34]],
                    actionList[[35]]
                    )
                )

            if (!"package:vit" %in% search()) { # remove ...
                menuBarList[[1]][[5]] <- NULL  ## gseparator
                menuBarList[[1]][[4]] <- NULL  ## iNZightVIT Home
            }

            menubar <<- gmenu(menuBarList, container = cont)

            ## if (all(dim(.self$getActiveData()) == 1)) {
            ##     one day, get around to disabling the menu bar before data is loaded
            ## }
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
            addActDocObs(function() {
                dataNameWidget$updateWidget()
            })
            ## if the dataSet changes, update the data set name
            getActiveDoc()$addDataObserver(
                function() {
                    dataNameWidget$updateWidget()
                }
            )
            .self$dataNameWidget
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
                    if (!is.null(curSet$x)) {
                        if (is.numeric(curSet$x) & is.numeric(curSet$y)) {
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

                        w <- gwindow("Summary", 
                                     width = 800 * preferences$font.size / 10, 
                                     height = 400 * preferences$font.size / 10,
                                     visible = FALSE, parent = win)
                        g <- gvbox(container = w)
                        txtSmry <- gtext(text = paste(do.call(
                                             iNZightPlots:::getPlotSummary,
                                             curSet),
                                             collapse = "\n"),
                                         expand = TRUE, container = g, wrap = FALSE,
                                         font.attr = list(family = "monospace", 
                                                          size = preferences$font.size))

                        ## if regression OR anova is going on:
                        if (is.null(curSet$g1) && is.null(curSet$g2) &&
                            !is.null(curSet$y) && (is.numeric(curSet$x) | is.numeric(curSet$y)) &&
                            (!is.null(curSet$trend) | curSet$smooth > 0 | !is.numeric(curSet$x) | !is.numeric(curSet$y))) {
                            btngrp <- ggroup(container = g)
                            addSpace(btngrp, 5)

                            btnHandler <- function(h, ...) {
                                varType <- ifelse(grepl("residuals", svalue(h$obj)), "residual", "predict")

                                ## window asking for variable names:
                                w2 <- gwindow("Store fitted values", width = 350,
                                              parent = w, visible = FALSE)

                                g2 <- gvbox(container = w2)
                                g2$set_borderwidth(15)

                                lbl <- glabel(sprintf("Specify names for the new variable%s", ifelse(length(curSet$trend) > 1, "s", "")), container = g2,
                                              anchor = c(-1, -1))
                                font(lbl) <- list(size = 12, weight = "bold")

                                addSpace(g2, 20)


                                tbl <- glayout(container = g2)
                                ii <- 1

                                ## Predicted values for GROUP MEANS:
                                fittedLbl <- glabel("")
                                fittedName <- gedit(sprintf("%s.%s", curSet$varnames[[ifelse(is.numeric(curSet$y), "y", "x")]], varType),
                                                    width = 25)
                                if (is.factor(curSet$x) || is.factor(curSet$y)) { ##} || length(curSet$trend) == 1) {
                                    tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl
                                    tbl[ii, 4:6, expand = TRUE] <- fittedName
                                    ii <- ii + 1
                                }

                                ## Predicted values for LINEAR trend:
                                fittedLbl.lin <- glabel(ifelse(length(curSet$trend) > 1, "Linear :", ""))
                                fittedName.lin <- gedit(sprintf("%s.%s%s", curSet$varnames$y, varType,
                                                                 ifelse(length(curSet$trend) > 1, ".linear", "")),
                                                        width = 25)
                                if (length(curSet$trend) >= 1 && "linear" %in% curSet$trend) {
                                    tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl.lin
                                    tbl[ii, 4:6, expand = TRUE] <- fittedName.lin
                                    ii <- ii + 1
                                }

                                ## Predicted values for QUADRATIC trend:
                                fittedLbl.quad <- glabel(ifelse(length(curSet$trend) > 1, "Quadratic :", ""))
                                fittedName.quad <- gedit(sprintf("%s.%s%s", curSet$varnames$y, varType,
                                                                 ifelse(length(curSet$trend) > 1, ".quadratic", "")),
                                                         width = 25)
                                if (length(curSet$trend) >= 1 && "quadratic" %in% curSet$trend) {
                                    tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl.quad
                                    tbl[ii, 4:6, expand = TRUE] <- fittedName.quad
                                    ii <- ii + 1
                                }

                                ## Predicted values for CUBIC trend:
                                fittedLbl.cub <- glabel(ifelse(length(curSet$trend) > 1, "Cubic :", ""))
                                fittedName.cub <- gedit(sprintf("%s.%s%s", curSet$varnames$y, varType,
                                                                 ifelse(length(curSet$trend) > 1, ".cubic", "")),
                                                        width = 25)
                                if (length(curSet$trend) >= 1 && "cubic" %in% curSet$trend) {
                                    tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl.cub
                                    tbl[ii, 4:6, expand = TRUE] <- fittedName.cub
                                    ii <- ii + 1
                                }

                                ## Predicted values for SMOOTHER:
                                fittedLbl.smth <- glabel("Smoother :")
                                fittedName.smth <- gedit(sprintf("%s.%s.smooth", curSet$varnames$y, varType),
                                                        width = 25)
                                if (curSet$smooth > 0 && is.numeric(curSet$x) && is.numeric(curSet$y)) {
                                    tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl.smth
                                    tbl[ii, 4:6, expand = TRUE] <- fittedName.smth
                                    ii <- ii + 1
                                }

                                addSpring(g2)

                                okBtn <- gbutton("Ok", icon = "save",
                                                 handler = function(h, ...) {
                                                     FUN <- if (varType == "predict")
                                                                function(object)
                                                                    predict(object, newdata = data.frame(x = curSet$x))
                                                            else
                                                                function(object)
                                                                    residuals(object)

                                                     pred <- NULL
                                                     if (is.factor(curSet$x) || is.factor(curSet$y)) { #} || length(curSet$trend) == 1) {
                                                         ## just the one
                                                         fit <- with(curSet, lm(if (is.numeric(curSet$y)) y ~ x else x ~ y, na.action = na.exclude))
                                                         pred <- data.frame(FUN(fit))
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
                                                         newdata <- data.frame(getActiveData(), pred)
                                                     else
                                                         newdata <- getActiveData()


                                                     if (curSet$smooth > 0 && is.numeric(curSet$x) && is.numeric(curSet$y)) {
                                                         tmp <- data.frame(x = curSet$x, y = curSet$y)
                                                         fit <- with(curSet, loess(y ~ x, span = curSet$smooth, family = "gaussian", degree = 1, na.action = "na.exclude"))
                                                         pred <- data.frame(FUN(fit))
                                                         colnames(pred) <- svalue(fittedName.smth)
                                                         newdata <- data.frame(newdata, pred)
                                                     }


                                                     getActiveDoc()$getModel()$updateData(newdata)

                                                     dispose(w2)
                                                 }, container = g2)

                                visible(w2) <- TRUE
                            }

                            predBtn <- gbutton("Store fitted values", container = btngrp, handler = btnHandler)
                            residBtn <- gbutton("Store residuals", container = btngrp, handler = btnHandler)

                            addSpace(g, 0)
                        }

                        visible(w) <- TRUE
                    } else {
                        gmessage("Please select at least one variable",
                                 parent = win)
                    }
                })
            infBtn <<- gbutton(
                "Get Inference",
                handler = function(h, ...) {
                    curSet <- getActiveDoc()$getSettings()
                    if (!is.null(curSet$x)) {
                        ## Figure out what type of inference will be happening:
                        xnum <- is.numeric(curSet$x)

                        if (is.null(curSet$y)) {
                            INFTYPE <- ifelse(xnum, "onesample-ttest", "oneway-table")
                        } else {
                            ynum <- is.numeric(curSet$y)
                            if (xnum && ynum) {
                                INFTYPE <- "regression"
                            } else if (xnum | ynum) {
                                M <- if (xnum) length(levels(curSet$y)) else length(levels(curSet$x))
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
                        if (!is.null(curMod$dataDesign)) {
                            curSet$data <- NULL
                            curSet$design <- curMod$createSurveyObject()
                        }

                        w <- gwindow("Get Inference",
                                     width = 350,
                                     #height = 400,
                                     parent = win, visible = FALSE)
                        g <- gvbox(container = w, expand = TRUE, fill = TRUE)
                        g$set_borderwidth(5)

                        tbl <- glayout(container = g)
                        ii <- 1

                        ## Inference method
                        lbl <- glabel("Method :")
                        infMthd <- gradio(c("Normal", "Bootstrap"), horizontal = TRUE)
                        tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- lbl
                        tbl[ii, 4:6, expand = TRUE] <- infMthd
                        ii <- ii + 1

                        ii <- ii + 1


                        doHypTest <- grepl("ttest|anova|table", INFTYPE)
                        test.type <- switch(INFTYPE,
                                            "onesample-ttest" = "One Sample t-test",
                                            "twosample-ttest" = "Two Sample t-test",
                                            "anova"           = "ANOVA",
                                            "regression"      = "Regression Analysis",
                                            "oneway-table"    = ,
                                            "twoway-table"    = "Chi-square test")


                        ## Checkbox: perform hypothesis test? Activates hypothesis options.
                        TTEST <- grepl("ttest", INFTYPE)
                        TTEST2 <- INFTYPE == "twosample-ttest"
                        hypTest <- if (TTEST2)
                                       gradio(c("None", "Two Sample t-test", "ANOVA"), horizontal = TRUE)
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
                            }

                            ## alternative hypothesis
                            lbl <- glabel("Alternative Hypothesis :")
                            hypAlt <- gcombobox(c("two sided", "greater than", "less than"))
                            if (TTEST) {
                                tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- lbl
                                tbl[ii, 4:6, expand = TRUE] <- hypAlt
                                ii <- ii + 1
                            }

                            enabled(hypAlt) <- enabled(hypVal) <- if (TTEST2) svalue(hypTest, index = TRUE) == 2 else svalue(hypTest)

                            ## use equal variance assumption?
                            hypEqualVar <- gcheckbox("Use equal-variance", checked = FALSE)
                            if (TTEST2) {
                                tbl[ii, 4:6, expand = TRUE] <- hypEqualVar
                                ii <- ii + 1

                                enabled(hypEqualVar) <- svalue(hypTest, index = TRUE) == 2
                            }
                        }

                        addHandlerChanged(hypTest, function(h, ...) {
                            enabled(hypEqualVar) <- enabled(hypAlt) <- enabled(hypVal) <- if (TTEST2) svalue(hypTest, index = TRUE) == 2 else svalue(h$obj)
                        })


                        btn <- gbutton("OK", handler = function(h, ...) {
                            infType <- svalue(infMthd, index = TRUE)
                            sets <- curSet
                            sets <- modifyList(
                                sets,
                                list(bs.inference = infType == 2,
                                     summary.type = "inference",
                                     inference.type = "conf",
                                     inference.par = NULL)
                            )
                            if (ifelse(TTEST2, svalue(hypTest, index = TRUE) > 1, svalue(hypTest))) {
                                if (is.na(as.numeric(svalue(hypVal)))) {
                                    gmessage("Null value must be a valid number.", title = "Invalid Value",
                                             icon = "error")
                                    return()
                                }
                                sets <- modifyList(
                                    sets,
                                    list(hypothesis.value = as.numeric(svalue(hypVal)),
                                         hypothesis.alt   = switch(svalue(hypAlt, index = TRUE),
                                                                   "two.sided", "greater", "less"),
                                         hypothesis.var.equal = svalue(hypEqualVar),
                                         hypothesis.test = ifelse(TTEST2, switch(svalue(hypTest, index = TRUE), "default", "t.test", "anova"), "default")))
                            } else {
                                sets <- modifyList(sets, list(hypothesis = NULL), keep.null = TRUE)
                            }

                            ## Close setup window and display results
                            dispose(w)

                            infTitle <- "Inference Information"
                            if (infType == 2) {
                                ## Not sure why this acts weird. At least on Linux, the text inside `wBoots` doesn't become visible until the
                                ## function has finished.
                                wBoots <- gwindow("Performing Bootstrap ...",
                                                  parent = win, width=350, height=120, visible = FALSE)
                                ggBoots <- ggroup(container = wBoots)
                                addSpace(ggBoots, 5)
                                hBoots <- gvbox(container = ggBoots, spacing = 10)
                                addSpace(hBoots, 5)
                                iBoots <- gimage(stock.id = "info", cont = hBoots, size = "dialog")
                                fBoots <- gvbox(container = ggBoots, spacing = 10)
                                fBoots$set_borderwidth(10)
                                gBoots <- glabel("Please wait while iNZight\nperforms bootstrap simulations.", anchor = c(-1, 0), cont = fBoots)
                                font(gBoots) <- list(size = 14, weight = "bold")
                                gBoots2 <- glabel("Depending on the size of the data,\nthis could take a while.", anchor = c(-1, 0), cont = fBoots)
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
                                        sets),
                                    collapse = "\n"),
                                expand = TRUE, cont = w2, wrap = FALSE,
                                font.attr = list(family = "monospace", size = preferences$font.size))
                            visible(w2) <- TRUE
                            try(dispose(wBoots), silent = TRUE)
                        })

                        addSpring(g)
                        add(g, btn)

                        visible(w) <- TRUE

                    } else {
                        gmessage("Please select at least one variable",
                                 parent = win)
                    }
                })
            font(sumBtn) <<- list(weight = "bold",
                                 family = "normal",
                                 color = "navy")
            font(infBtn) <<- list(weight = "bold",
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
                    parent = win)
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
                ## Design or data?
                curMod <- getActiveDoc()$getModel()
                if (!is.null(curMod$dataDesign)) {
                    curPlSet$data <- NULL
                    curPlSet$design <- curMod$createSurveyObject()
                }

                ## Suppress the warnings produced by iNZightPlot ...
                suppressWarnings({
                    curPlot <<- unclass(rawpl <- do.call(iNZightPlot, curPlSet))
                    if (allow.redraw & !is.null(attr(curPlot, "dotplot.redraw")))
                        if (attr(curPlot, "dotplot.redraw"))
                            curPlot <<- unclass(rawpl <- do.call(iNZightPlot, curPlSet))
                })
                plotType <<- attr(curPlot, "plottype")
            } else {
                rawpl <- iNZightPlots:::resetPlot()
                plotType <<- "none"
            }
            invisible(rawpl)
        },
        ## set a new iNZDocument and make it the active one
        setDocument = function(document) {
            ## reset control widget
            # state <- ctrlWidget$getState()
            ctrlWidget$resetWidget()
            ## add a iNZDocument to the end of the doc list
            iNZDocuments <<- c(iNZDocuments, list(document))
            ## clean up any 'empty' datasets ..
            iNZDocuments <<- iNZDocuments[sapply(iNZDocuments, function(d) !all(dim(d$dataModel$dataSet) == 1))]
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
            ## if plotSettings change, update the plot
            getActiveDoc()$addSettingsObserver(function() updatePlot())
            # ctrlWidget$setState(state)
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
        ## display warning message
        displayMsg = function(module, type) {
            if (type == 1) {
                gmessage(msg = paste("A dataset is required to use the", module, "module"),
                         title = "Empty data", icon = "error")
            } else if (type == 2) {
                gmessage(msg = paste("Imported dataset is not appropriate for", module, "module"),
                         title = "Inappropriate data type", icon = "error")
            }
        },
        ## create a gvbox object into the module window (ie, initialize it)
        ## NOTE: should be run every time when a new module is open
        initializeModuleWindow = function(mod) {
            ## delete any old ones:
            if (length(.self$leftMain$children) > 1) {
                delete(.self$leftMain, .self$leftMain$children[[2]])
            }
            ## create a gvbox in moduleWindow
            moduleWindow <<- gvbox(container = leftMain, expand = TRUE)
            visible(gp1) <<- FALSE

            if (!missing(mod))
                activeModule <<- mod
        },
        initializeCodeHistory = function() {
            rhistory <<- iNZcodeWidget$new(.self)

            addActDocObs(
                function() {
                    rhistory$update()
                })
            getActiveDoc()$addDataObserver(
                function() {
                    rhistory$update()
                })
        },
        ## --- PREFERENCES SETTINGS and LOCATIONS etc ...
        defaultPrefs = function() {
            ## The default iNZight settings:
            list(track = "ask", track.id = NULL,
                 check.updates = TRUE,
                 window.size = c(1250, 850),
                 popout = FALSE,
                 font.size = 10)
        },
        checkPrefs = function(prefs) {
            allowed.names <- c("track", "track.id", "check.updates", 
                               "window.size", "popout", "font.size")

            ## Only keep allowed preferences --- anything else is discarded
            prefs <- prefs[names(prefs) %in% allowed.names]
            defs <- defaultPrefs()

            ## TRACK = TRUE | FALSE | "ask"
            prefs$track <-
                if (is.null(prefs$track)) defs$track
                else if (!is.na(prefs$track) & (prefs$track == "ask" | is.logical(prefs$track))) prefs$track
                else defs$track


            ## check.updates = TRUE | FALSE
            prefs$check.updates <-
                if (is.null(prefs$check.updates)) defs$check.updates
                else if (!is.na(prefs$check.updates) & is.logical(prefs$check.updates)) prefs$check.updates
                else defs$check.updates

            ## window.size = c(WIDTH, HEIGHT)
            prefs$window.size <-
                if (is.null(prefs$window.size)) defs$window.size
                else if (length(prefs$window.size) != 2) defs$window.size
                else if (is.numeric(prefs$window.size)) prefs$window.size
                else defs$window.size

            ## pop-out layout = FALSE
            prefs$popout <-
                if (is.null(prefs$popout)) defs$popout
                else if (is.logical(prefs$popout)) prefs$popout
                else defs$popout

            ## font size
            prefs$font.size <-
                if (is.null(prefs$font.size) || !is.numeric(prefs$font.size)) defs$font.size
                else prefs$font.size

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

            prefs.location <<-
                switch(OS,
                       "windows" = {
                           if (file.exists(file.path("~", "iNZightVIT"))) {
                               path <- file.path("~", "iNZightVIT", ".inzight")
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
                       })

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
                gmessage(paste("iNZight was unable to save your preferences, so",
                               "they won't carry over into the next session."),
                         title = "Unable to save preferences", icon = "warning")
            }
        },
        plotSplashScreen = function() {
            if (requireNamespace("png", quietly = TRUE)) {
                img <- png::readPNG(system.file("images/inzight_splash.png", package = "iNZight"))
                grid::grid.newpage()
                grid::pushViewport(grid::viewport())
                grid::grid.raster(img)

                grDevices::dev.flush()
            }
        },
        showHistory = function() {
            wh <- gwindow("R Code History", parent = .self$win,
                          width = 800, height = 500)
            gh <- gvbox(container = wh)
            th <- gtext(container = gh, expand = TRUE, fill = TRUE, wrap = FALSE)
            insert(th, rhistory$get(), font.attr = list(family = "monospace"))
        })
    )
