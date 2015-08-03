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
                   menubar = "ANY",
                   
                   ## left group
                   leftMain = "ANY",
                   moduleWindow = "ANY",
                   gp1 = "ANY",
                   ## right group
                   gp2 = "ANY",
                   
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
                   preferences = "list"
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
            iNZDocuments <<- list(iNZDocument$new(data = data))
            win.title <- paste("iNZight (v",
                               packageDescription("iNZight")$Version,
                               ")", sep = "")

            ## We must set the correct directory if using a Mac
            if (is_MacOSX())
                try(setwd(Sys.getenv("R_DIR")), TRUE)
            
            ## Grab settings file (or try to!)
            getPreferences()
            
            ## Check for updates ... need to use try incase it fails (no connection etc)
            ## RCurl no longer supports R < 3, so it wont be available on Mac SL version.
            if ("RCurl" %in% row.names(installed.packages())) {
                connected <- RCurl::url.exists("docker.stat.auckland.ac.nz")
            } else connected <- FALSE
            
            if (connected) {
                if (preferences$track == "ask") {
                    preferences$track <<-
                        gconfirm("iNZight would like to use anonymous usage information. Are you ok for us to collect this information?",
                                 title = "Share usage information?", icon = "question")
                    savePreferences()
                }
                
                if (preferences$check.updates) {
                    ap <- suppressWarnings(try(numeric_version(available.packages(
                        contriburl = contrib.url("http://docker.stat.auckland.ac.nz/R",
                            getOption("pkgType")))[,"Version"]), TRUE))
                    if (!inherits(ap, "try-error")) {
                        if (length(ap) > 0) {
                            ip <- try(numeric_version(installed.packages()[names(ap), "Version"]), TRUE)
                            if (!inherits(ip, "try-error")) {
                                if (any(ap > ip))
                                    win.title <- paste(win.title, " [updates available]")
                            }
                        }
                    }
                }
                
                
                
                ## also want to be cheeky and add users to "database" of users so we can track...
                if (preferences$track) {
                    try({
                        version = packageVersion("iNZight")
                        os <- "Linux"
                        if (.Platform$OS == "windows") {
                            os = "Windows"
                        } else if (Sys.info()["sysname"] == "Darwin") { 
                            os = "Mac OS X"
                            osx.version <- try(system("sw_vers -productVersion", intern = TRUE), silent = TRUE)
                            if (!inherits(osx.version, "try-error")) {
                                os = paste("Mac OS X", osx.version)
                            }
                        }
                        
                        
                        ## have they updated before?
                        if (is.null(preferences$track.id)) {
                            ## compatibility mode ---
                            hash.id <- "new"
                            
                            if (os == "Windows") {
                                libp <- "prog_files"
                            } else if (os != "Linux") {
                                ## i.e., mac
                                libp <- "Library"
                            } else {
                                ## linux - save in library..
                                libp <- .libPaths()[which(sapply(.libPaths(), function(p)
                                                                 "iNZight" %in% list.files(p)))[1]]
                            }
                            
                            if (file.exists(file.path(libp, "id.txt"))) {
                                hash.id <- readLines(file.path(libp, "id.txt"))
                                unlink(file.path(libp, "id.txt"))  ## delete the old one
                            }
                            
                            ## only if not already tracking
                            if (hash.id == "new") {
                                track.url <- paste0("http://docker.stat.auckland.ac.nz/R/tracker/index.php?track&v=",
                                                    version, "&os=", gsub(" ", "%20", os), "&hash=", hash.id)
                                f <- try(url(track.url,  open = "r"), TRUE)
                                
                                ## write the hash code to their installation:
                                hash.id <- readLines(f)
                                
                                
                                ## try(writeLines(hash.id, file.path(libp, "id.txt")), silent = TRUE)
                            }

                            preferences$track.id <<- hash.id
                            savePreferences()
                        } else {
                            hash.id <- preferences$track.id
                            try(url(paste0("http://docker.stat.auckland.ac.nz/R/tracker/index.php?track&v=",
                                           version, "&os=", gsub(" ", "%20", os), "&hash=", hash.id), open = "r"), TRUE)
                        }
                    })
                }
            }
                
            win <<- gwindow(win.title, visible = FALSE, 
                            width = preferences$window.size[1], height = preferences$window.size[2])
            
            gtop <- ggroup(horizontal = FALSE, container = win,
                           use.scrollwindow = TRUE)
            menugrp <- ggroup(container = gtop)
            initializeMenu(menugrp, disposeR)
            g <- gpanedgroup(container = gtop, expand = TRUE)
            
            ## Left side group
            leftMain <<- ggroup(container = g)
            size(leftMain) <<- c(300, -1)
            
            gp1 <<- gvbox(container = leftMain,
                           expand = TRUE)
            
            ## Right side group
            gp2 <<- ggroup(horizontal = FALSE, container = g, expand = F)
            ## set up widgets in the left group
            ## set up the menu bar at the top
            # initializeMenu(gp1, disposeR)
            ## set up dataViewWidget, added below
            ## dataThreshold is used as maximum nr of cells
            ## before data.frame view gets deactivated
            dataThreshold <- 200000
            initializeDataView(dataThreshold)
            ## set up buttons to switch between data/var view
            add(gp1, .self$initializeViewSwitcher(dataThreshold)$viewGroup)
            ## display the name of the data set
            add(gp1, .self$initializeDataNameWidget()$nameLabel)
            ## display the data
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
            
#            add(g, leftMain)
#            add(g, gp2)
        },
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
                      setDocument(iNZDocument$new(data = getActiveDoc()$getModel()$origDataSet))
                        #getActiveDoc()$getModel()$updateData(
                        #    getActiveDoc()$getModel()$origDataSet)
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
                    label = "Time Series...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        ## module = "iNZightTS"
                        ## setup  = modSetup(module)
                        ## if (setup) {
                        ##     if (emptyData()) {
                        ##         ## if there is no imported 
                        ##         ## dataset, display a gmessage
                        ##         displayMsg("time series")
                        ##         return()
                        ##     }
                        ##     if (length(leftMain$children) == 1) {
                        ##         initializeModuleWindow()
                        ##         source(paste0("../Modules/", module, ".R"))
                        ##         iNZightTimeSeries$new(.self)
                        ##         visible(moduleWindow) <<- TRUE
                        ##     } else { return() }
                        ## } else {
                        ##     return()
                        ## }

                        ign <- gwindow("...", visible = FALSE)
                        tag(ign, "dataSet") <- getActiveData()
                        e <- list(obj = ign)
                        e$win <- win
                        timeSeries(e)
                    }
                    ),
                modelFit = gaction(
                  #18
                    label = "Model Fitting...",
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
                    label = "3D Plot...",
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
                modelFit = gaction(
                    ## 32
                    label = "Multiple Response...",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        ign <- gwindow("...", visible = FALSE)
                        tag(ign, "dataSet") <- getActiveData()
                        e <- list(obj = ign)
                        e$win <- win
                        multipleResponseWindow(e)
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
                        verlbl <- glabel(paste("Version", packageDescription("iNZight")$Version), container = g)
                        font(verlbl) <- list(weight = "normal", family = "normal", size = 10)
                        addSpace(g, 10)
                        copylbl <- glabel("Copyright (C) 2014 University of Auckland", container = g)
                        font(copylbl) <- list(weight = "normal", family = "normal", size = 8)
                        addSpace(g, 15)
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
                guideManipulateVariables = gaction(
                    ## 42
                    label = "Manipulate Variables Menu",
                    icon = "symbol_diamond",
                    handler = function(h, ...) {
                        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/manipulate_variables/")
                    }
                ),
                guideDataOptions = gaction(
                    ## 43
                    label = "Data Menu",
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
                        module = "iNZightMaps"
                        setup  = modSetup(module)
                        if (setup) {
                            ## if there is no imported dataset,
                            ## display a warning message
                            if (emptyData()) {
                                displayMsg("maps")
                                return()
                            }
                            ## if there is a module open, initialize and open
                            ## a module window
                            if (length(leftMain$children) == 1) {
                                initializeModuleWindow()
                                #source(paste0("../Modules/", module, ".R"))
                                iNZightMaps$new(.self)
                                visible(moduleWindow) <<- TRUE
                            } else { return() }
                        } else {
                            return()
                        }
                    }
                )
                #####################################################
                ###  big suggestion
                ###  any new update function should be placing below to match the actionList[[number]]
                ###  so next one should be #31 and placing below.
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
                File = actionList[c(16, 1:2, 36, 37)],
                "Dataset" = list(
                    actionList[[13]],
                    actionList[[27]],
                    actionList[[28]],
                    actionList[[31]],
                    actionList[[15]],
                    gseparator(),
                    actionList[[45]],
                    actionList[[46]]
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
                    #gaction(label = "",
                    #        icon = "diamond",
                    #        handler = function(h, ...) addtoPlot())
                    ),
                "Advanced" = list(
                    "Quick Explore" = actionList[c(24, 22, 23, 26, 20)],
                    actionList[[19]],
                    actionList[[17]],
                    actionList[[18]],
                    actionList[[32]],
                    actionList[[47]]
                    ),
                "Help" = list(
                    actionList[[33]],
                    actionList[[38]],
                    "User Guides" = actionList[39:44],
                    actionList[[34]],
                    actionList[[35]]
                    )
                )
            menubar <<- gmenu(menuBarList, container = cont)

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
            ## addActDocObs(
            ##     function() {
            ##         ctrlWidget$updateVariables()       
            ##     }
            ## )
            ## ## if the dataSet changes, update the variable View
            ## getActiveDoc()$addDataObserver(
            ##     function() {
            ##         ctrlWidget$updateVariables()
            ##     }
            ## )

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

                        w <- gwindow("Summary", width = 850, height = 400,
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
            infBtn <<- gbutton(
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
                                list(bs.inference = (svalue(rd, index = TRUE) == 2),
                                     summary.type = "inference",
                                     inference.type = "conf",
                                     inference.par = NULL)
                                )

                            infType <- svalue(rd, index = TRUE)
                            dispose(w)

                            infTitle <- "Inference Information"
                            if (infType == 2) {
                                ## Not sure why this acts weird. At least on Linux, the text inside `wBoots` doesn't becoem visible until the
                                ## function has finished.
                                wBoots <- gwindow("Please wait while iNZight performs bootstrap simulations ...", visible = FALSE,
                                                  parent = win, width=850, height=400)
                                gBoots <- gtext("Currently performing bootstrap simulations.\nDepending on the size of your data, this may take a while.",
                                                cont = wBoots, expand = TRUE,
                                                font.attr = list(family = "monospace"))
                                visible(wBoots) <- TRUE
                            }
                            
                            w2 <- gwindow(infTitle, width = 850, height = 400,
                                          visible = FALSE, parent = win)
                            g2 <- gtext(
                                paste(
                                    do.call(
                                        iNZightPlots:::getPlotSummary,
                                        sets),
                                    collapse = "\n"),
                                expand = TRUE, cont = w2, wrap = FALSE,
                                font.attr = list(family = "monospace"))
                            visible(w2) <- TRUE
                            try(dispose(wBoots), silent = TRUE)
                        }, cont = g)

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
                ## Design or data?
                curMod <- getActiveDoc()$getModel()
                if (!is.null(curMod$dataDesign)) {
                    curPlSet$data <- NULL
                    curPlSet$design <- curMod$createSurveyObject()
                }
                
                ## Suppress the warnings produced by iNZightPlot ...
                suppressWarnings({
                    curPlot <<- unclass(do.call(iNZightPlot, curPlSet))
                })
                plotType <<- attr(curPlot, "plottype")
            } else {
                iNZightPlots:::resetPlot()
                plotType <<- "none"
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
        },
        
        ## check for any imported data
        emptyData = function() {
            vars = names(.self$getActiveData())
            if(length(vars) == 1 && vars == "empty") {
                return(TRUE)
            } else {
                return(FALSE)
            }
        },
        
        ## display warning message
        displayMsg = function(label) {
            gmessage(msg = paste("A dataset is required to use the",
                                 label, "module"),
                     title = "No data", icon = "error")
        },
        
        ## module setup
        modSetup = function(mod) {
            if (mod %in% rownames(installed.packages())) {
                require(mod, character.only = TRUE)
            } else {
                install = gconfirm("The module is not found. Would you like to download it?")
                if (install) {
                    install.packages(mod, repo = "http://docker.stat.auckland.ac.nz/R")
                    require(mod, character.only = TRUE)
                }
                return(install)
            }
        },
        
        ## create a gvbox object into the module window (ie, initialize it)
        ## NOTE: should be run every time when a new module is open
        initializeModuleWindow = function() {
            ## create a gvbox in moduleWindow
            moduleWindow <<- gvbox(container = leftMain, expand = TRUE)
            visible(gp1) <<- FALSE
        },
        defaultPrefs = function() {
            ## The default iNZight settings:
            list(track = "ask", track.id = NULL,
                 check.updates = TRUE,
                 window.size = c(870, 600))
        },
        checkPrefs = function(prefs) {
            allowed.names <- c("track", "track.id", "check.updates", "window.size")

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


            prefs
            
        },
        getPreferences = function() {
            tt <- try({
                preferences <<-
                    if (".inzight" %in% list.files(all.files = TRUE)) {
                        checkPrefs(dget(".inzight"))
                    } else if (".inzight" %in% list.files("~", all.files = TRUE)) {
                        checkPrefs(dget("~/.inzight"))
                    } else {
                        defaultPrefs()
                    }
            }, TRUE)
            
            if (inherits(tt, "try-error"))
                preferences <<- defaultPrefs()
        },
        savePreferences = function() {
            if (".inzight" %in% list.files(all.files = TRUE)) {
                dput(preferences, ".inzight")
            } else if (".inzight" %in% list.files("~", all.files = TRUE)) {
                dput(preferences, "~/.inzight")
            } else {
                conf <- gconfirm("iNZight will place a settings file in the current directory.",
                                 title = "Create preferences file?", icon = "question")
                if (conf) {D
                    tt <- try(dput(preferences, ".inzight"))
                    if (inherits(tt, "try-error"))
                        gmessage("iNZight was unable to save your preferences. They will be saved for the current session, but will not carry over to future sessions.",
                                 title = "Unable to save preferences")
                }
            }
        })
    )
