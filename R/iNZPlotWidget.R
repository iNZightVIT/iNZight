## ---------------------------
## A class that handles the display of
## plots in a gnotebook and stores information
## about the tabs(plots
## ---------------------------

iNZPlotWidget <- setRefClass(
    "iNZPlotWidget",
    fields = list(
        GUI = "ANY",
        plotNb = "ANY",
        tabDevLink = "numeric" # vector with the link between device and nb nr.
        ),
    methods = list(
        initialize = function(gui) {
            initFields(GUI = gui,
                       tabDevLink = numeric(0))
            plotNb <<- gnotebook(expand = TRUE)
        },
        addPlot = function() {
            add(plotNb, ggraphics(expand = TRUE), label = "plot",
                close.button = FALSE)
            tabDevLink <<- c(tabDevLink, dev.cur())
        },
        closePlot = function() {
            if(length(plotNb$children) > 1) {
                tabDevLink <<- tabDevLink[-svalue(plotNb)]
                dispose(plotNb)
            }
        },
        renamePlot = function() {
            input <- ginput("Name Plot as:",
                            text=names(plotNb)[svalue(plotNb)],
                            title="Input",
                            icon = "question",
                            parent = GUI$win
                            )
            if (length(input) > 0)
                names(plotNb)[svalue(plotNb)] <<- input
        },
        savePlot = function(fun = GUI$updatePlot) {
            ## iNZSaveWin$new(GUI, type = "plot",
            ##                which = tabDevLink[svalue(plotNb)]
            ##                )

            w <- gwindow("Save plot", parent = GUI$win, width = 500, height = 250)
            g <- gvbox(spacing = 15, container = w)

            g$set_borderwidth(15)

            tbl <- glayout()
            ii <- 1

            lbl <- glabel("File type :")
            font(lbl) <- list(weight = "bold")
            filetypes <- list("JPEG (.jpg)" = jpeg,
                              "PNG (.png)" = png,
                              "Bitmap (.bmp)" = bmp,
                              "TIFF (.tiff)" = tiff,
                              "PDF (.pdf)" = pdf,
                              "SVG (.svg)" = iNZightPlots:::exportSVG.function,
                              "Interactive HTML (.html)" = iNZightPlots:::exportHTML.function)

            fileType <- gcombobox(names(filetypes))
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:6, expand = TRUE] <- fileType
            ii <- ii + 1


            lbl <- glabel("Save location :")
            font(lbl) <- list(weight = "bold")
            initial.dir <- switch(GUI$OS,
                                  "windows" = {
                                      if (file.exists(file.path(path.expand("~"), "iNZightVIT", "Saved Plots")))
                                          f <- file.path(path.expand("~"), "iNZightVIT", "Saved Plots")
                                      else
                                          f <- getwd()
                                      f
                                  },
                                  "mac" = ,
                                  "linux" = {
                                      if (file.exists(file.path(path.expand("~"), "Documents", "iNZightVIT", "Saved Plots")))
                                          f <- file.path(path.expand("~"), "Documents", "iNZightVIT", "Saved Plots")
                                      else
                                          f <- getwd()
                                      f
                                  })
            fLoc <- gedit(initial.dir, editable = TRUE)
            fBrowse <- gbutton("Browse", handler = function(h, ...) {
                                   ff <- gfile("Select save location ...", type = "selectdir",
                                               initial.dir = svalue(fLoc))
                                   if (length(ff) == 1)
                                       svalue(fLoc) <- ff
                               })
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- fLoc
            tbl[ii, 5:6, expand = TRUE] <- fBrowse
            ii <- ii + 1


            lbl <- glabel("File name :")
            font(lbl) <- list(weight = "bold")
            fName <- gedit("")
            fExt <- glabel(gsub(".+\\(|\\)", "", svalue(fileType)))
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:4, expand = TRUE] <- fName
            tbl[ii, 5, anchor = c(-1, 0), expand = TRUE] <- fExt
            ii <- ii + 1

            add(g, tbl)

            addSpace(g, 5)

            ## Additional options for various components (scatter plots)
            gHTML <- gvbox(container = g)
            visible(gHTML) <- FALSE
            if (GUI$plotType %in% c("dot", "scatter", "terrain", "terrain-background", "toner-lite", "toner")) {
                labHTML <- glabel("Select additional variables to export", container = gHTML)
                font(labHTML) <- list(size = 12, weight = "bold")
                
                tabHTML <- gtable(data.frame(Variable = colnames(GUI$getActiveData())),
                          container = gHTML, multiple = TRUE)
                size(tabHTML) <- c(-1, 160)
                subHTML <- glabel("Hold CTRL to select multiple", container = gHTML)
                font(subHTML) <- list(size = 9)
                addHandlerChanged(fileType, function(h, ...) {
                    visible(gHTML) <- grepl("html", svalue(h$obj))
                })
            }

            
            addSpace(g, 5)
            glabel("Developmental - only working for base plots.\nDoesn't check for existing file.", container = g)

            addSpring(g)
            btnGrp <- ggroup(container = g)

            e <- environment()

            addSpring(btnGrp)
            cnclBtn <- gbutton("Cancel", handler = function(h, ...) dispose(w), container = btnGrp, expand = TRUE)
            saveBtn <- gbutton("Save", container = btnGrp, expand = TRUE,
                               handler = function(h, ...) {
                                   if (svalue(fName) == "") {
                                       gmessage("No file name", icon = "error", parent = w)
                                       return()
                                   }

                                   f <- file.path(svalue(fLoc), paste0(svalue(fName), svalue(fExt)))

                                   if (grepl("html|svg", svalue(fileType))) {
                                       ## exportXXX will produce a warning if the required packages aren't installed.
                                       ## If that is the case, we need to catch the error and ask the user
                                       ## if they'd like to install the packages.
                                       fp <- ""
                                       tryCatch({
                                           if (visible(gHTML) && length(svalue(tabHTML)) > 0) {
                                               dat <- GUI$getActiveData()
                                               vars <- as.character(svalue(tabHTML))
                                           } else {
                                               dat <- NULL
                                               vars <- NULL
                                           }
                                         
                                           plot.settings <- GUI$getActiveDoc()$getSettings()
                                           
                                           if (isTRUE(length(plot.settings$locate.id) > 0)) {
                                             print(plot.settings$locate.settings)
                                             if (isTRUE(plot.settings$locate.settings$txtVar != "id")) {
                                               dat <- GUI$getActiveData()
                                               vars <- c(vars, plot.settings$locate.settings$txtVar)
                                             }
                                             
                                             if (isTRUE(plot.settings$locate.settings$matchChk)) {
                                               dat <- GUI$getActiveData()
                                               vars <- c(vars, plot.settings$locate.settings$matchVar)
                                             }
                                           }
                                         
                                           fp <- filetypes[[svalue(fileType)]](fun, f, data = dat, extra.vars = vars)
                                       },
                                       error = function(e) {
                                           if (grepl("Required packages aren't installed", e$message)) {
                                               ## Ask use if they want to install:
                                               conf <- gconfirm(paste("To export HTML and SVG, you need to install a few additional packages.",
                                                                      "Would you like to do that now?", sep = "\n\n"),
                                                                title = "Install dependencies?",
                                                                parent = GUI$win)
                                               if (conf) {
                                                   ## Display confirmation message while packages are installed
                                                   w <- gbasicdialog("Installing packages", do.buttons = FALSE, container = GUI$win)
                                                   gg <- gvbox(container = w)
                                                   addSpace(gg, 10)
                                                   ggg <- ggroup(spacing = 15, container = gg)
                                                   addSpace(ggg, 0)
                                                   gimage(stock.id = "gtk-info", size="dialog", cont=ggg)
                                                   glabel("Please wait while packages are installed...", container = ggg,
                                                          anchor = c(-1, 1))
                                                   addSpace(ggg, 10)
                                                   addSpace(gg, 10)
                                                   install.packages("iNZightPlots", dependencies = TRUE)
                                                   dispose(w)
                                                   gmessage("Done! You can try saving as HTML or SVG again.",
                                                            title = "Installing packages complete",
                                                            parent = GUI$win)
                                               }
                                           } else {
                                               gmessage(paste("There was an error trying to save the plot as HTML.",
                                                              "Try restarting iNZight, and if you continue to see this message, copy the contents of the R Console and send a copy to inzight_support@stat.auckland.ac.nz, along with an explanation of what you were trying to do.", sep = "\n\n"),
                                                        parent = GUI$win, icon = "error")
                                               print(e$message)
                                           }
                                       },
                                       finally = {
                                           ## I don't really know what to do here
                                       })
                                       
                                       if (fp == "") return()
                                       ## `fp` is of class `inzHTML` and has a print method that'll open it in a browser
                                       print(fp)
                                   } else {
                                       switch(svalue(fileType),
                                              "PDF (.pdf)" = {
                                                  dim <- dev.size("in")
                                                  pdf(file = f,
                                                      width = dim[1],
                                                      height = dim[2],
                                                      useDingbats = FALSE,
                                                      onefile = FALSE)
                                              }, {
                                                  dim <- dev.size("px")
                                                  filetypes[[svalue(fileType)]](file = f,
                                                      width = dim[1],
                                                      height = dim[2])
                                              })
                                       fun()
                                       dev.off()
                                   }

                                   dispose(w)
                               })

            addHandlerChanged(fileType, function(h, ...) {
                                  svalue(fExt) <- gsub(".+\\(|\\)", "", svalue(fileType))
                              })
        })
    )
