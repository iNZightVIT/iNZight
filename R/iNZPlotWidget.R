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
                              "TIFF (.tiff)" = tiff)
            
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

            addSpace(g, 10)
            glabel("Developmental - only working for base plots.\nDoesn't check for existing file.", container = g)
            
            addSpring(g)
            btnGrp <- ggroup(container = g)
            
            addSpring(btnGrp)
            cnclBtn <- gbutton("Cancel", handler = function(h, ...) dispose(w), container = btnGrp, expand = TRUE)
            saveBtn <- gbutton("Save", container = btnGrp, expand = TRUE,
                               handler = function(h, ...) {
                                   if (svalue(fName) == "") {
                                       gmessage("No file name", icon = "error", parent = w)
                                       return()
                                   }
                                   
                                   f <- file.path(svalue(fLoc), paste0(svalue(fName), svalue(fExt)))
                                   dim <- dev.size("px")
                                   
                                   filetypes[[svalue(fileType)]](file = f,
                                                                 width = dim[1],
                                                                 height = dim[2])
                                   fun()
                                   dev.off()

                                   dispose(w)
                               })

            addHandlerChanged(fileType, function(h, ...) {
                                  svalue(fExt) <- gsub(".+\\(|\\)", "", svalue(fileType))
                              })
        })
    )
