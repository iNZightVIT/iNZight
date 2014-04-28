## --------------------------------------------
## The super class for the plot modification window
## The different windows that are opened through the
## 'Add to Plot' button are subclasses of this superclass
## The window that is opened depends on the variables
## currently selected in the control widget (or in the iNZDocument,
## which is the same since the two are linked together)
## --------------------------------------------

iNZPlotModWin <- setRefClass(
    "iNZPlotModWin",
    fields = list(
        GUI = "ANY",
        modWin = "ANY",
        okButton = "ANY",
        ## grp that will hold the multiple choices for plot mods
        radioGrp = "ANY",
        ## depending on selection in radioGrp, options for mod
        ## will be displayed here
        optGrp = "ANY",
        curSet = "list" ## the current plot settings
        ),
    methods = list(
        initialize = function(gui=NULL) {
            initFields(GUI = gui)
            if (!is.null(GUI)) {
                updateSettings()
                modWin <<- gwindow(title = "Add to Plot",
                                   visible = TRUE,
                                   parent = GUI$win)
                mainGrp <- ggroup(horizontal = FALSE,
                                  container = modWin,
                                  expand = FALSE,
                                  fill = "y")
                mainGrp$set_borderwidth(15)
                topGrp <- ggroup(horizontal = TRUE,
                                 container = mainGrp,
                                 expand = FALSE)
                lbl <- glabel("I want to")
                font(lbl) <- list(weight="bold",
                                  family = "normal",
                                  size = 11)
                radioGrp <<- ggroup(horizontal = FALSE,
                                    expand = FALSE)
                btnGrp <- ggroup(horizontal = FALSE,
                                 expand = FALSE)
                addSpring(btnGrp)
                okButton <<- gbutton("Done", expand = FALSE,
                                     cont = btnGrp,
                                     handler = function(h, ...) dispose(modWin))
                addSpring(btnGrp)
                optGrp <<- ggroup(horizontal = FALSE, expand = TRUE)
                add(topGrp, lbl)
                add(topGrp, radioGrp)
                addSpring(topGrp)
                add(topGrp, btnGrp)
                add(mainGrp, optGrp)
                ##visible(modWin) <<- TRUE
            }
        },
        ## up the curSet class variable
        updateSettings = function() {
            curSet <<- GUI$getActiveDoc()$getSettings()
        })
    )


iNZDotchartMod <- setRefClass(
    "iNZDotchartMod",
    contains = "iNZPlotModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            ## need to specify the methods that we want to use in
            ## do.call later on (see changeOpts())
            usingMethods(opt1, opt2, opt3, opt4)
            opts <- gradio(c("Code more variables",
                             "Change plot appearance",
                             "Identify points",
                             "Customize Labels"),
                           selected = 1,
                           horizontal = FALSE)
            add(radioGrp, opts)
            opt1()
            addHandlerChanged(opts,
                              handler = function(h, ...) {
                                  changeOpts(svalue(h$obj,
                                                    index = TRUE))
                              })
        },
        changeOpts = function(index) {
            ## delete current displayed options
            invisible(sapply(optGrp$children, function(x) delete(optGrp, x)))
            do.call(paste("opt", index, sep=""),
                    args = list())
        },
        ## Following are the different views for the indices of the
        ## gradio
        opt1 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Code More Variables")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            lbl2 <- glabel("Colour by levels of :")
            grpVarList <- gcombobox(c("", names(GUI$getActiveData())),
                                    selected = ifelse(
                                        is.null(curSet$by),
                                        1, which(names(GUI$getActiveData()) ==
                                                 curSet$varnames$by)[1] + 1
                                        )
                                    )
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      GUI$getActiveDoc()$setSettings(
                                          list(by = GUI$getActiveData()[[
                                                   svalue(grpVarList)]],
                                               varnames = list(
                                                   by = svalue(grpVarList)))
                                          )
                                      updateSettings()
                                  })
            tbl[3, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[4, 1, anchor = c(-1, -1), expand = TRUE] <- lbl2
            tbl[4, 2, expand = TRUE] <- grpVarList
            tbl[5, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)
        },
        ## Change Plot appearance
        opt2 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Change plot appearance")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            lbl2 <- glabel("Colour of symbols :")
            lbl3 <- glabel("Background colour :")
            lbl4 <- glabel("Size of symbols  :")
            lbl5 <- glabel("Thickness of symbols :")
            lbl6 <- glabel("(Use drop down list or type in if desired color is unavailable)")
            lbl7 <- glabel("Transparency of symbols  :")
            lbl8 <- glabel("Plot Type :")
            font(lbl6) <- list(family = "normal",
                               size = 8)
            ## default settings
            defts <- iNZightPlots:::inzPlotDefaults()
            ## active settings
            pointCols <- c(defts$col.pt, "darkblue", "darkgreen",
                "darkmagenta", "darkslateblue", "hotpink4",
                "lightsalmon2", "palegreen3", "steelblue3")
            backgroundCols <- c(defts$bg, "antiquewhite",
                "azure3", "bisque", "cornsilk", "darkolivegreen2",
                "darkslategray1", "greenyellow", "lightblue1",
                "lightpink", "rosybrown1", "slategray1", "thistle1",
                "wheat1")
            plotTypes <- c("default", "dot plot", "histogram")
            ## the values used for `largesample` in the plot settings
            plotTypeValues <- list(NULL, FALSE, TRUE)
            symbolColList <- gcombobox(
                pointCols,
                selected = ifelse(
                    is.na(which(pointCols == curSet$col.pt)[1]),
                    1,
                    which(pointCols == curSet$col.pt)[1]),
                editable = TRUE)
            backgroundColList <- gcombobox(
                backgroundCols,
                selected = ifelse(
                    is.na(which(backgroundCols == curSet$bg)[1]),
                    1,
                    which(backgroundCols == curSet$bg)[1]),
                editable = TRUE)
            plotTypeList <- gcombobox(
                plotTypes,
                selected = ifelse(
                    is.null(curSet$largesample),
                    1,
                    ifelse(
                        curSet$largesample == FALSE,
                        2, 3))
                )
            fillColor <- gcheckbox("Colour symbol interior",
                                   checked = (curSet$pch != 1))
            cexSlider <- gslider(from = 0.05, to = 3.5,
                by = 0.05, value = curSet$cex.pt)
            transpSlider <- gslider(from = 0.01, to = 1,
                                    by = 0.01, value = curSet$alpha)
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      pch.sel <- ifelse(svalue(fillColor),
                                                        19, 1)
                                      GUI$getActiveDoc()$setSettings(
                                          list(col.pt = svalue(symbolColList),
                                               bg = svalue(backgroundColList),
                                               cex.pt = svalue(cexSlider),
                                               pch = pch.sel,
                                               alpha = svalue(transpSlider),
                                               largesample = plotTypeValues[[svalue(
                                                   plotTypeList, index = TRUE)]])
                                          )
                                      updateSettings()
                                  })
            tbl[3,2:4, anchor = c(-1,-1), expand = TRUE] <- lbl1
            tbl[4,2, anchor = c(-1,-1), expand = TRUE] <- lbl8
            tbl[4,3, expand = TRUE] <- plotTypeList
            tbl[5,2, anchor = c(-1,-1), expand = TRUE] <- lbl2
            tbl[5,3, expand = TRUE] <- symbolColList
            tbl[6,2:4] <- lbl6
            tbl[7,3, expand = TRUE] <- fillColor
            tbl[8,2, anchor = c(-1,-1), expand = TRUE] <- lbl3
            tbl[8,3, expand = TRUE] <- backgroundColList
            tbl[10,2, anchor = c(-1,-1), expand = TRUE] <- lbl4
            tbl[10,3, expand = TRUE] <- cexSlider
            tbl[11,2, anchor = c(-1,-1), expand = TRUE] <- lbl7
            tbl[11,3, expand = TRUE] <- transpSlider
            tbl[12, 2:4] <- showButton

            ## if the "by" options is set, i.e. points are colored
            ## according to another var, disable the option to
            ## change the color
            if (!is.null(GUI$getActiveDoc()$getSettings()$by)) {
                enabled(symbolColList) <- FALSE
                svalue(lbl6) <- paste(
                    "Changing the color of symbols is disabled since",
                    " the symbols are\n colored by variable '",
                    GUI$getActiveDoc()$getSettings()$varnames$by,
                    "'", sep = "")
            }
            add(optGrp, tbl)
        },
        opt3 = function() {
            ## Do checking first
            ## If g1 or g2 = _MULTI, then we can't identify points (yet ...)
            cantDo <- function(msg = "using subsetting variables.") {
                gmessage(paste("Cannot identify points when", msg),
                         icon = "error", title = "Unable to identify",
                         parent = modWin)
                dispose(GUI$modWin)
                return()
            }
            

            lbl1 <- "Select variable to identify:"
            font(lbl1) <- list(weight="bold", family = "normal")
            varmenu <- gcombobox(c("id", names(GUI$getActiveData())), selected = 1)

            
            lbl2 <- "Select points by: "
            selOpts <- gradio(c("By mouse click", "Min/Max n points"),
                              selected = 1, horizontal = FALSE)
            minPts <- gcheckbox("Minimum", checked = TRUE, )
            enabled(minPts) <- FALSE
            maxPts <- gcheckbox("Maximum", checked = TRUE)
            enabled(maxPts) <- FALSE
            nlbl <- "N ="
            nPts <- gedit("5", width = 3)
            enabled(nPts) <- FALSE

            
            locateButton <- gbutton("Locate",
                                    handler = function(h, ...) {
                                        
                                        x <- curSet$x  # used for removing missing values ...
                                        y <- curSet$y
                                        v <- svalue(varmenu)
                                    
                                        w <- rep(TRUE, length(x))
                                        if (!is.null(curSet$g1)) {
                                            if (is.null(curSet$g1.level)) {
                                                cantDo()
                                            } else if (curSet$g1.level == "_MULTI") {
                                                cantDo()
                                            }
                                            w[curSet$g1 != curSet$g1.level] <- FALSE
                                        }
                                        if (!is.null(curSet$g2)) {
                                            if (curSet$g2.level == "_MULTI") {
                                                cantDo()
                                            } else {
                                                if (curSet$g2.level != "_ALL")
                                                    w[curSet$g2 != curSet$g2.level] <- FALSE
                                            }
                                        }
                                        if (is.factor(y))
                                            cantDo("when Variable 2 is a factor.")
                                        
                                        if (is.null(v))
                                            v <- as.character(1:length(x))
                                        else {
                                            if (v == "id")
                                                v <- as.character(1:length(x))
                                            else {
                                                v <- as.character(GUI$getActiveData()[, v])
                                                v[is.na(v)] <- "missing"
                                            }
                                        }

                                       
                                        
                                        seekViewport("MAINVP")

                                        isNA <- is.na(x)
                                        if (!is.null(curSet$g1))
                                            isNA <- isNA | is.na(curSet$g1)
                                        if (!is.null(curSet$g2))
                                            isNA <- isNA | is.na(curSet$g2)

                                            
                                        dp <- grid.get("DOTPOINTS")
                                        # these are the points, but not in the correct order ...
                                        d <- data.frame(x = as.numeric(dp$x),
                                                        y = as.numeric(dp$y),
                                                        v = v[w & !isNA])
                                       
                                        d$v <- iNZightPlots:::makePoints(x[w & !isNA], v[w & !isNA])$col
                                        
                                        seekViewport("DOTPLOTVP")  # need correct coordinate system
                                                                               
                                        # FOR TESTING:
                                        mmPoints <- svalue(selOpts, index = TRUE) == 2
                                        if (mmPoints) {
                                            Npts <- as.numeric(svalue(nPts))
                                            if (is.na(Npts)) {
                                                gmessage("Please specify a numeric value for Number of Points.")
                                                return()
                                            }
                                            
                                            Wpts <- sum(svalue(minPts), 2 * svalue(maxPts))
                                            if (Wpts == 0) {
                                                gmessage("Please select either Minimum of Maximum points to identify.")
                                                return()
                                            }
                                            if (Wpts == 3) Wpts <- c(1, 2)
                                            
                                            if (any(Wpts == 1)) {
                                                o <- d[order(d$x, decreasing = FALSE), ][1:Npts, ]
                                                apply(o, 1, function(r) {
                                                    tt <- r[3]
                                                    or <- as.numeric(r[1:2])
                                                    grid.text(tt,
                                                              or[1] + convertWidth(unit(0.5, "char"), "native", TRUE),
                                                              or[2] + convertWidth(unit(0.5, "char"), "native", TRUE),
                                                              just = "left", rot = 45,
                                                              default.units = "native", gp = gpar(cex = 0.7))
                                                })
                                            }
                                            if (any(Wpts == 2)) {
                                                o <- d[order(d$x, decreasing = TRUE), ][1:Npts, ]
                                                apply(o, 1, function(r) {
                                                    tt <- r[3]
                                                    or <- as.numeric(r[1:2])
                                                    grid.text(tt,
                                                              or[1] + convertWidth(unit(0.5, "char"), "native", TRUE),
                                                              or[2] + convertWidth(unit(0.5, "char"), "native", TRUE),
                                                              just = "left", rot = 45,
                                                              default.units = "native", gp = gpar(cex = 0.7))
                                                })
                                            }
                                            
                                            
                                        } else {
                                            xy <- as.numeric(grid.locator())
                                            
                                            
                                            ## We only want to check X and Y for missing
                                            na <- apply(d[, 1:2], 1, function(x) any(is.na(x)))
                                            d <- d[!na, ]
                                            
                                            ## So now, d = data.frame with x, y, and the label
                                            ## Standardise it:
                                            x.s <- (d$x - min(d$x)) / (max(d$x) - min(d$x))
                                            y.s <- (d$y - min(d$y)) / (max(d$y) - min(d$y))
                                            
                                            xy.s <- numeric(2)
                                            xy.s[1] <- (xy[1] - min(d$x)) / (max(d$x) - min(d$x))
                                            xy.s[2] <- (xy[2] - min(d$y)) / (max(d$y) - min(d$y))
                                            
                                            o <- d[which.min((x.s - xy.s[1])^2 + (y.s - xy.s[2])^2), ]
                                            
                                            grid.text(o$v,
                                                      o$x + convertWidth(unit(0.5, "char"), "native", TRUE),
                                                      o$y + convertWidth(unit(0.5, "char"), "native", TRUE),
                                                      just = "left", rot = 45,
                                                      default.units = "native", gp = gpar(cex = 0.7))
                                        }
                                        
                                    })

            tbl1 <- glayout()
            tbl2 <- glayout()
            tbl3 <- glayout()
            tbl1[1, 1:2, expand = TRUE, anchor = c(-1, 0)] <- lbl1
            tbl1[2, 1:2, expand = TRUE, anchor = c(1, 0)] <- varmenu
            tbl1[3, 1, expand = FALSE, anchor = c(1, 1)] <- lbl2
            tbl1[3, 2] <- selOpts

            tbl2[1, 1] <- minPts
            tbl2[1, 2] <- maxPts
            tbl2[1, 3] <- nlbl
            tbl2[1, 4] <- nPts
            
            tbl3[2, 2, expand = FALSE, anchor = c(1, 0)] <- locateButton
            
            add(optGrp, tbl1)
            add(optGrp, tbl2)
            add(optGrp, tbl3)

            ## Some things to change values ...
            addHandlerChanged(selOpts, handler = function(h, ...) {
                if (svalue(selOpts, index = TRUE) == 1) {
                    enabled(minPts) <- FALSE
                    enabled(maxPts) <- FALSE
                    enabled(nPts) <- FALSE
                } else {
                    enabled(minPts) <- TRUE
                    enabled(maxPts) <- TRUE
                    enabled(nPts) <- TRUE
                }
            })
        },
        opt4 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Customize Labels")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)

            curPlSet <- GUI$getActiveDoc()$getSettings()
            oldMain <- curPlSet$main
            oldX <- curPlSet$xlab
            if (is.null(oldMain)) oldMain <- ''
            if (is.null(oldX)) oldX <- ''

            lbl2    <- glabel("Main title :")
            labMain <- gedit(oldMain)
            lbl3    <- glabel("x-axis label :")
            labX    <- gedit(oldX)

            lbl4 <- glabel("(Enter a single space to print no title)")
            font(lbl4) <- list(family = "normal",
                               size = 8)

            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      mlab <- svalue(labMain)
                                      xlab <- svalue(labX)
                                      GUI$getActiveDoc()$setSettings(
                                          list(main = if (mlab != '') mlab else NULL,
                                               xlab = if (xlab != '') xlab else NULL)
                                          )
                                      updateSettings()
                                  })
            tbl[3, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[4, 1, anchor = c(-1, -1), expand = TRUE] <- lbl2
            tbl[4, 2, expand = TRUE] <- labMain
            tbl[5, 1, anchor = c(-1, -1), expand = TRUE] <- lbl3
            tbl[5, 2, expand = TRUE] <- labX
            tbl[6, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl4
            tbl[7, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)
        })
    )

iNZBarchartMod <- setRefClass(
    "iNZBarchartMod",
    contains = "iNZPlotModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            ## need to specify the methods that we want to use in
            ## do.call later on (see changeOpts())
            usingMethods(opt1, opt2)
            opts <- gradio(c("Code more variables",
                             "Customize Labels"),
                           selected = 1,
                           horizontal = FALSE)
            add(radioGrp, opts)
            opt1()
            addHandlerChanged(opts,
                              handler = function(h, ...) {
                                  changeOpts(svalue(h$obj,
                                                    index = TRUE))
                              })
        },
        changeOpts = function(index) {
            ## delete current displayed options
            invisible(sapply(optGrp$children, function(x) delete(optGrp, x)))
            do.call(paste("opt", index, sep=""),
                    args = list())
        },
        ## Following are the different views for the indices of the
        ## gradio
        opt1 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Colour by levels of :")
            grpVarList <- gcombobox(c("", names(GUI$getActiveData())))
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      GUI$getActiveDoc()$setSettings(
                                          list(by = GUI$getActiveData()[[
                                                   svalue(grpVarList)]],
                                               varnames = list(
                                                   by = svalue(grpVarList)))
                                          )
                                      updateSettings()
                                  })
            tbl[1, 1] <- lbl1
            tbl[1, 2] <- grpVarList
            tbl[2, 1:3, expand = TRUE] <- showButton
            add(optGrp, tbl)
        },
        opt2 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Customize Labels")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)

            curPlSet <- GUI$getActiveDoc()$getSettings()
            oldMain <- curPlSet$main
            oldX <- curPlSet$xlab
            if (is.null(oldMain)) oldMain <- ''
            if (is.null(oldX)) oldX <- ''

            lbl2    <- glabel("Main title :")
            labMain <- gedit(oldMain)
            lbl3    <- glabel("x-axis label :")
            labX    <- gedit(oldX)

            lbl4 <- glabel("(Enter a single space to print no title)")
            font(lbl4) <- list(family = "normal",
                               size = 8)

            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      mlab <- svalue(labMain)
                                      xlab <- svalue(labX)
                                      GUI$getActiveDoc()$setSettings(
                                          list(main = if (mlab != '') mlab else NULL,
                                               xlab = if (xlab != '') xlab else NULL)
                                          )
                                      updateSettings()
                                  })
            tbl[3, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[4, 1, anchor = c(-1, -1), expand = TRUE] <- lbl2
            tbl[4, 2, expand = TRUE] <- labMain
            tbl[5, 1, anchor = c(-1, -1), expand = TRUE] <- lbl3
            tbl[5, 2, expand = TRUE] <- labX
            tbl[6, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl4
            tbl[7, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)
        })
    )


iNZScatterMod <- setRefClass(
    "iNZScatterMod",
    contains = "iNZPlotModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            ## need to specify the methods that we want to use in
            ## do.call later on (see changeOpts())
            usingMethods(opt1, opt2, opt3, opt4, opt5, opt6, opt7, opt8, opt9)
            opts <- gradio(c("Code more variables",
                             "Add trend curves",
                             "Add x=y line",
                             "Add a jitter",
                             "Add rugs",
                             "Join points by lines",
                             "Change plot appearance",
                             "Identify points",
                             "Customize Labels"),
                           selected = 1,
                           horizontal = FALSE)
            add(radioGrp, opts)
            opt1()
            addHandlerChanged(opts,
                              handler = function(h, ...) {
                                  changeOpts(svalue(h$obj,
                                                    index = TRUE))
                              })
        },
        changeOpts = function(index) {
            ## delete current displayed options
            invisible(sapply(optGrp$children, function(x) delete(optGrp, x)))
            do.call(paste("opt", index, sep=""),
                    args = list())
        },
        ## Following are the different views for the indices of the
        ## gradio
        ## Code more variables
        opt1 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Code More Variables")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            lbl2 <- glabel("Colour by levels of:")
            grpVarList <- gcombobox(c("", names(GUI$getActiveData())),
                                    selected = ifelse(
                                        is.null(curSet$by),
                                        1, which(names(GUI$getActiveData()) ==
                                                 curSet$varnames$by)[1] + 1
                                        )
                                    )
            lbl3 <- glabel("Resize points proportional to:")
            rszVarList <- gcombobox(
                c("",
                  names(GUI$getActiveData())[sapply(GUI$getActiveData(),
                                                    is.numeric)])
                )
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      ## update plot settings
                                      GUI$getActiveDoc()$setSettings(
                                          list(by = GUI$getActiveData()[[
                                                   svalue(grpVarList)]],
                                               prop.size = GUI$getActiveData()[[
                                                   svalue(rszVarList)]],
                                               varnames = list(
                                                   by = svalue(grpVarList),
                                                   prop.size = svalue(rszVarList)))
                                          )
                                      updateSettings()
                                  })
            tbl[3, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[4, 1, anchor = c(-1, -1), expand = TRUE] <- lbl2
            tbl[4, 2, expand = TRUE] <- grpVarList
            tbl[5, 1, anchor = c(-1, -1), expand = TRUE] <- lbl3
            tbl[5, 2, expand = TRUE] <- rszVarList
            tbl[6, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)
        },
        ## Add trend curves
        opt2 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Add trend curves")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            ## vector of possible trend curves
            trCrvs <- c("linear", "quadratic", "cubic")
            trCols <- c("red", "black", "blue", "green4",
                        "yellow", "pink", "grey", "orange")
            linChk <- gcheckbox(trCrvs[1],
                                checked = trCrvs[1] %in% curSet$trend)
            quaChk <- gcheckbox(trCrvs[2],
                                checked = trCrvs[2] %in% curSet$trend)
            cubChk <- gcheckbox(trCrvs[3],
                                checked = trCrvs[3] %in% curSet$trend)
            smthChk <- gcheckbox("Draw a smoother",
                                 checked = curSet$smooth!=0)
            quantSmthChk <- gcheckbox("Use Quantiles",
                                      checked = !is.null(curSet$quant.smooth))
            trendByChk <- gcheckbox(paste("For each level of",
                                            curSet$varnames$by),
                                      checked = curSet$trend.by)
            linCol <- gcombobox(trCols,
                                selected = which(
                                    curSet$col.trend$linear == trCols
                                    )
                                )
            quaCol <- gcombobox(trCols,
                                selected = which(
                                    curSet$col.trend$quadratic == trCols
                                    )
                                )
            cubCol <- gcombobox(trCols,
                                selected = which(
                                    curSet$col.trend$cubic == trCols
                                    )
                                )
            smthCols <- c("red", "black", "blue", "green", "yellow",
                          "magenta", "grey", "orange")
            smthCol <- gcombobox(smthCols,
                                 selected = which(
                                     curSet$col.smooth == smthCols)
                                 )
            smthSlid <- gslider(from = 0.1, to = 1,
                                by = 0.1,
                                value = ifelse(curSet$smooth==0,
                                    0.7, curSet$smooth))
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      ## vector of selected trends
                                      trSel <- c(svalue(linChk),
                                                 svalue(quaChk),
                                                 svalue(cubChk))
                                      ## vector of colors chosen
                                      trCol <- c(svalue(linCol),
                                                 svalue(quaCol),
                                                 svalue(cubCol))
                                      ## smoother option
                                      qsmth <- if (svalue(quantSmthChk)) "default"
                                               else NULL
                                      smth <- ifelse(svalue(smthChk) & is.null(qsmth),
                                                     svalue(smthSlid),
                                                     0)
                                      ## update plot settings
                                      GUI$getActiveDoc()$setSettings(
                                          list(trend = trCrvs[trSel],
                                               smooth = smth,
                                               quant.smooth = qsmth,
                                               col.trend = list(
                                                   linear = trCol[1],
                                                   quadratic = trCol[2],
                                                   cubic = trCol[3]),
                                               col.smooth = svalue(smthCol),
                                               trend.by = svalue(trendByChk)
                                               )
                                          )
                                      updateSettings()
                                  })
            ## only have the smoother slider enabled if the
            ## smoother checkbox is ticked
            if (!svalue(smthChk)) {
                enabled(smthSlid) <- FALSE
                enabled(quantSmthChk) <- FALSE
            }

            addHandlerChanged(smthChk, handler = function(h, ...) {
                if (svalue(smthChk)) {
                    if (!svalue(quantSmthChk))
                        enabled(smthSlid) <- TRUE
                    else
                        enabled(smthSlid) <- FALSE

                    enabled(quantSmthChk) <- TRUE
                } else {
                    enabled(smthSlid) <- FALSE
                    enabled(quantSmthChk) <- FALSE
                }
            })
            ## if quantiles are used, disable slider
            addHandlerChanged(quantSmthChk,
                              handler = function(h, ...) {
                                  if (svalue(quantSmthChk))
                                      enabled(smthSlid) <- FALSE
                                  else
                                      enabled(smthSlid) <- TRUE
                              })

            ## only have the trend by level option enabled if
            ## the colored by variable option is set
            if (is.null(curSet$by))
                enabled(trendByChk) <- FALSE
            tbl[1, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[2, 1] <- linChk
            tbl[3, 1] <- quaChk
            tbl[4, 1] <- cubChk
            tbl[5, 1] <- smthChk
            tbl[2, 2] <- linCol
            tbl[3, 2] <- quaCol
            tbl[4, 2] <- cubCol
            tbl[5, 2] <- smthCol
            tbl[6, 1] <- quantSmthChk
            tbl[7, 1:2] <- smthSlid
            tbl[8, 1] <- trendByChk
            tbl[9, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)
        },
        ## Add x=y line
        opt3 = function(){
            tbl <- glayout()
            lbl1 <- glabel("Add x=y line")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            xyline <- gcheckbox("Plot x=y line",
                                checked = curSet$LOE)
            xyCols <- c("red", "black", "blue", "green4",
                        "yellow", "pink", "grey", "orange")
            xyCol <- gcombobox(xyCols,
                               selected = which(
                                   curSet$col.LOE == xyCols
                                   )
                               )
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      ## update plot settings
                                      GUI$getActiveDoc()$setSettings(
                                          list(LOE = svalue(xyline),
                                               col.LOE = svalue(xyCol))
                                          )
                                      updateSettings()
                                  })
            tbl[1, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[2, 1, expand = TRUE] <- xyline
            tbl[2, 2, expand = TRUE] <- xyCol
            tbl[3, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)
        },
        ## Add jitter
        opt4 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Add jitter:")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            xJit <- gcheckbox("Jitter x-variable",
                              checked = curSet$jitter %in% c("x", "xy"))
            yJit <- gcheckbox("Jitter y-variable",
                              checked = curSet$jitter %in% c("y", "xy"))
            showButton <- gbutton(
                "Show Changes",
                handler = function(h, ...) {
                    ## build string to show which jitter opt
                    ## was selected
                    jit <- ""
                    if (svalue(xJit)) jit <- paste(jit, "x", sep = "")
                    if (svalue(yJit)) jit <- paste(jit, "y", sep = "")
                    ## update plot settings
                    GUI$getActiveDoc()$setSettings(
                        list(jitter = jit)
                        )
                    updateSettings()
                })
            tbl[1, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[2, 1] <- xJit
            tbl[3, 1] <- yJit
            tbl[4, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)
        },
        ## Add rug
        opt5 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Add rug:")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            xRug <- gcheckbox("Add x-rug")
            yRug <- gcheckbox("Add y-rug")
            showButton <- gbutton(
                "Show Changes",
                handler = function(h, ...) {
                    ## build string to show which jitter opt
                    ## was selected
                    rug <- ""
                    if (svalue(xRug)) rug <- paste(rug, "x", sep = "")
                    if (svalue(yRug)) rug <- paste(rug, "y", sep = "")
                    ## update plot settings
                    GUI$getActiveDoc()$setSettings(
                        list(rugs = rug)
                        )
                    updateSettings()
                })
            tbl[1, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[2, 1] <- xRug
            tbl[3, 1] <- yRug
            tbl[4, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)
        },
        ## Join points by lines
        opt6 = function(){
            tbl <- glayout()
            lbl1 <- glabel("Join points by lines")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            joinPts <- gcheckbox("Join points",
                                 checked = curSet$join)
            lineByChk <- gcheckbox(paste("For each level of",
                                          curSet$varnames$by),
                                   selected = curSet$lines.by)
            joinCols <- c("red", "black", "blue", "green4",
                          "yellow", "pink", "grey", "orange")
            joinCol <- gcombobox(joinCols,
                               selected = which(
                                   curSet$col.line == joinCols
                                   )
                               )
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      ## update plot settings
                                      GUI$getActiveDoc()$setSettings(
                                          list(join = svalue(joinPts),
                                               col.line = svalue(joinCol),
                                               lines.by = svalue(lineByChk))
                                          )
                                      updateSettings()
                                  })
            ## only have the lines by level option enabled if
            ## the colored by variable option is set
            if (is.null(curSet$by))
                enabled(lineByChk) <- FALSE
            tbl[1, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[2, 1, expand = TRUE] <- joinPts
            tbl[2, 2, expand = TRUE] <- joinCol
            tbl[3, 1] <- lineByChk
            tbl[4, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)
        },
        ## change plot appearance
        opt7 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Change plot appearance")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            lbl2 = glabel("Colour of symbols :")
            lbl3 = glabel("Background colour :")
            lbl4 = glabel("Size of symbols  :")
            lbl5 = glabel("Thickness of symbols :")
            lbl6 = glabel("(Use drop down list or type in if desired color is unavailable)")
            lbl7 = glabel("Transparency of symbols  :")
            lbl8 <- glabel("Plot Type :")
            font(lbl6) <- list(family = "normal",
                               size = 8)
            ## default settings
            defts <- iNZightPlots:::inzPlotDefaults()
            ## active settings
            pointCols <- c(defts$col.pt, "darkblue", "darkgreen",
                "darkmagenta", "darkslateblue", "hotpink4",
                "lightsalmon2", "palegreen3", "steelblue3")
            backgroundCols <- c(defts$bg, "antiquewhite",
                "azure3", "bisque", "cornsilk", "darkolivegreen2",
                "darkslategray1", "greenyellow", "lightblue1",
                "lightpink", "rosybrown1", "slategray1", "thistle1",
                "wheat1")
            plotTypes <- c("default", "scatter plot", "grid-density plot")
            ## the values used for `largesample` in the plot settings
            plotTypeValues <- list(NULL, FALSE, TRUE)
            symbolColList <- gcombobox(
                pointCols,
                selected = ifelse(
                    is.na(which(pointCols == curSet$col.pt)[1]),
                    1,
                    which(pointCols == curSet$col.pt)[1]),
                editable = TRUE)
            backgroundColList <- gcombobox(
                backgroundCols,
                selected = ifelse(
                    is.na(which(backgroundCols == curSet$bg)[1]),
                    1,
                    which(backgroundCols == curSet$bg)[1]),
                editable = TRUE)
            plotTypeList <- gcombobox(
                plotTypes,
                selected = ifelse(
                    is.null(curSet$largesample),
                    1,
                    ifelse(
                        curSet$largesample == FALSE,
                        2, 3))
                )
            fillColor <- gcheckbox("Colour symbol interior",
                                   checked = (curSet$pch != 1))
            cexSlider <- gslider(from = 0.05, to = 3.5,
                by = 0.05, value = curSet$cex.pt)
            transpSlider <- gslider(from = 0.01, to = 1,
                                    by = 0.01, value = curSet$alpha)
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      pch.sel <- ifelse(svalue(fillColor),
                                                        19, 1)
                                      GUI$getActiveDoc()$setSettings(
                                          list(col.pt = svalue(symbolColList),
                                               bg = svalue(backgroundColList),
                                               cex.pt = svalue(cexSlider),
                                               pch = pch.sel,
                                               alpha = svalue(transpSlider),
                                               largesample = plotTypeValues[[svalue(
                                                   plotTypeList, index = TRUE)]]
                                               )
                                          )
                                      updateSettings()
                                  })
            tbl[3,2:4, anchor = c(-1,-1), expand = TRUE] <- lbl1
            tbl[4,2, anchor = c(-1,-1), expand = TRUE] <- lbl8
            tbl[4,3, expand = TRUE] <- plotTypeList
            tbl[5,2, anchor = c(-1,-1), expand = TRUE] <- lbl2
            tbl[5,3, expand = TRUE] <- symbolColList
            tbl[6,2:4] <- lbl6
            tbl[7,3, expand = TRUE] <- fillColor
            tbl[8,2, anchor = c(-1,-1), expand = TRUE] <- lbl3
            tbl[8,3, expand = TRUE] <- backgroundColList
            tbl[10,2, anchor = c(-1,-1), expand = TRUE] <- lbl4
            tbl[10,3, expand = TRUE] <- cexSlider
            tbl[11,2, anchor = c(-1,-1), expand = TRUE] <- lbl7
            tbl[11,3, expand = TRUE] <- transpSlider
            tbl[12, 2:4] <- showButton

            ## if the "by" options is set, i.e. points are colored
            ## according to another var, disable the option to
            ## change the color
            if (!is.null(GUI$getActiveDoc()$getSettings()$by)) {
                enabled(symbolColList) <- FALSE
                svalue(lbl6) <- paste(
                    "Changing the color of symbols is disabled since",
                    " the symbols are\n colored by variable '",
                    GUI$getActiveDoc()$getSettings()$varnames$by,
                    "'", sep = "")
            }
            add(optGrp, tbl)
        },
        opt8 = function() {
            ## Do checking first
            ## If g1 or g2 = _MULTI, then we can't identify points (yet ...)
            cantDo <- function(msg = "using subsetting variables.") {
                gmessage(paste("Cannot identify points when", msg),
                         icon = "error", title = "Unable to identify",
                         parent = modWin)
                dispose(GUI$modWin)
                return()
            }

            lbl1 <- "Select variable to identify:"
            font(lbl1) <- list(weight="bold", family = "normal")
            varmenu <- gcombobox(c("id", names(GUI$getActiveData())), selected = 1)

            lbl2 <- "Click \"Locate\" to locate a point"
            locateButton <- gbutton("Locate", handler = function(h, ...) {
                x <- curSet$x  # used for removing missing values ...
                y <- curSet$y
                v <- svalue(varmenu)
                
                w <- rep(TRUE, length(x))
                if (!is.null(curSet$g1)) {
                    if (is.null(curSet$g1.level)) {
                        cantDo()
                    } else if (curSet$g1.level == "_MULTI") {
                        cantDo()
                    }
                    w[curSet$g1 != curSet$g1.level] <- FALSE
                }
                if (!is.null(curSet$g2)) {
                    if (curSet$g2.level == "_MULTI") {
                        cantDo()
                    } else {
                        if (curSet$g2.level != "_ALL")
                            w[curSet$g2 != curSet$g2.level] <- FALSE
                    }
                }                

                if (is.null(v))
                    v <- as.character(1:length(x))
                else {
                    if (v == "id")
                        v <- as.character(1:length(x))
                    else {
                        v <- as.character(GUI$getActiveData()[, v])
                        v[is.na(v)] <- "missing"
                    }
                }
                
                
                if (!is.null(curSet$g1)) {
                    w[curSet$g1 != curSet$g1.level] <- FALSE
                }
                if (!is.null(curSet$g2)) {
                    if (curSet$g2.level != "_ALL") {
                        w[curSet$g2 != curSet$g2.level] <- FALSE
                    }
                }
               
                seekViewport("MAINVP")

                isNA <- is.na(x) | is.na(y)
                if (!is.null(curSet$g1))
                    isNA <- isNA | is.na(curSet$g1)
                if (!is.null(curSet$g2))
                    isNA <- isNA | is.na(curSet$g2)

                dp <- grid.get("SCATTERPOINTS")
                d <- data.frame(x = as.numeric(dp$x),
                                y = as.numeric(dp$y),
                                v = v[w & !isNA])
                seekViewport("SCATTERVP")
                
                xy <- as.numeric(grid.locator())
                
                ## We only want to check X and Y for missing
                na <- apply(d[, 1:2], 1, function(x) any(is.na(x)))
                d <- d[!na, ]

                ## So now, d = data.frame with x, y, and the label
                ## Standardise it:
                x.s <- (d$x - min(d$x)) / (max(d$x) - min(d$x))
                y.s <- (d$y - min(d$y)) / (max(d$y) - min(d$y))

                xy.s <- numeric(2)
                xy.s[1] <- (xy[1] - min(d$x)) / (max(d$x) - min(d$x))
                xy.s[2] <- (xy[2] - min(d$y)) / (max(d$y) - min(d$y))

                o <- d[which.min((x.s - xy.s[1])^2 + (y.s - xy.s[2])^2), ]

                grid.text(o$v, o$x,
                          o$y + ifelse(o$y < mean(d$y), 1, -1) *
                          convertWidth(unit(0.5, "char"), "native", TRUE),
                          just = ifelse(o$y > mean(d$y), "top", "bottom"),
                          default.units = "native", gp = gpar(cex = 0.7))

            })

            tbl <- glayout()
            tbl[1, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl1
            tbl[2, 1, expand = TRUE, anchor = c(1, 0)] <- varmenu
            tbl[4, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl2
            tbl[5, 1, expand = TRUE, anchor = c(1, 0)] <- locateButton
            
            add(optGrp, tbl)
        },
        opt9 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Customize Labels")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)

            curPlSet <- GUI$getActiveDoc()$getSettings()
            oldMain <- curPlSet$main
            oldX <- curPlSet$xlab
            oldY <- curPlSet$ylab
            if (is.null(oldMain)) oldMain <- ''
            if (is.null(oldX)) oldX <- ''
            if (is.null(oldY)) oldY <- ''

            lbl2    <- glabel("Main title :")
            labMain <- gedit(oldMain)
            lbl3    <- glabel("x-axis label :")
            labX    <- gedit(oldX)
            lbl4    <- glabel("y-axis label :")
            labY    <- gedit(oldY)

            lbl5 <- glabel("(Enter a single space to print no title)")
            font(lbl5) <- list(family = "normal",
                               size = 8)

            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      mlab <- svalue(labMain)
                                      xlab <- svalue(labX)
                                      ylab <- svalue(labY)
                                      GUI$getActiveDoc()$setSettings(
                                          list(main = if (mlab != '') mlab else NULL,
                                               xlab = if (xlab != '') xlab else NULL,
                                               ylab = if (ylab != '') ylab else NULL)
                                          )
                                      updateSettings()
                                  })
            tbl[3, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[4, 1, anchor = c(-1, -1), expand = TRUE] <- lbl2
            tbl[4, 2, expand = TRUE] <- labMain
            tbl[5, 1, anchor = c(-1, -1), expand = TRUE] <- lbl3
            tbl[5, 2, expand = TRUE] <- labX
            tbl[6, 1, anchor = c(-1, -1), expand = TRUE] <- lbl4
            tbl[6, 2, expand = TRUE] <- labY
            tbl[7, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl5
            tbl[8, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)
        })
    )
