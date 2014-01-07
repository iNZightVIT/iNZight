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
                curSet <<- GUI$getActiveDoc()$getSettings()
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
            usingMethods(opt1, opt2)
            opts <- gradio(c("Code more variables",
                             "Change plot appearance"),
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
                                  })
            tbl[3, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[4, 1, anchor = c(-1, -1), expand = TRUE] <- lbl2
            tbl[4, 2, expand = TRUE] <- grpVarList
            tbl[5, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)
        },
        opt2 = function() {
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
            fillColor <- gcheckbox("Colour symbol interior",
                                   checked = (curSet$pch != 1))
            cexSlider <- gslider(from = 0.5, to = 3.5,
                by = 0.1, value = curSet$cex.pt)
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      pch.sel <- ifelse(svalue(fillColor),
                                                        19, 1)
                                      GUI$getActiveDoc()$setSettings(
                                          list(col.pt = svalue(symbolColList),
                                               bg = svalue(backgroundColList),
                                               cex.pt = svalue(cexSlider),
                                               pch = pch.sel)
                                          )
                                  })            
            tbl[3,2:4, anchor = c(-1,-1), expand = TRUE] <- lbl1            
            tbl[4,2, anchor = c(-1,-1), expand = TRUE] <- lbl2
            tbl[4,3, expand = TRUE] <- symbolColList
            tbl[5,2:4] <- lbl6
            tbl[6,3, expand = TRUE] <- fillColor
            tbl[7,2, anchor = c(-1,-1), expand = TRUE] <- lbl3
            tbl[7,3, expand = TRUE] <- backgroundColList
            tbl[9,2, anchor = c(-1,-1), expand = TRUE] <- lbl4
            tbl[9,3, expand = TRUE] <- cexSlider
            tbl[11, 2:4] <- showButton
            add(optGrp, tbl)
        })
    )

iNZBarchartMod <- setRefClass(
    "iNZBarchartMod",
    contains = "iNZPlotModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            opts <- glabel("segment bar chart(s)")
            addSpring(radioGrp)
            add(radioGrp, opts)
            addSpring(radioGrp)
            radioGrp$set_borderwidth(10)
            tbl <- glayout()
            lbl1 <- glabel("segment by levels of :")
            grpVarList <- gcombobox(c("", names(GUI$getActiveData())))
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      GUI$getActiveDoc()$setSettings(
                                          list(by = GUI$getActiveData()[[
                                                   svalue(grpVarList)]],
                                               varnames = list(
                                                   by = svalue(grpVarList)))
                                          )
                                  })
            tbl[1, 1] <- lbl1
            tbl[1, 2] <- grpVarList
            tbl[2, 1:3, expand = TRUE] <- showButton
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
            usingMethods(opt1, opt2, opt3, opt4, opt5, opt6, opt7, opt8)
            opts <- gradio(c("Code more variables",
                             "Add trend curves",
                             "Add x=y line",
                             "Add a smoother",
                             "Add a jitter",
                             "Add rugs",
                             "Join points by lines",
                             "Change plot appearance"),
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
                                               varnames = list(
                                                   by = svalue(grpVarList)))
                                          )
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
                                      ## update plot settings
                                      GUI$getActiveDoc()$setSettings(
                                          list(trend = trCrvs[trSel],
                                               col.trend = list(
                                                   linear = trCol[1],
                                                   quadratic = trCol[2],
                                                   cubic = trCol[3])
                                               )
                                          )                                      
                                  })
            tbl[1, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[2, 1] <- linChk
            tbl[3, 1] <- quaChk
            tbl[4, 1] <- cubChk
            tbl[2, 2] <- linCol
            tbl[3, 2] <- quaCol
            tbl[4, 2] <- cubCol
            tbl[5, 1:2, expand = TRUE] <- showButton
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
                                  })
            tbl[1, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[2, 1, expand = TRUE] <- xyline
            tbl[2, 2, expand = TRUE] <- xyCol
            tbl[3, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)            
        },
        ## Add smoother
        opt4 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Choose the smoothness:")            
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            lbl2 <- glabel("(Larger values give more smoothness)")
            font(lbl2) <- list(size = 8)
            smthChk <- gcheckbox("Draw a smoother",
                                 checked = curSet$smooth!=0)
            smthSlid <- gslider(from = 0.1, to = 1,
                                by = 0.1,
                                value = ifelse(curSet$smooth==0,
                                    0.7, curSet$smooth))
            smthCols <- c("red", "black", "blue", "green", "yellow",
                          "magenta", "grey", "orange")
            smthCol <- gcombobox(smthCols,
                                 selected = which(
                                     curSet$col.smooth == smthCols)
                                 )
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      ## vector of selected trends
                                      smth <- ifelse(svalue(smthChk),
                                                     svalue(smthSlid),
                                                     0)
                                      ## update plot settings
                                      GUI$getActiveDoc()$setSettings(
                                          list(smooth = smth,
                                               col.smooth = svalue(smthCol)
                                               )
                                          )                                      
                                  })
            tbl[1, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[2, 1:2, anchor = c(-1, 1), expand = TRUE] <- lbl2
            tbl[3, 1, expand = TRUE] <- smthChk
            tbl[3, 2, expand = TRUE] <- smthCol
            tbl[4, 1:2, expand = TRUE] <- smthSlid
            tbl[5, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)            
        },
        ## Add jitter
        opt5 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Add jitter:")            
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            xJit <- gcheckbox("Jitter x-variable",
                              checked = curSet$jitter %in% c("x", "xy"))
            yJit <- gcheckbox("Jitter y-variable",
                              checked = curSet$jitter %in% c("y", "xy"))
            showButton <- gbutton("Show Changes",
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
                                  })
            tbl[1, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[2, 1] <- xJit
            tbl[3, 1] <- yJit
            tbl[4, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)               
        },
        ## Add rug
        opt6 = function() {
            tbl <- glayout()
            lbl1 <- glabel("Add rug:")            
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            xRug <- gcheckbox("Add x-rug")
            yRug <- gcheckbox("Add y-rug")
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      ## build string to show which jitter opt
                                      ## was selected
                                      rug <- ""
                                      if (svalue(xRug)) jit <- paste(rug, "x", sep = "")
                                      if (svalue(yRug)) jit <- paste(rug, "y", sep = "")
                                      ## update plot settings
                                      GUI$getActiveDoc()$setSettings(
                                          list(rugs = rug)
                                          )                                      
                                  })
            tbl[1, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[2, 1] <- xRug
            tbl[3, 1] <- yRug
            tbl[4, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)               
        },
        ## Join points by lines
        opt7 = function(){
            tbl <- glayout()
            lbl1 <- glabel("Join points by lines")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            joinPts <- gcheckbox("Join points",
                                 checked = curSet$join)
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
                                               col.line = svalue(joinCol))
                                          )                                      
                                  })
            tbl[1, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            tbl[2, 1, expand = TRUE] <- joinPts
            tbl[2, 2, expand = TRUE] <- joinCol
            tbl[3, 1:2, expand = TRUE] <- showButton
            add(optGrp, tbl)            
        },
        opt8 = function() {
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
            fillColor <- gcheckbox("Colour symbol interior",
                                   checked = (curSet$pch != 1))
            cexSlider <- gslider(from = 0.5, to = 3.5,
                by = 0.1, value = curSet$cex.pt)
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) {
                                      pch.sel <- ifelse(svalue(fillColor),
                                                        19, 1)
                                      GUI$getActiveDoc()$setSettings(
                                          list(col.pt = svalue(symbolColList),
                                               bg = svalue(backgroundColList),
                                               cex.pt = svalue(cexSlider),
                                               pch = pch.sel)
                                          )
                                  })            
            tbl[3,2:4, anchor = c(-1,-1), expand = TRUE] <- lbl1            
            tbl[4,2, anchor = c(-1,-1), expand = TRUE] <- lbl2
            tbl[4,3, expand = TRUE] <- symbolColList
            tbl[5,2:4] <- lbl6
            tbl[6,3, expand = TRUE] <- fillColor
            tbl[7,2, anchor = c(-1,-1), expand = TRUE] <- lbl3
            tbl[7,3, expand = TRUE] <- backgroundColList
            tbl[9,2, anchor = c(-1,-1), expand = TRUE] <- lbl4
            tbl[9,3, expand = TRUE] <- cexSlider
            tbl[11, 2:4] <- showButton
            add(optGrp, tbl)
        })            
    )
