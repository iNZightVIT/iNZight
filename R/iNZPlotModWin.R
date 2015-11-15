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
        curSet = "list", ## the current plot settings
        auto = "logical",   ## if TRUE, then changes occur automatically
        locSet = "ANY"
        ),
    methods = list(
        initialize = function(gui = NULL, which = 1) {
            initFields(GUI = gui)
            if (!is.null(GUI)) {
                updateSettings()

                if (length(GUI$leftMain$children) > 1) {
                    delete(GUI$leftMain, GUI$leftMain$children[[2]])
                }
                GUI$initializeModuleWindow()
                
                mainGrp <- gvbox(container = GUI$moduleWindow, expand = TRUE, fill = TRUE)
                mainGrp$set_borderwidth(5)
                topGrp <- ggroup(horizontal = TRUE,
                                 container = mainGrp)
                lbl <- glabel("I want to ")
                font(lbl) <- list(weight="bold",
                                  family = "normal",
                                  size = 11)
                radioGrp <<- ggroup(horizontal = FALSE,
                                    expand = TRUE)
                
                optGrp <<- ggroup(horizontal = FALSE, expand = TRUE)
                add(topGrp, lbl)
                add(topGrp, radioGrp, expand = TRUE, fill = TRUE)

                add(mainGrp, optGrp)

                addSpring(mainGrp)

                btnGrp <- ggroup(horizontal = TRUE,
                                 expand = FALSE)

                helpButton <- gbutton("Help", expand = TRUE, fill = TRUE,
                                      cont = btnGrp,
                                      handler = function(h, ...) {
                                          browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/plot_options/?topic=add_to_plot")
                                      })
                            
                okButton <<- gbutton("Close", expand = TRUE, fill = TRUE,
                                     cont = btnGrp,
                                     handler = function(h, ...) {
                                         ## delete the module window
                                         delete(GUI$leftMain, GUI$leftMain$children[[2]])
                                         ## display the default view (data, variable, etc.)
                                         visible(GUI$gp1) <<- TRUE
                                     })
                
                
                add(mainGrp, btnGrp, expand = FALSE, fill = "x", anchor = c(0, -1))

                ## If sample size is too big, use a button instead of automatically apply changes
                auto <<- nrow(GUI$getActiveData()) < 100000

                visible(GUI$moduleWindow) <<- TRUE
            }
        },
        ## up the curSet class variable
        updateSettings = function() {
            curSet <<- GUI$getActiveDoc()$getSettings()
        },
        iNZLocatePoints = function(dot = FALSE) {
            ## Do checking first
            ## If g1 or g2 = _MULTI, then we can't identify points (yet ...)
            cantDo <- function(msg = "using subsetting variables.") {
                gmessage(paste("Cannot identify points when", msg),
                         icon = "error", title = "Unable to identify",
                         parent = modWin)
                return()
            }
            
            locSet <<- curSet$locate.settings
            
            updateEverything <- function(locate = GUI$getActiveDoc()$getSettings()$locate,
                                         id = GUI$getActiveDoc()$getSettings()$locate.id,
                                         col = GUI$getActiveDoc()$getSettings()$locate.col,
                                         ext = GUI$getActiveDoc()$getSettings()$locate.extreme) {
                if (is.null(id) & is.null(ext)) {
                    locate = NULL
                    id = NULL
                    col = NULL
                    ext = NULL
                }
                
                if (!is.null(id)) {
                    ext <- NULL
                } else {
                    locSet$ID <<- NULL
                }
                
                highlight <-
                    if (svalue(matchChk)) locSet$ID
                    else NULL
                
                ## update the locate settings:
                locSet$txtLabs <<- svalue(txtLabs)
                locSet$txtVar <<- svalue(varmenu)
                
                locSet$colLabs <<- svalue(colLabs)
                locSet$colVar <<- svalue(colmenu)
                
                locSet$matchChk <<- svalue(matchChk)
                locSet$matchVar <<- svalue(matchVar)
                
                locSet$selectMthd <<- svalue(selectMthd)
                
                curSet$locate.settings <<- locSet
                
                if (locSet$matchChk) {
                    levs <- unique(GUI$getActiveData()[highlight, locSet$matchVar])
                    
                    if (length(levs) > 1)
                        levs <- paste0("{", paste(levs, collapse = ", "), "}")
                    
                    if (length(levs) == 1)
                        subt <- paste0("(Locating points with ",
                                       locSet$matchVar, " = ", levs, ")")
                    else
                        subt <- NULL
                } else {
                    subt <- NULL
                }
                
                GUI$getActiveDoc()$setSettings(
                                      list(locate = locate,
                                           locate.id = unique(id),
                                           locate.col = col,
                                           locate.extreme = ext,
                                           locate.settings = locSet,
                                           highlight = highlight,
                                           subtitle = subt)
                                  )
                updateSettings()
            }
            
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("How do you want to label points?")
            font(lbl) <- list(weight = "bold", family = "normal")
            tbl[ii, 1:2, expand = TRUE, anchor = c(-1, 0)] <- lbl
            ii <- ii + 1
            
            txtLabs <- gcheckbox("Text Labels", checked = TRUE)
            varmenu <- gcombobox(c("id", names(GUI$getActiveData())), selected = 1, expand = TRUE)
            tbl[ii, 1] <- txtLabs
            tbl[ii, 2, expand = TRUE] <- varmenu
            ii <- ii + 1
            
            if (!is.null(locSet$txtLabs)) svalue(txtLabs) <- locSet$txtLabs
            if (!is.null(locSet$txtVar))
                if (locSet$txtVar %in% c("id", names(GUI$getActiveData())))
                    svalue(varmenu) <- locSet$txtVar
            
            colLabs <- gcheckbox("Colour Points", checked = FALSE)
            colmenu <- gcombobox(c("red", "blue", "green4"), selected = 1, editable = TRUE, expand = TRUE)
            tbl[ii, 1] <- colLabs
            tbl[ii, 2, expand = TRUE] <- colmenu
            ii <- ii + 1
            
            if (!is.null(locSet$colLabs)) svalue(colLabs) <- locSet$colLab
            if (!is.null(locSet$colVar))
                svalue(colmenu) <- locSet$colVar
            
            enabled(varmenu) <- svalue(txtLabs)  #labMthd, TRUE) == 1
            enabled(colmenu) <- svalue(colLabs)  #labMthd, TRUE) == 2
            
            addHandlerChanged(txtLabs, function(h, ...) {
                enabled(varmenu) <- svalue(txtLabs)  #labMthd, TRUE) == 1
                v <- svalue(varmenu)
                locVar <- if (v == "id") 1:nrow(GUI$getActiveData()) else GUI$getActiveData()[, v]
                updateEverything(
                    locate = if (svalue(txtLabs)) locVar else NULL
                )
            })
            addHandlerChanged(varmenu, function(h, ...) {
                v <- svalue(varmenu)
                locVar <- if (v == "id") 1:nrow(GUI$getActiveData()) else GUI$getActiveData()[, v]
                updateEverything(
                    locate = if (svalue(txtLabs)) locVar else NULL
                )
            })
            addHandlerChanged(colLabs, function(h, ...) {
                enabled(colmenu) <- svalue(colLabs)  #labMthd, TRUE) == 2
                updateEverything(
                    col = if (svalue(colLabs)) svalue(colmenu) else NULL
                )
            })
            addHandlerChanged(colmenu, function(h, ...) {
                if (svalue(colmenu) %in% colours()) {
                    updateEverything(
                        col = if (svalue(colLabs)) svalue(colmenu) else NULL
                    )
                }
            })
            
            
            matchChk <- gcheckbox("With the same level of")
            tbl[ii, 1] <- matchChk
            
            if (!is.null(locSet$matchChk)) svalue(matchChk) <- locSet$matchChk
            
            matchVar <- gcombobox(names(GUI$getActiveData()), selected = 1)
            enabled(matchVar) <- svalue(matchChk)
            tbl[ii, 2, expand = TRUE] <- matchVar
            ii <- ii + 1
            
            if (!is.null(locSet$matchVar))
                if (locSet$matchVar %in% names(GUI$getActiveData()))
                    svalue(matchVar) <- locSet$matchVar
            
            addHandlerChanged(matchChk, function(h, ...) {
                enabled(matchVar) <- svalue(matchChk)
                enabled(clearMulti) <- svalue(matchChk)
                svalue(clearMulti) <- svalue(matchChk)
                
                if (svalue(matchChk)) {
                    ## Add all the points:
                    
                    matchVar <- as.character(GUI$getActiveData()[, svalue(matchVar)])
                    matchVar[is.na(matchVar)] <- "missing"
                    
                    matchLvls <- unique(matchVar[locSet$ID])
                    newIDS <- which(matchVar %in% matchLvls)
                    
                    updateEverything(id = newIDS)
                } else {
                    ## Remove all the points:
                    
                    updateEverything(id = locSet$ID)
                }
            })
            
            addHandlerChanged(matchVar, function(h, ...) {
                matchVar <- as.character(GUI$getActiveData()[, svalue(matchVar)])
                matchVar[is.na(matchVar)] <- "missing"
                
                matchLvls <- unique(matchVar[locSet$ID])
                newIDS <- which(matchVar %in% matchLvls)
                
                updateEverything(id = newIDS)
            })
            
            
            
            ii <- ii + 1
            
            
            lbl <- glabel("How do you want to select points?")
            font(lbl) <- list(weight = "bold", family = "normal")
            tbl[ii, 1:2, expand = TRUE, anchor = c(-1, 0)] <- lbl
            ii <- ii + 1
            
            
            selectMthd <- gradio(c("Click points",
                                   "Select by value of ...",
                                   "Extreme values"), selected = 1)
            tbl[ii, 1:2, expand = TRUE] <- selectMthd
            ii <- ii + 1
            
            selectGrp <- ggroup(horiz = FALSE, expand = TRUE)
            
            
            
            locator <- function(h, remove = FALSE, btn, dot = FALSE, ...) {
                x <- curSet$x  # used for removing missing values ...
                if (!dot)
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
                
                match.all <- svalue(matchChk)
                
                locVar <-
                    if (v == "id") 1:nrow(GUI$getActiveData())
                    else GUI$getActiveData()[, v]
                
                matchVar <- as.character(GUI$getActiveData()[, svalue(matchVar)])
                matchVar[is.na(matchVar)] <- "missing"
                
                ## Entire data set - ignore missing values etc etc
                d <- data.frame(x = curSet$x,
                                locate = locVar, id = 1:nrow(GUI$getActiveData()),
                                match = matchVar)
                if (!dot)
                    d$y <- curSet$y
                
                if (!is.null(curSet$g1)) {
                    w[curSet$g1 != curSet$g1.level] <- FALSE
                }
                if (!is.null(curSet$g2)) {
                    if (curSet$g2.level != "_ALL") {
                        w[curSet$g2 != curSet$g2.level] <- FALSE
                    }
                }
                
                if (dot)
                    isNA <- is.na(x)
                else
                    isNA <- is.na(x) | is.na(y)
                
                if (!is.null(curSet$g1))
                    isNA <- isNA | is.na(curSet$g1)
                if (!is.null(curSet$g2))
                    isNA <- isNA | is.na(curSet$g2)
                
                dp <- grid.get(ifelse(dot, "DOTPOINTS", "SCATTERPOINTS"))
                d <- d[w & !isNA, ]
                d$x <- as.numeric(dp$x)
                d$y <- as.numeric(dp$y)
                
                if (dot) {
                    order <- attr(GUI$curPlot[[1]][[1]]$toplot[[1]], "order")
                    d[, !colnames(d) %in% c("x", "y")] <- d[order, !colnames(d) %in% c("x", "y")]
                }        
                
                seekViewport(ifelse(dot, "VP:plotregion", "VP:locate.these.points"))
                
                blockHandlers(btn)
                oldVal <- svalue(btn)
                svalue(btn) <- "Click a point"                
                xy <- as.numeric(grid.locator())
                svalue(btn) <- oldVal
                unblockHandlers(btn)
                
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
                
                if (remove) {
                    ## Remove it
                    if (svalue(clearMulti)) {
                        rid <- which(matchVar == o[, 'match'])
                    } else {
                        rid <- o$id
                    }
                    
                    locSet$ID <<- locSet$ID[!locSet$ID %in% rid]
                    newID <- curSet$locate.id[!curSet$locate.id %in% rid]
                } else {
                    ## Store the reference ID - add it
                    
                    if (!svalue(txtLabs) & match.all) 
                        locSet$ID <<- o$id
                    else 
                        locSet$ID <<- unique(c(locSet$ID, o$id))
                    
                    ## Grab the label:
                    if (match.all) {
                        ## Match all instances of the same label:
                        pid <- which(matchVar == o[, 'match'])
                    } else {
                        pid <- o$id
                    }
                    
                    newID <- if (svalue(txtLabs) | !match.all) c(curSet$locate.id, pid) else pid
                }
                
                updateEverything(
                    locate = if (svalue(txtLabs)) locVar else NULL,
                    id = newID,
                    col = if (svalue(colLabs)) svalue(colmenu) else NULL
                )
            }
            
            if (attr(GUI$curPlot, "nplots") > 1) {
                locateButton <- glabel("Cannot locate using mouse for multiple graphs.", cont =
                                                                                             selectGrp)
                svalue(selectMthd, TRUE) <- 2
            } else if (dot & is.factor(curSet$y)) {
                locateButton <- glabel("Cannot locate when Variable 2 is a factor.", cont = selectGrp)
                svalue(selectMthd, TRUE) <- 2
            } else {
                locateButton <- gbutton("Click to Locate ...", cont = selectGrp)
                addHandlerClicked(locateButton, function(h, ...) {
                    locator(h, btn = locateButton, dot = dot)
                })
            }
            
            selectListGrp <- ggroup(FALSE, cont = selectGrp, expand = TRUE, fill = TRUE)
            
            selectList <- ggroup(TRUE, cont = selectListGrp, expand = TRUE, fill = TRUE)
            selectLab <- glabel("Variable: ", cont = selectList)
            selectVar <- gcombobox(colnames(GUI$getActiveData()), selected = 0, cont = selectList,
                                   expand = TRUE)
            
            selectSlideGrp <- ggroup(TRUE, cont = selectListGrp, expand = FALSE, fill = TRUE)
            selectGo <- gbutton("Select values ...", cont = selectList)
            
            enabled(selectGo) <- svalue(selectVar, TRUE) > 0
            addHandlerChanged(selectVar, function(h, ...) {
                enabled(selectGo) <- svalue(selectVar, TRUE) > 0
                
                selVar <- GUI$getActiveData()[, svalue(selectVar)]
                
                if (length(selectSlideGrp$children) > 0)
                    selectSlideGrp$remove_child(selectSlideGrp$children[[1]])
                
                if (is.factor(selVar) | (length(unique(selVar)) <= 20)) {
                    nn <- if (is.factor(selVar)) length(levels(selVar)) else length(unique(selVar))
                    selectSlide <- gslider(if (is.factor(selVar)) levels(selVar) else unique(selVar),
                                           cont = selectSlideGrp, expand = TRUE, fill = TRUE)
                    
                    addHandlerChanged(selectSlide, function(h, ...) {
                        ids <- which(GUI$getActiveData()[, svalue(selectVar)] == svalue(selectSlide))
                        locSet$ID <<- ids
                        
                        if (svalue(matchChk)) {
                            levs <- unique(as.character(GUI$getActiveData()[ids, svalue(matchVar)]))
                            ids <- which(GUI$getActiveData()[, svalue(matchVar)] %in% levs)
                        }
                        
                        v <- svalue(varmenu)
                        locVar <-
                            if (v == "id") 1:nrow(GUI$getActiveData())
                            else GUI$getActiveData()[, v]
                        
                        updateEverything(
                            locate = if (svalue(txtLabs)) locVar else NULL,
                            id = ids,
                            col = if (svalue(colLabs)) svalue(colmenu) else NULL
                        )
                    })
                    selectSlide$invoke_change_handler()
                }
            })
            
            
            extremeGrp <- ggroup(FALSE, cont = selectGrp, expand = TRUE, fill = TRUE)
            if (dot) {
                extremePts <- ggroup(FALSE, cont = extremeGrp, expand = TRUE, fill = TRUE)
                
                lowerG <- ggroup(cont = extremePts, expand = TRUE, fill = TRUE)
                lowerLab <- glabel("N Lower: ", cont = lowerG)
                nlowerSld <- gslider(0, 20, expand = TRUE, fill = TRUE, cont = lowerG)
                
                upperG <- ggroup(cont = extremePts, expand = TRUE, fill = TRUE)
                upperLab <- glabel("N Upper: ", cont = upperG)
                nupperSld <- gslider(0, 20, expand = TRUE, fill = TRUE, cont = upperG)
                
                updateMe <- function(h, ...) {
                    v <- svalue(varmenu)
                    locVar <- if (v == "id") 1:nrow(GUI$getActiveData()) else GUI$getActiveData()[, v]
                    updateEverything(
                        locate = if (svalue(txtLabs)) locVar else NULL,
                        id = NULL,
                        col = if (svalue(colLabs)) svalue(colmenu) else NULL,
                        ext = c(svalue(nlowerSld), svalue(nupperSld))
                    )
                    
                    enabled(addPts) <- svalue(nlowerSld) > 0 | svalue(nupperSld) > 0
                }
                addHandlerChanged(nlowerSld, updateMe)
                addHandlerChanged(nupperSld, updateMe)
            } else {
                extremePts <- ggroup(cont = extremeGrp, expand = TRUE, fill = TRUE)
                extLab <- glabel("Number of points: ", cont = extremePts)
                extN <- gslider(0, 20, cont = extremePts, expand = TRUE)
                if (!is.null(curSet$locate.extreme)) svalue(extN) <- curSet$locate.extreme
                addHandlerChanged(extN, handler = function(h, ...) {
                    v <- svalue(varmenu)
                    locVar <- if (v == "id") 1:nrow(GUI$getActiveData()) else GUI$getActiveData()[, v]
                    
                    updateEverything(
                        locate = if (svalue(txtLabs)) locVar else NULL,
                        id = NULL,
                        col = if (svalue(colLabs)) svalue(colmenu) else NULL,
                        ext = if (svalue(extN) > 0) svalue(extN) else NULL
                    )
                    enabled(addPts) <- svalue(extN) > 0
                })
            }
            addPts <- gbutton("Save these points ...", cont = extremeGrp, expand = FALSE, anchor = c(0, 1))
            enabled(addPts) <- if (dot) svalue(nlowerSld) > 0 | svalue(nupperSld) > 0 else svalue(extN) > 0
            
            extLabel <- glabel("NOTE: related points wont be located until\nyou click the above button.")
            font(extLabel) <- list(family = "normal", size = 7)
            add(extremeGrp, extLabel, anchor = c(-1, -1))
            
            addHandlerClicked(addPts, function(h, ...) {
                cp <- GUI$curPlot
                ## drop the last 3 pieces (gen, xlim, ylim)
                cp <- cp[1:(length(cp) - 3)]
                if (dot) {
                    ids <- sapply(cp, function(p) sapply(p, function(q) sapply(q$toplot, function(r) r$extreme.ids)))
                } else {
                    ids <- sapply(cp, function(p) sapply(p, function(q) q$extreme.ids))
                }
                ids <- sapply(ids[!sapply(ids, is.null)], function(x) x)
                
                locSet$ID <<- ids
                v <- svalue(varmenu)
                locVar <- if (v == "id") 1:nrow(GUI$getActiveData()) else GUI$getActiveData()[, v]
                
                if (svalue(matchChk)) {
                    mVar <- as.character(GUI$getActiveData()[, svalue(matchVar)])
                    mVar[is.na(mVar)] <- "missing"
                    mLevs <- unique(mVar[ids])
                    ids <- which(mVar %in% mLevs)
                }
                
                updateEverything(
                    locate = if (svalue(txtLabs)) locVar else NULL,
                    id = ids,
                    col = if (svalue(colLabs)) svalue(colmenu) else NULL,
                    ext = NULL
                )
                
                enabled(addPts) <- length(locSet$ID) == 0
            })
            
            if (!is.null(locSet$selectMthd))
                svalue(selectMthd) <- locSet$selectMthd
            
            
            ## Bring up a new window to allow user to select levels to label:
            addHandlerClicked(selectGo, function(h, ...) {
                ww <- gwindow("Select levels to label ...", visible = FALSE, width = 200, height = 400,
                              parent = GUI$win)
                wg <- ggroup(FALSE, cont = ww)
                wlbl <- glabel("Select levels to label\n(ctrl for multiple)", cont = wg)
                
                selectLevels <- gtable(levels(as.factor(GUI$getActiveData()[, svalue(selectVar)])),
                                       multiple = TRUE, cont = wg, expand = TRUE)
                
                wb <- gbutton("Done", cont = wg)
                addHandlerClicked(wb, function(h, ...) {
                    ids <-  which(GUI$getActiveData()[, svalue(selectVar)] %in% svalue(selectLevels))
                    locSet$ID <<- ids
                    
                    if (svalue(matchChk)) {
                        levs <- unique(as.character(GUI$getActiveData()[ids, svalue(matchVar)]))
                        ids <- which(GUI$getActiveData()[, svalue(matchVar)] %in% levs)
                    }
                    
                    v <- svalue(varmenu)
                    locVar <-
                        if (v == "id") 1:nrow(GUI$getActiveData())
                        else GUI$getActiveData()[, v]
                    
                    updateEverything(
                        locate = if (svalue(txtLabs)) locVar else NULL,
                        id = ids,
                        col = if (svalue(colLabs)) svalue(colmenu) else NULL
                    )
                    
                    dispose(ww)
                })
                visible(ww) <- TRUE
            })
            
            tbl[ii, 1:2, expand = TRUE, anchor = c(1, 0)] <- selectGrp
            ii <- ii + 1
            
            
            ii <- ii + 1
            
            clearBtn <- gbutton("Clear all labels")
            addHandlerClicked(clearBtn, function(h, ...) {
                updateEverything(NULL, NULL, NULL, NULL)
            })
            tbl[ii, 1, expand = TRUE] <- clearBtn
            
            
            clearBtn2 <- gbutton("Clear label ...")
            addHandlerClicked(clearBtn2, function(h, ...) {
                locator(h, remove = TRUE, btn = clearBtn2, dot = dot)
            })
            tbl[ii, 2, expand = TRUE] <- clearBtn2
            ii <- ii + 1
            
            clearMulti <- gcheckbox("Remove group", checked = svalue(matchChk))
            tbl[ii, 2, expand = TRUE] <- clearMulti
            ii <- ii + 1
            
            addHandlerChanged(selectMthd, function(h, ...) {
                visible(locateButton) <- svalue(selectMthd, TRUE) == 1
                visible(selectListGrp) <- svalue(selectMthd, TRUE) == 2
                visible(extremeGrp) <- svalue(selectMthd, TRUE) == 3
                
                ## enabled(matchChk) <- svalue(selectMthd, TRUE) != 3
                visible(clearBtn2) <- svalue(selectMthd, TRUE) == 1
                visible(clearMulti) <- svalue(selectMthd, TRUE) == 1
                enabled(clearMulti) <- svalue(matchChk)
            })
            selectMthd$invoke_change_handler()
            
            add(optGrp, tbl, expand = TRUE, fill = TRUE)
        },
        specifyColours = function(var) {
            if (is.numeric(var)) {
                return(NULL)
            } else {
                lvls <- levels(var)
                colWin <- gwindow("Select Colours", visible = FALSE, parent = GUI$win)
                cgrp <- gvbox(spacing = 5, container = colWin)
                cgrp$set_borderwidth(5)
                tbl <- glayout()
                jj <- 1
                
                lbl <- glabel("Select colours")
                font(lbl) <- list(weight = "bold", family = "normal", size = 9)
                tbl[jj, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
                jj <- jj + 1
                
                ## this really needs changing!!
                default.cols <- c("darkblue", "darkgreen",
                                  "darkmagenta", "darkslateblue", "hotpink4",
                                  "lightsalmon2", "palegreen3", "steelblue3")
                current.cols <- GUI$curPlot$gen$col.args$f.cols
                
                for (k in 1:length(lvls)) {
                    tbl[jj, 1, expand = TRUE, anchor = c(1, 0)] <- glabel(lvls[k])
                    tbl[jj, 2] <- gcombobox(items = c(current.cols[k], default.cols), editable = TRUE)
                    jj <- jj + 1
                }
                
                okBtn <- gbutton("OK", function(h, ...) {
                    ri <- (1:length(lvls)) + 1
                    newCols <- sapply(tbl[ri, 2], svalue)
                    
                    ## check values are valid colours:
                    OK <- sapply(newCols, function(x) {
                        sapply(x, function(X) {
                            tryCatch(is.matrix(col2rgb(X)), 
                                     error = function(e) FALSE)
                        })
                    })
                    
                    if (all(OK)) {
                        GUI$getActiveDoc()$setSettings(
                                              list(col.pt = newCols)
                                          )
                        updateSettings()
                        dispose(colWin)
                    } else {
                        gmessage(paste0("Not valid colours:\n\n",
                                        paste(newCols[!OK], collapse = ", ")),
                                 title = "Invalid Colours", icon = "error")
                    }
                })
                
                cnclBtn <- gbutton("Cancel", function(h, ...) {
                    dispose(colWin)
                })
                resetBtn <- gbutton("Reset", function(h, ...) {
                    GUI$getActiveDoc()$setSettings(
                                          list(col.pt = NULL)
                                      )
                    updateSettings()
                    dispose(colWin)
                })
                
                add(cgrp, tbl)
                addSpring(cgrp)
                
                cbtnGrp <- ggroup(cont = cgrp)
                add(cbtnGrp, resetBtn)
                
                addSpring(cbtnGrp)
                
                add(cbtnGrp, okBtn)
                addSpace(cbtnGrp, 10)
                add(cbtnGrp, cnclBtn)
                
                visible(colWin) <- TRUE
            }
        }
    )
)


### ---------------------------------------------------------------------------------------------------
### DOT PLOT MOD WINDOW

iNZDotchartMod <- setRefClass(
    "iNZDotchartMod",
    contains = "iNZPlotModWin",
    methods = list(
        initialize = function(GUI, which = 1) {
            callSuper(GUI)
            ## need to specify the methods that we want to use in
            ## do.call later on (see changeOpts())
            usingMethods(opt1, opt2, opt3, opt4, opt5, iNZLocatePoints)
            opts <- gcombobox(c("Code more variables",
                                "Change plot appearance",
                                "Identify points",
                                "Customize Labels",
                                "Adjust axis limits"),
                              selected = which)
            add(radioGrp, opts, fill = TRUE, expand = TRUE)
            eval(parse(text = paste0("opt", which, "()")))
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
            ii <- 3
            
            lbl1 <- glabel("Code More Variables")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            ii <- ii + 1
            
            lbl2 <- glabel("Colour by levels of :")
            grpVarList <- gcombobox(c("", names(GUI$getActiveData())),
                                    selected = ifelse(
                                        is.null(curSet$colby),
                                        1, which(names(GUI$getActiveData()) ==
                                                     curSet$varnames$colby)[1] + 1
                                        )
                                    )
            
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl2
            tbl[ii, 2, expand = TRUE] <- grpVarList
            ii <- ii + 1

            lvlCols <- gbutton("Specify colours")
            tbl[ii, 2, expand = TRUE] <- lvlCols
            visible(lvlCols) <- svalue(grpVarList, index = TRUE) != 1
            ii <- ii + 1

            addHandlerClicked(lvlCols, function(h, ...) {
                variable <- GUI$getActiveData()[, svalue(grpVarList, index = FALSE)]
                if (is.numeric(variable)) {
                    gmessage("Set colour of numeric ... not yet implemented.", "Not ready yet.", icon = "warning")
                } else {
                    specifyColours(variable)
                }                
            })


            ## Maintain a single function that is called whenever anything is updated:
            updateEverything <- function() {
                GUI$getActiveDoc()$setSettings(
                    list(colby = GUI$getActiveData()[[
                             svalue(grpVarList)]],
                         varnames = list(
                             colby = svalue(grpVarList)))
                    )
                updateSettings()
            }
            
            ## in this case, no point in having a separate "show" button
            addHandlerChanged(grpVarList,
                              handler = function(h, ...) {
                                  if (svalue(grpVarList, index = TRUE) == 1) {
                                      updateEverything()
                                      visible(lvlCols) <- FALSE
                                  } else {
                                      var <- GUI$getActiveData()[[svalue(grpVarList)]]
                                      if (length(unique(var)) <= 1) {
                                          visible(lvlCols) <- FALSE
                                          gmessage(paste("The variable", svalue(grpVarList),
                                                         "only has one unique value, so colouring by it wont work."),
                                                   icon = "warning", title = "Invalid variable")
                                      } else {
                                          updateEverything()
                                          visible(lvlCols) <- is.factor(var)
                                      }
                                  }
                              })
            
            add(optGrp, tbl)
        },
        ## Change Plot appearance
        opt2 = function() {
            tbl <- glayout()
            ii <- 3

            ## Default settings
            defts <- iNZightPlots:::inzpar()
            
            ## PLOT APPEARANCE
            lbl <- glabel("Change plot appearance")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii,  1:2, anchor = c(-1,-1), expand = TRUE] <- lbl
            ii <- ii + 1

            
            ## PLOT TYPE
            lbl <- glabel("Plot Type :")
            
            plotTypes <- c("default", "dot plot", "histogram")
            plotTypeValues <- list("default", "dot", "hist")
            plotTypeList <- gcombobox(
                plotTypes,
                selected = which(plotTypeValues == curSet$plottype)
                )
            
            addHandlerChanged(plotTypeList, handler = function(h, ...) {
                GUI$getActiveDoc()$setSettings(
                    list(plottype = plotTypeValues[[svalue(plotTypeList, index = TRUE)]])
                    )
                updateSettings()

                plType <- svalue(plotTypeList, index = TRUE)
                if (plType == 3 | (plType == 1 & GUI$plotType == "hist")) {
                    iNZHistogramMod$new(GUI, which = 1)
                }
            })
            
            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- plotTypeList
            ii <- ii + 1


            ## BACKGROUND COLOUR
            lbl <- glabel("Background colour :")
            
            backgroundCols <- c(defts$bg, "antiquewhite",
                                "azure3", "bisque", "cornsilk", "darkolivegreen2",
                                "darkslategray1", "greenyellow", "lightblue1",
                                "lightpink", "rosybrown1", "slategray1", "thistle1",
                                "wheat1")
            backgroundColList <- gcombobox(
                backgroundCols,
                selected = ifelse(
                    is.na(which(backgroundCols == curSet$bg)[1]),
                    1,
                    which(backgroundCols == curSet$bg)[1]),
                editable = TRUE)
            
            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- backgroundColList
            ii <- ii + 1


            ## ## SYMBOL OPTIONS
            ii <- ii + 1
            lbl <- glabel("Symbol options")
            font(lbl) <- list(weight="bold", family = "normal", size = 8)
            tbl[ii, 1:2, anchor = c(-1,-1), expand = TRUE] <- lbl
            ii <- ii + 1

            ## COLOUR            
            lbl <- glabel("Colour :")
            pointCols <- c(defts$col.pt, "darkblue", "darkgreen",
                           "darkmagenta", "darkslateblue", "hotpink4",
                           "lightsalmon2", "palegreen3", "steelblue3")
            symbolColList <- gcombobox(
                pointCols,
                selected = ifelse(
                    is.na(which(pointCols == curSet$col.pt)[1]),
                    1,
                    which(pointCols == curSet$col.pt)[1]),
                editable = TRUE)

            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- symbolColList
            ii <- ii + 1

            lbl <- glabel("NOTE: You can type in a colour if it is not listed.")
            
            ## if the "colby" options is set, i.e. points are colored
            ## according to another var, disable the option to
            ## change the color
            if (!is.null(GUI$getActiveDoc()$getSettings()$colby)) {
                enabled(symbolColList) <- FALSE
                svalue(lbl) <- paste(
                    "Changing the color of symbols is disabled since\n",
                    "the symbols are colored by '",
                    GUI$getActiveDoc()$getSettings()$varnames$colby,
                    "'", sep = "")
            }
            font(lbl) <- list(family = "normal", size = 8)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl                
            ii <- ii + 1

            ## FILL
            fillColor <- gcheckbox("Colour interior",
                                   checked = (curSet$pch != 1))
            tbl[ii,  2, expand = TRUE] <- fillColor
            ii <- ii + 1
            
            ## SIZE
            lbl <- glabel("Size :")
            cexSlider <- gslider(from = 0.05, to = 3.5,
                                 by = 0.05, value = curSet$cex.dotpt)
            tbl[ii, 1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- cexSlider
            ii <- ii + 1
            
            ## Transparency
            lbl <- glabel("Transparency :")
            transpSlider <- gslider(from = 0, to = 100,
                                    by = 1, value = 100 * (1 - curSet$alpha))
            tbl[ii, 1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- transpSlider
            ii <- ii + 1

            updateEverything <- function(update = auto) {
                ## To easily diable automatic updating of plot, add this argument,
                ## otherwise would have to block/unblock handlers
                if (!update)
                    return()
                
                pch.sel <- ifelse(svalue(fillColor) | svalue(transpSlider) > 0,
                                  19, 1)
                GUI$getActiveDoc()$setSettings(
                    list(col.pt = svalue(symbolColList),
                         bg = svalue(backgroundColList),
                         cex.dotpt = svalue(cexSlider),
                         pch = pch.sel,
                         alpha = 1 - svalue(transpSlider) / 100,
                         fill.pt =
                             ifelse(svalue(transpSlider) == 0,
                                    svalue(fillColor),
                                    svalue(symbolColList))
                         ))
                updateSettings()
            }

            ii <- ii + 1
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) updateEverything(TRUE))
            if (auto) {
                bcoltimer <- NULL
                addHandlerChanged(backgroundColList,
                                  handler = function(h, ...) {
                                      if (!is.null(bcoltimer))
                                          bcoltimer$stop_timer()
                                      bcoltimer <- gtimer(500, function(...) {
                                                               if (nchar(svalue(backgroundColList)) >= 3)
                                                                   updateEverything()
                                                           }, one.shot = TRUE)
                                  })

                ## This one needs to be deactivated if user is typing:
                pcoltimer <- NULL
                addHandlerChanged(symbolColList,
                                  handler = function(h, ...) {
                                      if (!is.null(pcoltimer))
                                          pcoltimer$stop_timer()
                                      pcoltimer <- gtimer(500, function(...) {
                                                               if (nchar(svalue(symbolColList)) >= 3)
                                                                   updateEverything()
                                                           }, one.shot = TRUE)
                                  })
 
                addHandlerChanged(fillColor,
                                  handler = function(h, ...) updateEverything())
                
                cextimer <- NULL
                addHandlerChanged(cexSlider,
                                  handler = function(h, ...) {
                                      if (!is.null(cextimer))
                                          cextimer$stop_timer()
                                      cextimer <- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })

                transptimer <- NULL
                addHandlerChanged(transpSlider,
                                  handler = function(h, ...) {
                                      if (!is.null(transptimer))
                                          transptimer$stop_timer()
                                      transptimer <- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })

                autoCheck <- gcheckbox("Update automatically", checked = auto)
                tbl[ii, 1:2, expand = TRUE] <- autoCheck
                ii <- ii + 1

                addHandlerChanged(autoCheck, handler = function(h, ...) {
                                                 enabled(showButton) <- !svalue(autoCheck)
                                                 auto <<- svalue(autoCheck)
                                             })
            }
            
            
            tbl[ii, 1:2, expand = TRUE] <- showButton
            enabled(showButton) <- !auto
            
            add(optGrp, tbl)
        },
        opt3 = function() {
            iNZLocatePoints(TRUE)
            return()
            
            if (attr(GUI$curPlot, "nplots") > 1 | !is.null(curSet$y)) {
                tbl1 <- glayout()
                tbl1[1, 1] <- glabel("Cannot identify points for this type of plot.")
                add(optGrp, tbl1)
                return()
            }
                
            ## Do checking first
            ## If g1 or g2 = _MULTI, then we can't identify points (yet ...)
            cantDo <- function(msg = "using subsetting variables.") {
                gmessage(paste("Cannot identify points when", msg),
                         icon = "error", title = "Unable to identify",
                         parent = modWin)
                return()
            }
            

            lbl1 <- "Select variable to identify:"
            font(lbl1) <- list(weight="bold", family = "normal")
            varmenu <- gcombobox(c("id", names(GUI$getActiveData())), selected = 1)

            
            lbl2 <- "Select points by: "
            selOpts <- gradio(c("mouse click", "min/max N points"),
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

                                       
                                        
                                        seekViewport("VP:locate.these.points")

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

                                        order <- attr(GUI$curPlot[[1]][[1]]$toplot[[1]], "order")
                                        d$v <- d$v[order]
                                        
                                        seekViewport("VP:plotregion")  # need correct coordinate system
                                                                               
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
                                            ## However, need to be careful if only one unique X or Y value:
                                            
                                            if (diff(range(d$x)) == 0)
                                                x.s <- rep(0, length(d$x))
                                            else
                                                x.s <- (d$x - min(d$x)) / (max(d$x) - min(d$x))
                                            
                                            if (diff(range(d$y)) == 0)
                                                y.s <- rep(0, length(d$y))
                                            else
                                                y.s <- (d$y - min(d$y)) / (max(d$y) - min(d$y))
                                            
                                            xy.s <- numeric(2)
                                            if (diff(range(d$x)) == 0)
                                                xy.s[1] <- xy[1]
                                            else
                                                xy.s[1] <- (xy[1] - min(d$x)) / (max(d$x) - min(d$x))
                                            
                                            if (diff(range(d$y)) == 0)
                                                xy.s[2] <- xy[2]
                                            else
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

            lbl <- glabel("Identify Points")
            font(lbl) <- list(weight = "bold", family = "normal", size = 9)
            tbl1[3, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            
            tbl1[4, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl1
            tbl1[4, 2, expand = TRUE, anchor = c(1, 0)] <- varmenu
            tbl1[5, 1, expand = FALSE, anchor = c(1, 1)] <- lbl2
            tbl1[5, 2] <- selOpts

            tbl2[2, 1] <- minPts
            tbl2[2, 2] <- maxPts
            tbl2[2, 3] <- nlbl
            tbl2[2, 4] <- nPts
            
            tbl3[2, 1, expand = TRUE] <- locateButton

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
            ii <- 3
            
            lbl <- glabel("Customize Labels")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            

            curPlSet <- GUI$getActiveDoc()$getSettings()
            oldMain <- curPlSet$main
            oldX <- curPlSet$xlab
            if (is.null(oldMain)) oldMain <- ''
            if (is.null(oldX)) oldX <- ''

            lbl    <- glabel("Main title :")
            labMain <- gedit(oldMain)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labMain
            ii <- ii + 1
            
            lbl    <- glabel("x-axis label :")
            labX    <- gedit(oldX)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labX
            ii <- ii + 1
            

            lbl <- glabel("Enter a single space to print no label\nLeave blank to print default label")
            font(lbl) <- list(family = "normal",
                               size = 8)
            tbl[ii, 2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            
            lbl <- glabel("Press ENTER/RETURN to apply changes")
            font(lbl) <- list(family = "normal", size = 8)
            tbl[ii, 2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 2

            
            lbl <- glabel("Group labels (\"y-axis\")")
            font(lbl) <- list(weight = "bold", family = "normal", size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            intLabs <- gcheckbox("Display inside graph (if labels aren't showing, check this box)",
                                 checked = !curSet$internal.labels)
            tbl[ii, 1:2, anchor = c(-1, -1), expand= TRUE] <- intLabs
            ii <- ii + 1

            
            updateEverything <- function() {
                mlab <- svalue(labMain)
                xlab <- svalue(labX)
                GUI$getActiveDoc()$setSettings(
                    list(main = if (mlab != '') mlab else NULL,
                         xlab = if (xlab != '') xlab else NULL,
                         internal.labels = !svalue(intLabs))
                    )
                updateSettings()
            }
            
            addHandlerChanged(labMain, handler = function(h, ...) updateEverything())
            addHandlerChanged(labX, handler = function(h, ...) updateEverything())
            addHandlerClicked(intLabs, handler = function(h, ...) updateEverything())
            
            add(optGrp, tbl)
        },
        ## Adjust axis limits
        opt5 = function(){
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Adjust Axis Limits")
            font(lbl) <- list(weight="bold", family = "normal", size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            pl <- GUI$curPlot
            xlim <- if (is.null(curSet$xlim))
                pl$xlim
            else
                curSet$xlim
           
            lbl <- glabel("x-axis: ")
            xlower <- gedit(xlim[1])
            xupper <- gedit(xlim[2])
            tbl[ii, 1, expand = TRUE, fill = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- xlower
            tbl[ii, 3, expand = TRUE] <- xupper
            ii <- ii + 1


            errlbl <- glabel("Limits must be numbers.")
            tbl[ii, 1:3] <- errlbl
            visible(errlbl) <- FALSE

            updateEverything <- function() {
                err <- FALSE
                xl <- suppressWarnings(as.numeric(svalue(xlower)))
                if (is.na(xl)) {
                    xl <- xlim[1]
                    err <- TRUE
                }
                xu <- suppressWarnings(as.numeric(svalue(xupper)))
                if (is.na(xu)) {
                    xu <- xlim[2]
                    err <- TRUE
                }

                visible(errlbl) <- err
                    
                ## update plot settings
                GUI$getActiveDoc()$setSettings(
                    list(xlim = c(xl, xu))
                    )
                updateSettings()
            }

            timer <- NULL
            updT <- function(h, ...) {
                if (!is.null(timer))
                    timer$stop_timer()
                timer <- gtimer(800, function(...) updateEverything(), one.shot = TRUE)
            }
            addHandlerKeystroke(xlower, updT)
            addHandlerKeystroke(xupper, updT)
            
            add(optGrp, tbl)

            resetGrp <- ggroup(cont = optGrp)
            addSpring(resetGrp)
            resetbtn <- gbutton("Reset", cont = resetGrp)
            addHandlerClicked(resetbtn, function(h, ...) {
                GUI$getActiveDoc()$setSettings(
                    list(xlim = NULL)
                    )
                updateSettings()

                ## reset the values in the boxes:
                pl <- GUI$curPlot
                xlim <-pl$xlim

                svalue(xlower) <- xlim[1]
                svalue(xupper) <- xlim[2]
            })
        })
    )



### ---------------------------------------------------------------------------------------------------
### HISOGRAM MOD WINDOW

iNZHistogramMod <- setRefClass(
    "iNZHistogramMod",
    contains = "iNZPlotModWin",
    methods = list(
        initialize = function(GUI, which = 1) {
            callSuper(GUI)
            ## need to specify the methods that we want to use in
            ## do.call later on (see changeOpts())
            usingMethods(opt1, opt2, opt3)
            opts <- gcombobox(c("Change plot appearance",
                                "Customize Labels",
                                "Adjust axis limits"),
                              selected = which)
            add(radioGrp, opts, expand = TRUE, fill = TRUE)
            eval(parse(text = paste0("opt", which, "()")))
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

        ## Change Plot appearance
        opt1 = function() {
            tbl <- glayout()
            ii <- 3

            ## Default settings
            defts <- iNZightPlots:::inzpar()
            
            ## PLOT APPEARANCE
            lbl <- glabel("Change plot appearance")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii,  1:2, anchor = c(-1,-1), expand = TRUE] <- lbl
            ii <- ii + 1

            
            ## PLOT TYPE
            lbl <- glabel("Plot Type :")
            
            plotTypes <- c("default", "dot plot", "histogram")
            plotTypeValues <- list("default", "dot", "hist")
            plotTypeList <- gcombobox(
                plotTypes,
                selected = which(plotTypeValues == curSet$plottype)
                )
            
            addHandlerChanged(plotTypeList, handler = function(h, ...) {
                GUI$getActiveDoc()$setSettings(
                    list(plottype = plotTypeValues[[svalue(plotTypeList, index = TRUE)]])
                    )
                updateSettings()

                plType <- svalue(plotTypeList, index = TRUE)
                if (plType == 2 | (plType == 1 & GUI$plotType == "dot")) {
                    iNZDotchartMod$new(GUI, which = 2)
                }
            })
            
            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- plotTypeList
            ii <- ii + 1


            ## BACKGROUND COLOUR
            lbl <- glabel("Background colour :")
            
            backgroundCols <- c(defts$bg, "antiquewhite",
                                "azure3", "bisque", "cornsilk", "darkolivegreen2",
                                "darkslategray1", "greenyellow", "lightblue1",
                                "lightpink", "rosybrown1", "slategray1", "thistle1",
                                "wheat1")
            backgroundColList <- gcombobox(
                backgroundCols,
                selected = ifelse(
                    is.na(which(backgroundCols == curSet$bg)[1]),
                    1,
                    which(backgroundCols == curSet$bg)[1]),
                editable = TRUE)
            
            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- backgroundColList
            ii <- ii + 1
            

            ## COLOUR            
            lbl <- glabel("Bar colour :")
            barCols <- c(defts$bar.fill, "darkblue", "darkgreen",
                           "darkmagenta", "darkslateblue", "hotpink4",
                           "lightsalmon2", "palegreen3", "steelblue3")
            barColList <- gcombobox(
                barCols,
                selected = ifelse(
                    is.na(which(barCols == curSet$bar.fill)[1]),
                    1,
                    which(barCols == curSet$bar.fill)[1]),
                editable = TRUE)

            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- barColList
            ii <- ii + 1

            lbl <- glabel("NOTE: You can type in a colour if it is not listed.")
            
            font(lbl) <- list(family = "normal", size = 8)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl                
            ii <- ii + 1

            
            ## Number of bars
            ii <- ii + 1
            lbl <- glabel("Number of bars :")

            adjBins <- gcheckbox("Manually adjust the number of bars",
                                 checked = !is.null(curSet$hist.bins))
            binSlider <- gslider(from = 5, to = 200,
                                 by = 1, value = curSet$hist.bins)
            enabled(binSlider) <- svalue(adjBins)

            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- adjBins
            ii <- ii + 1
            
            tbl[ii, 1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- binSlider
            ii <- ii + 1
            

            updateEverything <- function(update = auto) {
                ## To easily diable automatic updating of plot, add this argument,
                ## otherwise would have to block/unblock handlers
                if (!update)
                    return()
                
                GUI$getActiveDoc()$setSettings(
                    list(bar.fill = svalue(barColList),
                         bg = svalue(backgroundColList),
                         hist.bins = if (svalue(adjBins)) svalue(binSlider) else NULL
                         ))
                updateSettings()
            }

            ii <- ii + 1
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) updateEverything(TRUE))
            if (auto) {
                bcoltimer <- NULL
                addHandlerChanged(backgroundColList,
                                  handler = function(h, ...) {
                                      if (!is.null(bcoltimer))
                                          bcoltimer$stop_timer()
                                      bcoltimer <- gtimer(500, function(...) {
                                                               if (nchar(svalue(backgroundColList)) >= 3)
                                                                   updateEverything()
                                                           }, one.shot = TRUE)
                                  })

                ## This one needs to be deactivated if user is typing:
                pcoltimer <- NULL
                addHandlerChanged(barColList,
                                  handler = function(h, ...) {
                                      if (!is.null(pcoltimer))
                                          pcoltimer$stop_timer()
                                      pcoltimer <- gtimer(500, function(...) {
                                                               if (nchar(svalue(barColList)) >= 3)
                                                                   updateEverything()
                                                           }, one.shot = TRUE)
                                  })

                addHandlerChanged(adjBins,
                                  handler = function(h, ...) {
                                      enabled(binSlider) <- svalue(adjBins)
                                      updateEverything()
                                  })
                
                cextimer <- NULL
                addHandlerChanged(binSlider,
                                  handler = function(h, ...) {
                                      if (!is.null(cextimer))
                                          cextimer$stop_timer()
                                      cextimer <- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })

                autoCheck <- gcheckbox("Update automatically", checked = auto)
                tbl[ii, 1:2, expand = TRUE] <- autoCheck
                ii <- ii + 1

                addHandlerChanged(autoCheck, handler = function(h, ...) {
                                                 enabled(showButton) <- !svalue(autoCheck)
                                                 auto <<- svalue(autoCheck)
                                             })
            }
            
            
            tbl[ii, 1:2, expand = TRUE] <- showButton
            enabled(showButton) <- !auto
            
            add(optGrp, tbl)
        },
        opt2 = function() {
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Customize Labels")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            

            curPlSet <- GUI$getActiveDoc()$getSettings()
            oldMain <- curPlSet$main
            oldX <- curPlSet$xlab
            if (is.null(oldMain)) oldMain <- ''
            if (is.null(oldX)) oldX <- ''

            lbl    <- glabel("Main title :")
            labMain <- gedit(oldMain)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labMain
            ii <- ii + 1
            
            lbl    <- glabel("x-axis label :")
            labX    <- gedit(oldX)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labX
            ii <- ii + 1
            

            lbl <- glabel("Enter a single space to print no label\nLeave blank to print default label")
            font(lbl) <- list(family = "normal",
                               size = 8)
            tbl[ii, 2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            
            lbl <- glabel("Press ENTER/RETURN to apply changes")
            font(lbl) <- list(family = "normal", size = 8)
            tbl[ii, 2, anchor = c(-1, -1), expand = TRUE] <- lbl

            
            lbl <- glabel("Group labels (\"y-axis\")")
            font(lbl) <- list(weight = "bold", family = "normal", size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            intLabs <- gcheckbox("Display inside graph (if labels aren't showing, check this box)",
                                 checked = !curSet$internal.labels)
            tbl[ii, 1:2, anchor = c(-1, -1), expand= TRUE] <- intLabs
            ii <- ii + 1

            
            updateEverything <- function() {
                mlab <- svalue(labMain)
                xlab <- svalue(labX)
                GUI$getActiveDoc()$setSettings(
                    list(main = if (mlab != '') mlab else NULL,
                         xlab = if (xlab != '') xlab else NULL,
                         internal.labels = !svalue(intLabs))
                    )
                updateSettings()
            }
            
            addHandlerChanged(labMain, handler = function(h, ...) updateEverything())
            addHandlerChanged(labX, handler = function(h, ...) updateEverything())
            addHandlerClicked(intLabs, handler = function(h, ...) updateEverything())
            
            add(optGrp, tbl)
        },
        ## Adjust axis limits
        opt3 = function(){
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Adjust Axis Limits")
            font(lbl) <- list(weight="bold", family = "normal", size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            pl <- GUI$curPlot
            xlim <- if (is.null(curSet$xlim))
                        pl$xlim
                    else
                        curSet$xlim
           
            lbl <- glabel("x-axis: ")
            xlower <- gedit(xlim[1])
            xupper <- gedit(xlim[2])
            tbl[ii, 1, expand = TRUE, fill = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- xlower
            tbl[ii, 3, expand = TRUE] <- xupper
            ii <- ii + 1


            errlbl <- glabel("Limits must be numbers.")
            tbl[ii, 1:3] <- errlbl
            visible(errlbl) <- FALSE

            updateEverything <- function() {
                err <- FALSE
                xl <- suppressWarnings(as.numeric(svalue(xlower)))
                if (is.na(xl)) {
                    xl <- xlim[1]
                    err <- TRUE
                }
                xu <- suppressWarnings(as.numeric(svalue(xupper)))
                if (is.na(xu)) {
                    xu <- xlim[2]
                    err <- TRUE
                }

                visible(errlbl) <- err
                    
                ## update plot settings
                GUI$getActiveDoc()$setSettings(
                    list(xlim = c(xl, xu))
                    )
                updateSettings()
            }

            timer <- NULL
            updT <- function(h, ...) {
                if (!is.null(timer))
                    timer$stop_timer()
                timer <- gtimer(800, function(...) updateEverything(), one.shot = TRUE)
            }
            addHandlerKeystroke(xlower, updT)
            addHandlerKeystroke(xupper, updT)
            
            add(optGrp, tbl)

            resetGrp <- ggroup(cont = optGrp)
            addSpring(resetGrp)
            resetbtn <- gbutton("Reset", cont = resetGrp)
            addHandlerClicked(resetbtn, function(h, ...) {
                GUI$getActiveDoc()$setSettings(
                    list(xlim = NULL)
                    )
                updateSettings()

                ## reset the values in the boxes:
                pl <- GUI$curPlot
                xlim <-pl$xlim

                svalue(xlower) <- xlim[1]
                svalue(xupper) <- xlim[2]
            })
        })
    )





### ---------------------------------------------------------------------------------------------------
### BAR PLOT MOD WINDOW

iNZBarchartMod <- setRefClass(
    "iNZBarchartMod",
    contains = "iNZPlotModWin",
    methods = list(
        initialize = function(GUI, which = 1) {
            callSuper(GUI)
            ## need to specify the methods that we want to use in
            ## do.call later on (see changeOpts())

            if (is.null(curSet$y)) {
                usingMethods(opt1, opt2, opt3, opt4)
                opts <- gcombobox(c("Code more variables",
                                    "Change plot appearance",
                                    "Customize Labels",
                                    "Adjust number of Bars"),
                                  selected = which)
            } else {
                usingMethods(opt2, opt3, opt4)
                which <- ifelse(which == 1, 2, which)
                opts <- gcombobox(c("Change plot appearance",
                                    "Customize Labels",
                                    "Adjust number of Bars"),
                                  selected = which - 1)
            }
            add(radioGrp, opts, fill = TRUE, expand = TRUE)
            eval(parse(text = paste0("opt", which, "()")))
            addHandlerChanged(opts,
                              handler = function(h, ...) {
                                  changeOpts(svalue(h$obj, index = TRUE) + !is.null(curSet$y))
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
            ii <- 3
            
            lbl1 <- glabel("Code More Variables")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            ii <- ii + 1
            
            lbl2 <- glabel("Colour by levels of :")
            grpVarList <- gcombobox(c("",
                                      names(GUI$getActiveData())[sapply(GUI$getActiveData(),
                                                                        is.factor)]),
                                    selected = ifelse(
                                        is.null(curSet$colby),
                                        1, which(names(GUI$getActiveData())[sapply(GUI$getActiveData(),
                                                                                   is.factor)] ==
                                                     curSet$varnames$colby)[1] + 1
                                        )
                                    )
            
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl2
            tbl[ii, 2, expand = TRUE] <- grpVarList
            ii <- ii + 1


            ## Maintain a single function that is called whenever anything is updated:
            updateEverything <- function() {
                GUI$getActiveDoc()$setSettings(
                    list(colby = GUI$getActiveData()[[
                             svalue(grpVarList)]],
                         varnames = list(
                             colby = svalue(grpVarList)))
                    )
                updateSettings()
            }
            
            ## in this case, no point in having a separate "show" button
            addHandlerChanged(grpVarList,
                              handler = function(h, ...) {
                                  updateEverything()
                              })
            
            add(optGrp, tbl)
        },
        ## Change Plot appearance
        opt2 = function() {
            tbl <- glayout()
            ii <- 3

            ## Default settings
            defts <- iNZightPlots:::inzpar()
            
            ## PLOT APPEARANCE
            lbl <- glabel("Change plot appearance")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii,  1:2, anchor = c(-1,-1), expand = TRUE] <- lbl
            ii <- ii + 1


            ## BACKGROUND COLOUR
            lbl <- glabel("Background colour :")
            
            backgroundCols <- c(defts$bg, "antiquewhite",
                                "azure3", "bisque", "cornsilk", "darkolivegreen2",
                                "darkslategray1", "greenyellow", "lightblue1",
                                "lightpink", "rosybrown1", "slategray1", "thistle1",
                                "wheat1")
            backgroundColList <- gcombobox(
                backgroundCols,
                selected = ifelse(
                    is.na(which(backgroundCols == curSet$bg)[1]),
                    1,
                    which(backgroundCols == curSet$bg)[1]),
                editable = TRUE)
            
            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- backgroundColList
            ii <- ii + 1


            ## COLOUR
            lbl <- glabel("Bar colour :")
            barCols <- c(defts$bar.fill, "darkblue", "darkgreen",
                         "darkmagenta", "darkslateblue", "hotpink4",
                         "lightsalmon2", "palegreen3", "steelblue3")
            barColList <- gcombobox(
                barCols,
                selected = ifelse(
                    is.na(which(barCols == curSet$bar.fill)[1]),
                    1,
                    which(barCols == curSet$bar.fill)[1]),
                editable = TRUE)

            if (is.null(curSet$y)) {
                tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
                tbl[ii,  2, expand = TRUE] <- barColList
                ii <- ii + 1
            }

            lbl <- glabel("NOTE: You can type in a colour if it is not listed.")

            if (is.null(curSet$y)) {
                ## if the "colby" options is set, i.e. points are colored
                ## according to another var, disable the option to
                ## change the color
                if (!is.null(GUI$getActiveDoc()$getSettings()$colby)) {
                    enabled(barColList) <- FALSE
                    svalue(lbl) <- paste(
                        "Changing the bar color is disabled since\n",
                        "the bars are colored by '",
                        GUI$getActiveDoc()$getSettings()$varnames$colby,
                        "'", sep = "")
                }
                font(lbl) <- list(family = "normal", size = 8)
                tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl                
                ii <- ii + 1
            }
            
            updateEverything <- function(update = auto) {
                ## To easily diable automatic updating of plot, add this argument,
                ## otherwise would have to block/unblock handlers
                if (!update)
                    return()
                
                GUI$getActiveDoc()$setSettings(
                    list(bar.fill = svalue(barColList),
                         bg = svalue(backgroundColList)
                         ))
                updateSettings()
            }

            ii <- ii + 1

            bcoltimer <- NULL
            addHandlerChanged(backgroundColList,
                              handler = function(h, ...) {
                                  if (!is.null(bcoltimer))
                                      bcoltimer$stop_timer()
                                  bcoltimer <- gtimer(500, function(...) {
                                                          if (nchar(svalue(backgroundColList)) >= 3)
                                                              updateEverything()
                                                      }, one.shot = TRUE)
                              })

            if (is.null(curSet$y)) {
                pcoltimer <- NULL
                addHandlerChanged(barColList,
                                  handler = function(h, ...) {
                                      if (!is.null(pcoltimer))
                                          pcoltimer$stop_timer()
                                      pcoltimer <- gtimer(500, function(...) {
                                                              if (nchar(svalue(barColList)) >= 3)
                                                                  updateEverything()
                                                          }, one.shot = TRUE)
                                  })
            }
            
            add(optGrp, tbl)
        },
        opt3 = function() {
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Customize Labels")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            

            curPlSet <- GUI$getActiveDoc()$getSettings()
            oldMain <- curPlSet$main
            oldX <- curPlSet$xlab
            if (is.null(oldMain)) oldMain <- ''
            if (is.null(oldX)) oldX <- ''

            lbl    <- glabel("Main title :")
            labMain <- gedit(oldMain)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labMain
            ii <- ii + 1
            
            lbl    <- glabel("x-axis label :")
            labX    <- gedit(oldX)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labX
            ii <- ii + 1
            

            lbl <- glabel("Enter a single space to print no label\nLeave blank to print default label")
            font(lbl) <- list(family = "normal",
                               size = 8)
            tbl[ii, 2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            
            lbl <- glabel("Press ENTER/RETURN to apply changes")
            font(lbl) <- list(family = "normal", size = 8)
            tbl[ii, 2, anchor = c(-1, -1), expand = TRUE] <- lbl

            
            updateEverything <- function() {
                mlab <- svalue(labMain)
                xlab <- svalue(labX)
                GUI$getActiveDoc()$setSettings(
                    list(main = if (mlab != '') mlab else NULL,
                         xlab = if (xlab != '') xlab else NULL)
                    )
                updateSettings()
            }
            
            addHandlerChanged(labMain, handler = function(h, ...) updateEverything())
            addHandlerChanged(labX, handler = function(h, ...) updateEverything())
            
            add(optGrp, tbl)
        },
        ## Adjust number of bars visible
        opt4 = function(){
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Adjust Number of Bars")
            font(lbl) <- list(weight="bold", family = "normal", size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            zoom <- if (!is.null(curSet$zoombars))
                        curSet$zoombars
                    else
                        NULL
            
            lbl <- glabel("Number of bars: ")
            NBARS <- gslider(2, min(30, length(levels(curSet$x))), by = 1, value = min(30, length(levels(curSet$x))))
            tbl[ii, 1, expand = TRUE, fill = TRUE, anchor = c(-1, 0)] <- lbl
            tbl[ii, 2, expand = TRUE] <- NBARS
            ii <- ii + 1

            lbl <- glabel("Starting point: ")
            START <- gslider(levels(curSet$x)[1:(length(levels(curSet$x)) - 1)])
            tbl[ii, 1, expand = TRUE, fill = TRUE, anchor = c(-1, 0)] <- lbl
            tbl[ii, 2, expand = TRUE] <- START
            ii <- ii + 1         
            
            updateEverything <- function() {
                ## update plot settings
                GUI$getActiveDoc()$setSettings(
                    list(zoombars = c(svalue(START, index = TRUE), svalue(NBARS)))
                    )
                updateSettings()
            }

            addHandlerChanged(NBARS, function(h, ...) updateEverything())
            addHandlerChanged(START, function(h, ...) updateEverything())

            if (!is.null(zoom)) {
                svalue(NBARS) <- zoom[2]
                svalue(START, index = TRUE) <- zoom[1]
            }

            ## timer <- NULL
            ## updT <- function(h, ...) {
            ##     if (!is.null(timer))
            ##         timer$stop_timer()
            ##     timer <- gtimer(800, function(...) updateEverything(), one.shot = TRUE)
            ## }
            ## addHandlerKeystroke(xlower, updT)z
            ## addHandlerKeystroke(xupper, updT)
            
            add(optGrp, tbl)

            resetGrp <- ggroup(cont = optGrp)
            addSpring(resetGrp)
            resetbtn <- gbutton("Reset", cont = resetGrp)
            addHandlerClicked(resetbtn, function(h, ...) {
                GUI$getActiveDoc()$setSettings(
                    list(zoombars = NULL)
                    )
                updateSettings()

                ## reset the values in the boxes:
                ## pl <- GUI$curPlot
                ## xlim <-pl$xlim

                ## svalue(xlower) <- xlim[1]
                ## svalue(xupper) <- xlim[2]
            })

            ## We want to instantly display the results ...
            updateEverything()
        })
    )



### ---------------------------------------------------------------------------------------------------
### SCATTER PLOT MOD WINDOW

iNZScatterMod <- setRefClass(
    "iNZScatterMod",
    contains = "iNZPlotModWin",
    methods = list(
        initialize = function(GUI, which = 1) {
            callSuper(GUI)
            ## need to specify the methods that we want to use in
            ## do.call later on (see changeOpts())
            usingMethods(opt1, opt2, opt3, opt4, opt5, opt6, opt7, opt8, opt9, opt10, iNZLocatePoints)
            opts <- gcombobox(c("Code more variables",
                                "Add trend curves",
                                "Add x=y line",
                                "Add a jitter",
                                "Add rugs",
                                "Join points by lines",
                                "Change plot appearance",
                                "Identify points",
                                "Customize Labels",
                                "Adjust axis limits"),
                              selected = which)
            add(radioGrp, opts, expand = TRUE, fill = TRUE)
            eval(parse(text = paste0("opt", which, "()")))
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
            ii <- 3
            
            lbl1 <- glabel("Code More Variables")
            font(lbl1) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl1
            ii <- ii + 1
            
            lbl <- glabel("Colour by levels of :")
            grpVarList <- gcombobox(c("", names(GUI$getActiveData())),
                                    selected = ifelse(
                                        is.null(curSet$colby),
                                        1, which(names(GUI$getActiveData()) ==
                                                     curSet$varnames$colby)[1] + 1
                                        )
                                    )
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- grpVarList
            ii <- ii + 1

            lvlCols <- gbutton("Specify colours")
            tbl[ii, 2, expand = TRUE] <- lvlCols
            visible(lvlCols) <- svalue(grpVarList, index = TRUE) != 1
            ii <- ii + 1

            addHandlerClicked(lvlCols, function(h, ...) {
                variable <- GUI$getActiveData()[, svalue(grpVarList, index = FALSE)]
                if (is.numeric(variable)) {
                    gmessage("Set colour of numeric ... not yet implemented.", "Not ready yet.", icon = "warning")
                } else {
                    specifyColours(variable)
                }                
            })


            lbl <- glabel("Resize points proportional to :")
            rszVarList <- gcombobox(
                c("", rszNames <- names(GUI$getActiveData())[sapply(GUI$getActiveData(), is.numeric)]),
                selected = ifelse(
                    is.null(curSet$sizeby),
                    1, which(rszNames == curSet$varnames$sizeby)[1] + 1
                    )
                )
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- rszVarList
            ii <- ii + 1


            ## Maintain a single function that is called whenever anything is updated:
            updateEverything <- function() {
                GUI$getActiveDoc()$setSettings(
                    list(colby = GUI$getActiveData()[[
                             svalue(grpVarList)]],
                         sizeby = GUI$getActiveData()[[
                             svalue(rszVarList)]],
                         varnames = list(
                             colby = svalue(grpVarList),
                             sizeby = svalue(rszVarList)))
                    )
                updateSettings()
            }
            
            ## in this case, no point in having a separate "show" button
            addHandlerChanged(grpVarList, handler = function(h, ...) updateEverything())
            addHandlerChanged(rszVarList, handler = function(h, ...) updateEverything())

            addHandlerChanged(grpVarList,
                              handler = function(h, ...) {
                                  updateEverything()
                                  visible(lvlCols) <- svalue(grpVarList, index = TRUE) != 1 &&
                                      is.factor(GUI$getActiveData()[[svalue(grpVarList)]])
                              })
                              
            add(optGrp, tbl)
        },
        ## Add trend curves
        opt2 = function() {
            tbl <- glayout()
            ii <- 3

            #### TREND CURVES
            lbl <- glabel("Add trend curves")
            font(lbl) <- list(weight="bold", family = "normal", size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            
            ## Types of curves possible:
            trCrvs <- c("linear", "quadratic", "cubic")
            trCols <- c("red", "black", "blue", "green4",
                        "yellow", "pink", "grey", "orange")

            ## Linear trend
            linChk <- gcheckbox(trCrvs[1],
                                checked = trCrvs[1] %in% curSet$trend)
            linCol <- gcombobox(trCols,
                                selected = which(
                                    curSet$col.trend$linear == trCols
                                    )
                                )
            tbl[ii, 1] <- linChk
            tbl[ii, 2] <- linCol
            ii <- ii + 1
            
            quaChk <- gcheckbox(trCrvs[2],
                                checked = trCrvs[2] %in% curSet$trend)
            quaCol <- gcombobox(trCols,
                                selected = which(
                                    curSet$col.trend$quadratic == trCols
                                    )
                                )
            tbl[ii, 1] <- quaChk
            tbl[ii, 2] <- quaCol
            ii <- ii + 1
            
            cubChk <- gcheckbox(trCrvs[3],
                                checked = trCrvs[3] %in% curSet$trend)
            cubCol <- gcombobox(trCols,
                                selected = which(
                                    curSet$col.trend$cubic == trCols
                                    )
                                )
            tbl[ii, 1] <- cubChk
            tbl[ii, 2] <- cubCol
            ii <- ii + 1


            ii <- ii + 1
            #### SMOOTHERS
            smthCols <- c("red", "black", "blue", "green", "yellow",
                          "magenta", "grey", "orange")

            
            smthChk <- gcheckbox("Draw a smoother",
                                 checked = curSet$smooth!=0 | !is.null(curSet$quant.smooth))
            smthCol <- gcombobox(smthCols,
                                 selected = which(
                                     curSet$col.smooth == smthCols)
                                 )
            tbl[ii, 1] <- smthChk
            tbl[ii, 2] <- smthCol
            ii <- ii + 1
            
            quantSmthChk <- gcheckbox("Use Quantiles",
                                      checked = !is.null(curSet$quant.smooth))
            tbl[ii, 1] <- quantSmthChk
            ii <- ii + 1

            smthSlid <- gslider(from = 0.1, to = 1,
                                by = 0.01,
                                value = ifelse(curSet$smooth==0,
                                    0.7, curSet$smooth))
            tbl[ii, 1:2] <- smthSlid
            ii <- ii + 1


            ii <- ii + 1
            trendByChk <- gcheckbox(paste("For each level of",
                                            curSet$varnames$colby),
                                      checked = curSet$trend.by)
            trendParChk <- gcheckbox("Parallel trend lines",
                                     checked = curSet$trend.parallel)
            if (!is.null(curSet$colby)) {
                if (is.factor(curSet$colby)) {
                    tbl[ii, 1:2] <- trendByChk
                    ii <- ii + 1
                    tbl[ii, 1:2] <- trendParChk
                    ii <- ii + 1
                }
            }

            updateEverything <- function(update = auto) {
                if (!update)
                    return()
                
                ## vector of selected trends
                trSel <- c(svalue(linChk),
                           svalue(quaChk),
                           svalue(cubChk))
                ## vector of colors chosen
                trCol <- c(svalue(linCol),
                           svalue(quaCol),
                           svalue(cubCol))
                ## smoother option
                qsmth <-
                    if (svalue(quantSmthChk))
                        if (svalue(smthChk))"default" else NULL
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
                         trend.by = svalue(trendByChk),
                         trend.parallel = svalue(trendParChk)
                         )
                    )
                updateSettings()
            }

            ii <- ii + 1            
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) updateEverything(TRUE))
            
            ## only have the smoother slider enabled if the
            ## smoother checkbox is ticked
            if (!svalue(smthChk)) {
                enabled(smthSlid) <- FALSE
                enabled(quantSmthChk) <- FALSE
            } else {
                if (svalue(quantSmthChk))
                    enabled(smthSlid) <- FALSE
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
                updateEverything()
            })
            ## if quantiles are used, disable slider
            addHandlerChanged(quantSmthChk,
                              handler = function(h, ...) {
                                  if (svalue(quantSmthChk)) {
                                      enabled(smthSlid) <- FALSE
                                  }
                                  else {
                                      enabled(smthSlid) <- TRUE
                                  }
                                  updateEverything()
                              })

            ## activate/deactive trend by check box
            ## only have the trend by level option enabled if
            ## the colored by variable option is set
            ## and if lin/quad/cub/or normal smoother is checked
            activateTrendBy <- function() {
                enabled(trendByChk) <-
                    ifelse(is.null(curSet$colby), FALSE, is.factor(curSet$colby)) &
                        (svalue(linChk) | svalue(quaChk) | svalue(cubChk) |
                         (svalue(smthChk) & !svalue(quantSmthChk)))
            }
            activateTrendBy()
            addHandlerChanged(linChk, handler = function(h, ...) {
                                          updateEverything()
                                          activateTrendBy()
                                      })
            addHandlerChanged(quaChk, handler = function(h, ...) {
                                          updateEverything()
                                          activateTrendBy()
                                      })
            addHandlerChanged(cubChk, handler = function(h, ...) {
                                          updateEverything()
                                          activateTrendBy()
                                      })
            addHandlerChanged(smthChk, handler = function(h, ...) {
                                           updateEverything()
                                           activateTrendBy()
                                       })
            addHandlerChanged(quantSmthChk, handler = function(h, ...) {
                                                updateEverything()
                                                activateTrendBy()
                                            })

            
            ## activate/deactivate trend parallel box
            ## only have the "parallel lines" enabled if "trend by" is ticked
            activateTrendPar <- function() {
                enabled(trendParChk) <- svalue(trendByChk) &
                    (svalue(linChk) | svalue(quaChk) | svalue(cubChk))
            }
            activateTrendPar()
            addHandlerChanged(trendByChk, handler = function(h, ...) {
                                              updateEverything()
                                              activateTrendPar()
                                          })
            addHandlerChanged(linChk, handler = function(h, ...) {
                                          updateEverything()
                                          activateTrendPar()
                                      })
            addHandlerChanged(quaChk, handler = function(h, ...) {
                                          updateEverything()
                                          activateTrendPar()
                                      })
            addHandlerChanged(cubChk, handler = function(h, ...) {
                                          updateEverything()
                                          activateTrendPar()
                                      })


            ## Also update the colour things
            if (auto) {
                addHandlerChanged(linCol, handler = function(h, ...) updateEverything())
                addHandlerChanged(quaCol, handler = function(h, ...) updateEverything())
                addHandlerChanged(cubCol, handler = function(h, ...) updateEverything())
                addHandlerChanged(smthCol, handler = function(h, ...) updateEverything())
                addHandlerChanged(trendParChk, handler = function(h, ...) updateEverything())
                

                smthtimer <- NULL
                addHandlerChanged(smthSlid,
                                  handler = function(h, ...) {
                                      if (!is.null(smthtimer))
                                          smthtimer$stop_timer()
                                      smthtimer <- gtimer(800, function(...) updateEverything(), one.shot = TRUE)
                                  })

                autoCheck <- gcheckbox("Update automatically", checked = auto)
                tbl[ii, 1:2, expand = TRUE] <- autoCheck
                ii <- ii + 1

                addHandlerChanged(autoCheck, handler = function(h, ...) {
                                                 enabled(showButton) <- !svalue(autoCheck)
                                                 auto <<- svalue(autoCheck)
                                             })
            }

            tbl[ii, 1:2, expand = TRUE] <- showButton
            enabled(showButton) <- !auto

            add(optGrp, tbl)
        },
        ## Add x=y line
        opt3 = function(){
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Add x=y line")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            
            xyline <- gcheckbox("Plot x=y line",
                                checked = curSet$LOE)
            xyCols <- c("red", "black", "blue", "green4",
                        "yellow", "pink", "grey", "orange")
            xyCol <- gcombobox(xyCols,
                               selected = which(
                                   curSet$col.LOE == xyCols
                                   )
                               )
            tbl[ii, 1, expand = TRUE] <- xyline
            tbl[ii, 2, expand = TRUE] <- xyCol
            ii <- ii + 1

            updateEverything <- function() {
                ## update plot settings
                GUI$getActiveDoc()$setSettings(
                    list(LOE = svalue(xyline),
                         col.LOE = svalue(xyCol))
                    )
                updateSettings()
            }

            addHandlerChanged(xyline, handler = function(h, ...) updateEverything())
            addHandlerChanged(xyCol, handler = function(h, ...) updateEverything())
            
            add(optGrp, tbl)
        },
        ## Add jitter
        opt4 = function() {
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Add a jitter")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            
            xJit <- gcheckbox("Jitter x-variable",
                              checked = curSet$jitter %in% c("x", "xy"))
            yJit <- gcheckbox("Jitter y-variable",
                              checked = curSet$jitter %in% c("y", "xy"))
            tbl[ii, 1, expand = TRUE] <- xJit
            tbl[ii, 2, expand = TRUE] <- yJit
            ii <- ii + 1

            updateEverything <- function() {
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
            }

            addHandlerChanged(xJit, handler = function(h, ...) updateEverything())
            addHandlerChanged(yJit, handler = function(h, ...) updateEverything())
            
            add(optGrp, tbl)
        },
        ## Add rug
        opt5 = function() {
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Add rugs")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            
            xRug <- gcheckbox("Add x-rug",
                              checked = curSet$rug %in% c("x", "xy"))
            yRug <- gcheckbox("Add y-rug",
                              checked = curSet$rug %in% c("y", "xy"))
            tbl[ii, 1, expand = TRUE] <- xRug
            tbl[ii, 2, expand = TRUE] <- yRug
            ii <- ii + 1

            updateEverything <- function() {
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
            }

            addHandlerChanged(xRug, handler = function(h, ...) updateEverything())
            addHandlerChanged(yRug, handler = function(h, ...) updateEverything())
            
            add(optGrp, tbl)
        },
        ## Join points by lines
        opt6 = function(){
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Join points by lines")
            font(lbl) <- list(weight="bold", family = "normal", size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            
            joinPts <- gcheckbox("Join points",
                                 checked = curSet$join)
            joinCols <- c("red", "black", "blue", "green4",
                          "yellow", "pink", "grey", "orange")
            joinCol <- gcombobox(joinCols,
                               selected = which(
                                   curSet$col.line == joinCols
                                   )
                               )
            tbl[ii, 1, expand = TRUE] <- joinPts
            tbl[ii, 2, expand = TRUE] <- joinCol
            ii <- ii + 1

            lineByChk <- gcheckbox(paste("For each level of",
                                         curSet$varnames$colby),
                                   selected = curSet$lines.by)
            if (!is.null(curSet$colby)) {
                tbl[ii, 1] <- lineByChk
                ii <- 1
            }

            updateEverything <- function() {
                ## update plot settings
                GUI$getActiveDoc()$setSettings(
                    list(join = svalue(joinPts),
                         col.line = svalue(joinCol),
                         lines.by = svalue(lineByChk))
                    )
                updateSettings()
            }
            
            addHandlerChanged(joinPts, handler = function(h, ...) updateEverything())
            addHandlerChanged(joinCol, handler = function(h, ...) updateEverything())
            addHandlerChanged(lineByChk, handler = function(h, ...) updateEverything())
            
            add(optGrp, tbl)
        },
        ## change plot appearance
        opt7 = function() {
            tbl <- glayout()
            ii <- 3

            ## Default settings
            defts <- iNZightPlots:::inzpar()
            
            ## PLOT APPEARANCE
            lbl <- glabel("Change plot appearance")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii,  1:2, anchor = c(-1,-1), expand = TRUE] <- lbl
            ii <- ii + 1

            
            ## PLOT TYPE
            lbl <- glabel("Plot Type :")
            
            plotTypes <- c("default", "scatter plot", "grid-density plot", "hexbin plot")
            plotTypeValues <- list("default", "scatter", "grid", "hex")
            plotTypeList <- gcombobox(
                plotTypes,
                selected = which(plotTypeValues == curSet$plottype)
                )
            
            addHandlerChanged(plotTypeList, handler = function(h, ...) {
                GUI$getActiveDoc()$setSettings(
                    list(plottype = plotTypeValues[[svalue(plotTypeList, index = TRUE)]])
                    )
                updateSettings()

                plType <- svalue(plotTypeList, index = TRUE)
                if (plType == 3 | plType == 4 | (plType == 1 & GUI$plotType != "scatter")) {
                    switch(GUI$plotType,
                           "grid" = iNZGriddenMod$new(GUI, which = 3),
                           "hex" = iNZHexbinMod$new(GUI, which = 3))
                }
            })
            
            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- plotTypeList
            ii <- ii + 1


            ## BACKGROUND COLOUR
            lbl <- glabel("Background colour :")
            
            backgroundCols <- c(defts$bg, "antiquewhite",
                                "azure3", "bisque", "cornsilk", "darkolivegreen2",
                                "darkslategray1", "greenyellow", "lightblue1",
                                "lightpink", "rosybrown1", "slategray1", "thistle1",
                                "wheat1")
            backgroundColList <- gcombobox(
                backgroundCols,
                selected = ifelse(
                    is.na(which(backgroundCols == curSet$bg)[1]),
                    1,
                    which(backgroundCols == curSet$bg)[1]),
                editable = TRUE)
            
            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- backgroundColList
            ii <- ii + 1


            ## ## SYMBOL OPTIONS
            ii <- ii + 1
            lbl <- glabel("Symbol options")
            font(lbl) <- list(weight="bold", family = "normal", size = 8)
            tbl[ii, 1:2, anchor = c(-1,-1), expand = TRUE] <- lbl
            ii <- ii + 1

            ## COLOUR            
            lbl <- glabel("Colour :")
            pointCols <- c(defts$col.pt, "darkblue", "darkgreen",
                           "darkmagenta", "darkslateblue", "hotpink4",
                           "lightsalmon2", "palegreen3", "steelblue3")
            symbolColList <- gcombobox(
                pointCols,
                selected = ifelse(
                    is.na(which(pointCols == curSet$col.pt)[1]),
                    1,
                    which(pointCols == curSet$col.pt)[1]),
                editable = TRUE)

            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- symbolColList
            ii <- ii + 1

            lbl <- glabel("NOTE: You can type in a colour if it is not listed.")
            
            ## if the "colby" options is set, i.e. points are colored
            ## according to another var, disable the option to
            ## change the color
            if (!is.null(GUI$getActiveDoc()$getSettings()$colby)) {
                enabled(symbolColList) <- FALSE
                svalue(lbl) <- paste(
                    "Changing the color of symbols is disabled since\n",
                    "the symbols are colored by '",
                    GUI$getActiveDoc()$getSettings()$varnames$colby,
                    "'", sep = "")
            }
            font(lbl) <- list(family = "normal", size = 8)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl                
            ii <- ii + 1

            ## FILL
            fillColor <- gcheckbox("Colour interior",
                                   checked = (curSet$pch != 1))
            tbl[ii,  2, expand = TRUE] <- fillColor
            ii <- ii + 1
            
            ## SIZE
            lbl <- glabel("Size :")
            cexSlider <- gslider(from = 0.05, to = 3.5,
                                 by = 0.05, value = curSet$cex.pt)
            tbl[ii, 1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- cexSlider
            ii <- ii + 1
            
            ## Transparency
            lbl <- glabel("Transparency :")
            transpSlider <- gslider(from = 0, to = 100,
                                    by = 1, value = 100 * (1 - curSet$alpha))
            tbl[ii, 1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- transpSlider
            ii <- ii + 1

            updateEverything <- function(update = auto) {
                ## To easily diable automatic updating of plot, add this argument,
                ## otherwise would have to block/unblock handlers
                if (!update)
                    return()
                
                pch.sel <- ifelse(svalue(fillColor) | svalue(transpSlider) > 0,
                                  19, 1)
                GUI$getActiveDoc()$setSettings(
                    list(col.pt = svalue(symbolColList),
                         bg = svalue(backgroundColList),
                         cex.pt = svalue(cexSlider),
                         pch = pch.sel,
                         alpha = 1 - svalue(transpSlider) / 100
                         ))
                updateSettings()
            }

            ii <- ii + 1
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) updateEverything(TRUE))
            if (auto) {
                bcoltimer <- NULL
                addHandlerChanged(backgroundColList,
                                  handler = function(h, ...) {
                                      if (!is.null(bcoltimer))
                                          bcoltimer$stop_timer()
                                      bcoltimer <- gtimer(500, function(...) {
                                                               if (nchar(svalue(backgroundColList)) >= 3)
                                                                   updateEverything()
                                                           }, one.shot = TRUE)
                                  })

                ## This one needs to be deactivated if user is typing:
                pcoltimer <- NULL
                addHandlerChanged(symbolColList,
                                  handler = function(h, ...) {
                                      if (!is.null(pcoltimer))
                                          pcoltimer$stop_timer()
                                      pcoltimer <- gtimer(500, function(...) {
                                                               if (nchar(svalue(symbolColList)) >= 3)
                                                                   updateEverything()
                                                           }, one.shot = TRUE)
                                  })
 
                addHandlerChanged(fillColor,
                                  handler = function(h, ...) updateEverything())
                
                cextimer <- NULL
                addHandlerChanged(cexSlider,
                                  handler = function(h, ...) {
                                      if (!is.null(cextimer))
                                          cextimer$stop_timer()
                                      cextimer <- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })

                transptimer <- NULL
                addHandlerChanged(transpSlider,
                                  handler = function(h, ...) {
                                      if (!is.null(transptimer))
                                          transptimer$stop_timer()
                                      transptimer <- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })

                autoCheck <- gcheckbox("Update automatically", checked = auto)
                tbl[ii, 1:2, expand = TRUE] <- autoCheck
                ii <- ii + 1

                addHandlerChanged(autoCheck, handler = function(h, ...) {
                                                 enabled(showButton) <- !svalue(autoCheck)
                                                 auto <<- svalue(autoCheck)
                                             })
            }
            
            
            tbl[ii, 1:2, expand = TRUE] <- showButton
            enabled(showButton) <- !auto
            
            add(optGrp, tbl)
        },
        opt8 = function() iNZLocatePoints(),
        opt9 = function() {
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Customize Labels")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            

            curPlSet <- GUI$getActiveDoc()$getSettings()
            oldMain <- curPlSet$main
            oldX <- curPlSet$xlab
            oldY <- curPlSet$ylab
            if (is.null(oldMain)) oldMain <- ''
            if (is.null(oldX)) oldX <- ''
            if (is.null(oldY)) oldY <- ''

            lbl    <- glabel("Main title :")
            labMain <- gedit(oldMain)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labMain
            ii <- ii + 1
            
            lbl    <- glabel("x-axis label :")
            labX    <- gedit(oldX)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labX
            ii <- ii + 1

            lbl    <- glabel("y-axis label :")
            labY    <- gedit(oldY)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labY
            ii <- ii + 1
            

            lbl <- glabel("Enter a single space to print no label\nLeave blank to print default label")
            font(lbl) <- list(family = "normal",
                               size = 8)
            tbl[ii, 2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            
            lbl <- glabel("Press ENTER/RETURN to apply changes")
            font(lbl) <- list(family = "normal", size = 8)
            tbl[ii, 2, anchor = c(-1, -1), expand = TRUE] <- lbl

            
            updateEverything <- function() {
                mlab <- svalue(labMain)
                xlab <- svalue(labX)
                ylab <- svalue(labY)
                GUI$getActiveDoc()$setSettings(
                    list(main = if (mlab != '') mlab else NULL,
                         xlab = if (xlab != '') xlab else NULL,
                         ylab = if (ylab != '') ylab else NULL)
                    )
                updateSettings()
            }
            
            addHandlerChanged(labMain, handler = function(h, ...) updateEverything())
            addHandlerChanged(labX, handler = function(h, ...) updateEverything())
            addHandlerChanged(labY, handler = function(h, ...) updateEverything())
            
            add(optGrp, tbl)
        },
        ## Adjust axis limits
        opt10 = function(){
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Adjust Axis Limits")
            font(lbl) <- list(weight="bold", family = "normal", size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            pl <- GUI$curPlot
            xlim <- if (is.null(curSet$xlim))
                pl$xlim
            else
                curSet$xlim
            ylim <- if (is.null(curSet$ylim))
                pl$ylim
            else
                curSet$ylim

            
            lbl <- glabel("x-axis: ")
            xlower <- gedit(xlim[1])
            xupper <- gedit(xlim[2])
            tbl[ii, 1, expand = TRUE, fill = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- xlower
            tbl[ii, 3, expand = TRUE] <- xupper
            ii <- ii + 1

            lbl <- glabel("y-axis: ")
            ylower <- gedit(ylim[1])
            yupper <- gedit(ylim[2])
            tbl[ii, 1, expand = TRUE, fill = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- ylower
            tbl[ii, 3, expand = TRUE] <- yupper
            ii <- ii + 1

            errlbl <- glabel("Limits must be numbers.")
            tbl[ii, 1:3] <- errlbl
            visible(errlbl) <- FALSE
            ii <- ii + 1

            updateEverything <- function() {
                err <- FALSE
                xl <- suppressWarnings(as.numeric(svalue(xlower)))
                if (is.na(xl)) {
                    xl <- xlim[1]
                    err <- TRUE4
                }
                xu <- suppressWarnings(as.numeric(svalue(xupper)))
                if (is.na(xu)) {
                    xu <- xlim[2]
                    err <- TRUE
                }

                yl <- suppressWarnings(as.numeric(svalue(ylower)))
                if (is.na(yl)) {
                    yl <- ylim[1]
                    err <- TRUE
                }
                yu <- suppressWarnings(as.numeric(svalue(yupper)))
                if (is.na(yu)) {
                    yu <- ylim[2]
                    err <- TRUE
                }

                visible(errlbl) <- err
                    
                ## update plot settings
                GUI$getActiveDoc()$setSettings(
                    list(xlim = c(xl, xu),
                         ylim = c(yl, yu))
                    )
                updateSettings()
            }

            timer <- NULL
            updT <- function(h, ...) {
                if (!is.null(timer))
                    timer$stop_timer()
                timer <- gtimer(800, function(...) updateEverything(), one.shot = TRUE)
            }
            addHandlerKeystroke(xlower, updT)
            addHandlerKeystroke(xupper, updT)
            addHandlerKeystroke(ylower, updT)
            addHandlerKeystroke(yupper, updT)
            
            add(optGrp, tbl)

            resetGrp <- ggroup(cont = optGrp)
            addSpring(resetGrp)
            resetbtn <- gbutton("Reset", cont = resetGrp)
            addHandlerClicked(resetbtn, function(h, ...) {
                GUI$getActiveDoc()$setSettings(
                    list(xlim = NULL, ylim = NULL)
                    )
                updateSettings()

                ## reset the values in the boxes:
                pl <- GUI$curPlot
                xlim <-pl$xlim
                ylim <- pl$ylim

                svalue(xlower) <- xlim[1]
                svalue(xupper) <- xlim[2]
                svalue(ylower) <- ylim[1]
                svalue(yupper) <- ylim[2]
            })
        })
    )


### ---------------------------------------------------------------------------------------------------
### GRID DENSITY PLOT MOD WINDOW
iNZGriddenMod <- setRefClass(
    "iNZGriddenMod",
    contains = "iNZPlotModWin",
    methods = list(
        initialize = function(GUI, which = 1) {
            callSuper(GUI)
            ## need to specify the methods that we want to use in
            ## do.call later on (see changeOpts())
            usingMethods(opt1, opt2, opt3, opt4, opt5)
            opts <- gcombobox(c("Add trend curves",
                                "Add x=y line",
                                "Change plot appearance",
                                "Customize Labels",
                                "Adjust axis limits"),
                              selected = which)
            add(radioGrp, opts, expand = TRUE, fill = TRUE)
            eval(parse(text = paste0("opt", which, "()")))
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
        ## Add trend curves
        opt1 = function() {
            tbl <- glayout()
            ii <- 3

            #### TREND CURVES
            lbl <- glabel("Add trend curves")
            font(lbl) <- list(weight="bold", family = "normal", size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            
            ## Types of curves possible:
            trCrvs <- c("linear", "quadratic", "cubic")
            trCols <- c("red", "black", "blue", "green4",
                        "yellow", "pink", "grey", "orange")

            ## Linear trend
            linChk <- gcheckbox(trCrvs[1],
                                checked = trCrvs[1] %in% curSet$trend)
            linCol <- gcombobox(trCols,
                                selected = which(
                                    curSet$col.trend$linear == trCols
                                    )
                                )
            tbl[ii, 1] <- linChk
            tbl[ii, 2] <- linCol
            ii <- ii + 1
            
            quaChk <- gcheckbox(trCrvs[2],
                                checked = trCrvs[2] %in% curSet$trend)
            quaCol <- gcombobox(trCols,
                                selected = which(
                                    curSet$col.trend$quadratic == trCols
                                    )
                                )
            tbl[ii, 1] <- quaChk
            tbl[ii, 2] <- quaCol
            ii <- ii + 1
            
            cubChk <- gcheckbox(trCrvs[3],
                                checked = trCrvs[3] %in% curSet$trend)
            cubCol <- gcombobox(trCols,
                                selected = which(
                                    curSet$col.trend$cubic == trCols
                                    )
                                )
            tbl[ii, 1] <- cubChk
            tbl[ii, 2] <- cubCol
            ii <- ii + 1


            ii <- ii + 1
            #### SMOOTHERS
            smthCols <- c("red", "black", "blue", "green", "yellow",
                          "magenta", "grey", "orange")

            
            smthChk <- gcheckbox("Draw a smoother",
                                 checked = curSet$smooth!=0 | !is.null(curSet$quant.smooth))
            smthCol <- gcombobox(smthCols,
                                 selected = which(
                                     curSet$col.smooth == smthCols)
                                 )
            tbl[ii, 1] <- smthChk
            tbl[ii, 2] <- smthCol
            ii <- ii + 1
            
            quantSmthChk <- gcheckbox("Use Quantiles",
                                      checked = !is.null(curSet$quant.smooth))
            tbl[ii, 1] <- quantSmthChk
            ii <- ii + 1

            smthSlid <- gslider(from = 0.1, to = 1,
                                by = 0.01,
                                value = ifelse(curSet$smooth==0,
                                    0.7, curSet$smooth))
            tbl[ii, 1:2] <- smthSlid
            ii <- ii + 1

            updateEverything <- function(update = auto) {
                if (!update)
                    return()
                
                ## vector of selected trends
                trSel <- c(svalue(linChk),
                           svalue(quaChk),
                           svalue(cubChk))
                ## vector of colors chosen
                trCol <- c(svalue(linCol),
                           svalue(quaCol),
                           svalue(cubCol))
                ## smoother option
                qsmth <-
                    if (svalue(quantSmthChk))
                        if (svalue(smthChk))"default" else NULL
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
                         col.smooth = svalue(smthCol)
                         )
                    )
                updateSettings()
            }

            ii <- ii + 1            
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) updateEverything(TRUE))
            
            ## only have the smoother slider enabled if the
            ## smoother checkbox is ticked
            if (!svalue(smthChk)) {
                enabled(smthSlid) <- FALSE
                enabled(quantSmthChk) <- FALSE
            } else {
                if (svalue(quantSmthChk))
                    enabled(smthSlid) <- FALSE
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
                updateEverything()
            })
            ## if quantiles are used, disable slider
            addHandlerChanged(quantSmthChk,
                              handler = function(h, ...) {
                                  if (svalue(quantSmthChk)) {
                                      enabled(smthSlid) <- FALSE
                                  }
                                  else {
                                      enabled(smthSlid) <- TRUE
                                  }
                                  updateEverything()
                              })

            addHandlerChanged(linChk, handler = function(h, ...) {
                                          updateEverything()
                                      })
            addHandlerChanged(quaChk, handler = function(h, ...) {
                                          updateEverything()
                                      })
            addHandlerChanged(cubChk, handler = function(h, ...) {
                                          updateEverything()
                                      })
            addHandlerChanged(smthChk, handler = function(h, ...) {
                                           updateEverything()
                                       })
            addHandlerChanged(quantSmthChk, handler = function(h, ...) {
                                                updateEverything()
                                            })

            ## Also update the colour things
            if (auto) {
                addHandlerChanged(linCol, handler = function(h, ...) updateEverything())
                addHandlerChanged(quaCol, handler = function(h, ...) updateEverything())
                addHandlerChanged(cubCol, handler = function(h, ...) updateEverything())
                addHandlerChanged(smthCol, handler = function(h, ...) updateEverything())                

                smthtimer <- NULL
                addHandlerChanged(smthSlid,
                                  handler = function(h, ...) {
                                      if (!is.null(smthtimer))
                                          smthtimer$stop_timer()
                                      smthtimer <- gtimer(800, function(...) updateEverything(), one.shot = TRUE)
                                  })

                autoCheck <- gcheckbox("Update automatically", checked = auto)
                tbl[ii, 1:2, expand = TRUE] <- autoCheck
                ii <- ii + 1

                addHandlerChanged(autoCheck, handler = function(h, ...) {
                                                 enabled(showButton) <- !svalue(autoCheck)
                                                 auto <<- svalue(autoCheck)
                                             })
            }

            tbl[ii, 1:2, expand = TRUE] <- showButton
            enabled(showButton) <- !auto

            add(optGrp, tbl)
        },
        ## Add x=y line
        opt2 = function(){
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Add x=y line")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            
            xyline <- gcheckbox("Plot x=y line",
                                checked = curSet$LOE)
            xyCols <- c("red", "black", "blue", "green4",
                        "yellow", "pink", "grey", "orange")
            xyCol <- gcombobox(xyCols,
                               selected = which(
                                   curSet$col.LOE == xyCols
                                   )
                               )
            tbl[ii, 1, expand = TRUE] <- xyline
            tbl[ii, 2, expand = TRUE] <- xyCol
            ii <- ii + 1

            updateEverything <- function() {
                ## update plot settings
                GUI$getActiveDoc()$setSettings(
                    list(LOE = svalue(xyline),
                         col.LOE = svalue(xyCol))
                    )
                updateSettings()
            }

            addHandlerChanged(xyline, handler = function(h, ...) updateEverything())
            addHandlerChanged(xyCol, handler = function(h, ...) updateEverything())
            
            add(optGrp, tbl)
        },
        ## change plot appearance
        opt3 = function() {
            tbl <- glayout()
            ii <- 3

            ## Default settings
            defts <- iNZightPlots:::inzpar()
            
            ## PLOT APPEARANCE
            lbl <- glabel("Change plot appearance")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii,  1:2, anchor = c(-1,-1), expand = TRUE] <- lbl
            ii <- ii + 1

            
            ## PLOT TYPE
            lbl <- glabel("Plot Type :")
            
            plotTypes <- c("default", "scatter plot", "grid-density plot", "hexbin plot")
            plotTypeValues <- list("default", "scatter", "grid", "hex")
            plotTypeList <- gcombobox(
                plotTypes,
                selected = which(plotTypeValues == curSet$plottype)
                )
            
            addHandlerChanged(plotTypeList, handler = function(h, ...) {
                GUI$getActiveDoc()$setSettings(
                    list(plottype = plotTypeValues[[svalue(plotTypeList, index = TRUE)]])
                    )
                updateSettings()

                plType <- svalue(plotTypeList, index = TRUE)
                if (plType == 2 | plType == 4 | (plType == 1 & GUI$plotType != "grid")) {
                    switch(GUI$plotType,
                           "scatter" = iNZScatterMod$new(GUI, which = 7),
                           "hex" = iNZHexbinMod$new(GUI, which = 3))
                }
            })
            
            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- plotTypeList
            ii <- ii + 1


            ## BACKGROUND COLOUR
            lbl <- glabel("Background colour :")
            
            backgroundCols <- c(defts$bg, "antiquewhite",
                                "azure3", "bisque", "cornsilk", "darkolivegreen2",
                                "darkslategray1", "greenyellow", "lightblue1",
                                "lightpink", "rosybrown1", "slategray1", "thistle1",
                                "wheat1")
            backgroundColList <- gcombobox(
                backgroundCols,
                selected = ifelse(
                    is.na(which(backgroundCols == curSet$bg)[1]),
                    1,
                    which(backgroundCols == curSet$bg)[1]),
                editable = TRUE)
            
            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- backgroundColList
            ii <- ii + 1


            ## GRID SIZE
            lbl <- glabel("Grid size (n x n) :")
            binSlider <- gslider(from = 10, to = 250,
                                 by = 1, value = curSet$scatter.grid.bins)
            tbl[ii, 1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- binSlider
            ii <- ii + 1


            ## Min-count greyness scale
            lbl <- glabel("Min-count colour (% grey) :")
            greySlider <- gslider(from = 0, to = 50,
                                  by = 1, value = round(50 * (curSet$alpha)))
            tbl[ii, 1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- greySlider
            ii <- ii + 1


            updateEverything <- function(update = auto) {
                ## To easily diable automatic updating of plot, add this argument,
                ## otherwise would have to block/unblock handlers
                if (!update)
                    return()
                
                GUI$getActiveDoc()$setSettings(
                    list(bg = svalue(backgroundColList),
                         scatter.grid.bins = svalue(binSlider),
                         alpha = svalue(greySlider) / 50
                         )
                    )
                updateSettings()
            }

            ii <- ii + 1
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) updateEverything(TRUE))
            if (auto) {
                bcoltimer <- NULL
                addHandlerChanged(backgroundColList,
                                  handler = function(h, ...) {
                                      if (!is.null(bcoltimer))
                                          bcoltimer$stop_timer()
                                      bcoltimer <- gtimer(500, function(...) {
                                                               if (nchar(svalue(backgroundColList)) >= 3)
                                                                   updateEverything()
                                                           }, one.shot = TRUE)
                                  })
                
                bintimer <- NULL
                addHandlerChanged(binSlider,
                                  handler = function(h, ...) {
                                      if (!is.null(bintimer))
                                          bintimer$stop_timer()
                                      bintimer <- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })

                greytimer <- NULL
                addHandlerChanged(greySlider,
                                  handler = function(h, ...) {
                                      if (!is.null(greytimer))
                                          greytimer$stop_timer()
                                      greytimer <- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })

                autoCheck <- gcheckbox("Update automatically", checked = auto)
                tbl[ii, 1:2, expand = TRUE] <- autoCheck
                ii <- ii + 1

                addHandlerChanged(autoCheck, handler = function(h, ...) {
                                                 enabled(showButton) <- !svalue(autoCheck)
                                                 auto <<- svalue(autoCheck)
                                             })
            }
            
            
            tbl[ii, 1:2, expand = TRUE] <- showButton
            enabled(showButton) <- !auto
            
            add(optGrp, tbl)
        },
        opt4 = function() {
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Customize Labels")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            

            curPlSet <- GUI$getActiveDoc()$getSettings()
            oldMain <- curPlSet$main
            oldX <- curPlSet$xlab
            oldY <- curPlSet$ylab
            if (is.null(oldMain)) oldMain <- ''
            if (is.null(oldX)) oldX <- ''
            if (is.null(oldY)) oldY <- ''

            lbl    <- glabel("Main title :")
            labMain <- gedit(oldMain)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labMain
            ii <- ii + 1
            
            lbl    <- glabel("x-axis label :")
            labX    <- gedit(oldX)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labX
            ii <- ii + 1

            lbl    <- glabel("y-axis label :")
            labY    <- gedit(oldY)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labY
            ii <- ii + 1
            

            lbl <- glabel("Enter a single space to print no label\nLeave blank to print default label")
            font(lbl) <- list(family = "normal",
                               size = 8)
            tbl[ii, 2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            
            lbl <- glabel("Press ENTER/RETURN to apply changes")
            font(lbl) <- list(family = "normal", size = 8)
            tbl[ii, 2, anchor = c(-1, -1), expand = TRUE] <- lbl

            
            updateEverything <- function() {
                mlab <- svalue(labMain)
                xlab <- svalue(labX)
                ylab <- svalue(labY)
                GUI$getActiveDoc()$setSettings(
                    list(main = if (mlab != '') mlab else NULL,
                         xlab = if (xlab != '') xlab else NULL,
                         ylab = if (ylab != '') ylab else NULL)
                    )
                updateSettings()
            }
            
            addHandlerChanged(labMain, handler = function(h, ...) updateEverything())
            addHandlerChanged(labX, handler = function(h, ...) updateEverything())
            addHandlerChanged(labY, handler = function(h, ...) updateEverything())
            
            add(optGrp, tbl)
        },
        ## Adjust axis limits
        opt5 = function(){
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Adjust Axis Limits")
            font(lbl) <- list(weight="bold", family = "normal", size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            pl <- GUI$curPlot
            xlim <- if (is.null(curSet$xlim))
                pl$xlim
            else
                curSet$xlim
            ylim <- if (is.null(curSet$ylim))
                pl$ylim
            else
                curSet$ylim
           
            lbl <- glabel("x-axis: ")
            xlower <- gedit(xlim[1])
            xupper <- gedit(xlim[2])
            tbl[ii, 1, expand = TRUE, fill = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- xlower
            tbl[ii, 3, expand = TRUE] <- xupper
            ii <- ii + 1

            lbl <- glabel("y-axis: ")
            ylower <- gedit(ylim[1])
            yupper <- gedit(ylim[2])
            tbl[ii, 1, expand = TRUE, fill = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- ylower
            tbl[ii, 3, expand = TRUE] <- yupper
            ii <- ii + 1

            errlbl <- glabel("Limits must be numbers.")
            tbl[ii, 1:3] <- errlbl
            visible(errlbl) <- FALSE

            updateEverything <- function() {
                err <- FALSE
                xl <- suppressWarnings(as.numeric(svalue(xlower)))
                if (is.na(xl)) {
                    xl <- xlim[1]
                    err <- TRUE
                }
                xu <- suppressWarnings(as.numeric(svalue(xupper)))
                if (is.na(xu)) {
                    xu <- xlim[2]
                    err <- TRUE
                }

                yl <- suppressWarnings(as.numeric(svalue(ylower)))
                if (is.na(yl)) {
                    yl <- ylim[1]
                    err <- TRUE
                }
                yu <- suppressWarnings(as.numeric(svalue(yupper)))
                if (is.na(yu)) {
                    yu <- ylim[2]
                    err <- TRUE
                }

                visible(errlbl) <- err
                    
                ## update plot settings
                GUI$getActiveDoc()$setSettings(
                    list(xlim = c(xl, xu),
                         ylim = c(yl, yu))
                    )
                updateSettings()
            }

            timer <- NULL
            updT <- function(h, ...) {
                if (!is.null(timer))
                    timer$stop_timer()
                timer <- gtimer(800, function(...) updateEverything(), one.shot = TRUE)
            }
            addHandlerKeystroke(xlower, updT)
            addHandlerKeystroke(xupper, updT)
            addHandlerKeystroke(ylower, updT)
            addHandlerKeystroke(yupper, updT)
            
            add(optGrp, tbl)

            resetGrp <- ggroup(cont = optGrp)
            addSpring(resetGrp)
            resetbtn <- gbutton("Reset", cont = resetGrp)
            addHandlerClicked(resetbtn, function(h, ...) {
                GUI$getActiveDoc()$setSettings(
                    list(xlim = NULL, ylim = NULL)
                    )
                updateSettings()

                ## reset the values in the boxes:
                pl <- GUI$curPlot
                xlim <-pl$xlim
                ylim <- pl$ylim

                svalue(xlower) <- xlim[1]
                svalue(xupper) <- xlim[2]
                svalue(ylower) <- ylim[1]
                svalue(yupper) <- ylim[2]
            })
        })
    )

### ---------------------------------------------------------------------------------------------------
### HEXBIN PLOT MOD WINDOW
iNZHexbinMod <- setRefClass(
    "iNZHexbinMod",
    contains = "iNZPlotModWin",
    methods = list(
        initialize = function(GUI, which = 1) {
            callSuper(GUI)
            ## need to specify the methods that we want to use in
            ## do.call later on (see changeOpts())
            usingMethods(opt1, opt2, opt3, opt4, opt5)
            opts <- gcombobox(c("Add trend curves",
                                "Add x=y line",
                                "Change plot appearance",
                                "Customize Labels",
                                "Adjust axis limits"),
                              selected = which)
            add(radioGrp, opts, expand = TRUE, fill = TRUE)
            eval(parse(text = paste0("opt", which, "()")))
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
        ## Add trend curves
        opt1 = function() {
            tbl <- glayout()
            ii <- 3

            #### TREND CURVES
            lbl <- glabel("Add trend curves")
            font(lbl) <- list(weight="bold", family = "normal", size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            
            ## Types of curves possible:
            trCrvs <- c("linear", "quadratic", "cubic")
            trCols <- c("red", "black", "blue", "green4",
                        "yellow", "pink", "grey", "orange")

            ## Linear trend
            linChk <- gcheckbox(trCrvs[1],
                                checked = trCrvs[1] %in% curSet$trend)
            linCol <- gcombobox(trCols,
                                selected = which(
                                    curSet$col.trend$linear == trCols
                                    )
                                )
            tbl[ii, 1] <- linChk
            tbl[ii, 2] <- linCol
            ii <- ii + 1
            
            quaChk <- gcheckbox(trCrvs[2],
                                checked = trCrvs[2] %in% curSet$trend)
            quaCol <- gcombobox(trCols,
                                selected = which(
                                    curSet$col.trend$quadratic == trCols
                                    )
                                )
            tbl[ii, 1] <- quaChk
            tbl[ii, 2] <- quaCol
            ii <- ii + 1
            
            cubChk <- gcheckbox(trCrvs[3],
                                checked = trCrvs[3] %in% curSet$trend)
            cubCol <- gcombobox(trCols,
                                selected = which(
                                    curSet$col.trend$cubic == trCols
                                    )
                                )
            tbl[ii, 1] <- cubChk
            tbl[ii, 2] <- cubCol
            ii <- ii + 1


            ii <- ii + 1
            #### SMOOTHERS
            smthCols <- c("red", "black", "blue", "green", "yellow",
                          "magenta", "grey", "orange")

            
            smthChk <- gcheckbox("Draw a smoother",
                                 checked = curSet$smooth!=0 | !is.null(curSet$quant.smooth))
            smthCol <- gcombobox(smthCols,
                                 selected = which(
                                     curSet$col.smooth == smthCols)
                                 )
            tbl[ii, 1] <- smthChk
            tbl[ii, 2] <- smthCol
            ii <- ii + 1
            
            quantSmthChk <- gcheckbox("Use Quantiles",
                                      checked = !is.null(curSet$quant.smooth))
            tbl[ii, 1] <- quantSmthChk
            ii <- ii + 1

            smthSlid <- gslider(from = 0.1, to = 1,
                                by = 0.01,
                                value = ifelse(curSet$smooth==0,
                                    0.7, curSet$smooth))
            tbl[ii, 1:2] <- smthSlid
            ii <- ii + 1

            updateEverything <- function(update = auto) {
                if (!update)
                    return()
                
                ## vector of selected trends
                trSel <- c(svalue(linChk),
                           svalue(quaChk),
                           svalue(cubChk))
                ## vector of colors chosen
                trCol <- c(svalue(linCol),
                           svalue(quaCol),
                           svalue(cubCol))
                ## smoother option
                qsmth <-
                    if (svalue(quantSmthChk))
                        if (svalue(smthChk))"default" else NULL
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
                         col.smooth = svalue(smthCol)
                         )
                    )
                updateSettings()
            }

            ii <- ii + 1            
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) updateEverything(TRUE))
            
            ## only have the smoother slider enabled if the
            ## smoother checkbox is ticked
            if (!svalue(smthChk)) {
                enabled(smthSlid) <- FALSE
                enabled(quantSmthChk) <- FALSE
            } else {
                if (svalue(quantSmthChk))
                    enabled(smthSlid) <- FALSE
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
                updateEverything()
            })
            ## if quantiles are used, disable slider
            addHandlerChanged(quantSmthChk,
                              handler = function(h, ...) {
                                  if (svalue(quantSmthChk)) {
                                      enabled(smthSlid) <- FALSE
                                  }
                                  else {
                                      enabled(smthSlid) <- TRUE
                                  }
                                  updateEverything()
                              })

            addHandlerChanged(linChk, handler = function(h, ...) {
                                          updateEverything()
                                      })
            addHandlerChanged(quaChk, handler = function(h, ...) {
                                          updateEverything()
                                      })
            addHandlerChanged(cubChk, handler = function(h, ...) {
                                          updateEverything()
                                      })
            addHandlerChanged(smthChk, handler = function(h, ...) {
                                           updateEverything()
                                       })
            addHandlerChanged(quantSmthChk, handler = function(h, ...) {
                                                updateEverything()
                                            })

            ## Also update the colour things
            if (auto) {
                addHandlerChanged(linCol, handler = function(h, ...) updateEverything())
                addHandlerChanged(quaCol, handler = function(h, ...) updateEverything())
                addHandlerChanged(cubCol, handler = function(h, ...) updateEverything())
                addHandlerChanged(smthCol, handler = function(h, ...) updateEverything())                

                smthtimer <- NULL
                addHandlerChanged(smthSlid,
                                  handler = function(h, ...) {
                                      if (!is.null(smthtimer))
                                          smthtimer$stop_timer()
                                      smthtimer <- gtimer(800, function(...) updateEverything(), one.shot = TRUE)
                                  })

                autoCheck <- gcheckbox("Update automatically", checked = auto)
                tbl[ii, 1:2, expand = TRUE] <- autoCheck
                ii <- ii + 1

                addHandlerChanged(autoCheck, handler = function(h, ...) {
                                                 enabled(showButton) <- !svalue(autoCheck)
                                                 auto <<- svalue(autoCheck)
                                             })
            }

            tbl[ii, 1:2, expand = TRUE] <- showButton
            enabled(showButton) <- !auto

            add(optGrp, tbl)
        },
        ## Add x=y line
        opt2 = function(){
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Add x=y line")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            
            xyline <- gcheckbox("Plot x=y line",
                                checked = curSet$LOE)
            xyCols <- c("red", "black", "blue", "green4",
                        "yellow", "pink", "grey", "orange")
            xyCol <- gcombobox(xyCols,
                               selected = which(
                                   curSet$col.LOE == xyCols
                                   )
                               )
            tbl[ii, 1, expand = TRUE] <- xyline
            tbl[ii, 2, expand = TRUE] <- xyCol
            ii <- ii + 1

            updateEverything <- function() {
                ## update plot settings
                GUI$getActiveDoc()$setSettings(
                    list(LOE = svalue(xyline),
                         col.LOE = svalue(xyCol))
                    )
                updateSettings()
            }

            addHandlerChanged(xyline, handler = function(h, ...) updateEverything())
            addHandlerChanged(xyCol, handler = function(h, ...) updateEverything())
            
            add(optGrp, tbl)
        },
        ## change plot appearance
        opt3 = function() {
            tbl <- glayout()
            ii <- 3

            ## Default settings
            defts <- iNZightPlots:::inzpar()
            
            ## PLOT APPEARANCE
            lbl <- glabel("Change plot appearance")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii,  1:2, anchor = c(-1,-1), expand = TRUE] <- lbl
            ii <- ii + 1

            
            ## PLOT TYPE
            lbl <- glabel("Plot Type :")
            
            plotTypes <- c("default", "scatter plot", "grid-density plot", "hexbin plot")
            plotTypeValues <- list("default", "scatter", "grid", "hex")
            plotTypeList <- gcombobox(
                plotTypes,
                selected = which(plotTypeValues == curSet$plottype)
                )
            
            addHandlerChanged(plotTypeList, handler = function(h, ...) {
                GUI$getActiveDoc()$setSettings(
                    list(plottype = plotTypeValues[[svalue(plotTypeList, index = TRUE)]])
                    )
                updateSettings()

                plType <- svalue(plotTypeList, index = TRUE)
                if (plType == 2 | plType == 3 | (plType == 1 & GUI$plotType != "hex")) {
                    switch(GUI$plotType,
                           "scatter" = iNZScatterMod$new(GUI, which = 7),
                           "grid" = iNZGriddenMod$new(GUI, which = 3))
                }
            })
            
            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- plotTypeList
            ii <- ii + 1


            ## BACKGROUND COLOUR
            lbl <- glabel("Background colour :")
            
            backgroundCols <- c(defts$bg, "antiquewhite",
                                "azure3", "bisque", "cornsilk", "darkolivegreen2",
                                "darkslategray1", "greenyellow", "lightblue1",
                                "lightpink", "rosybrown1", "slategray1", "thistle1",
                                "wheat1")
            backgroundColList <- gcombobox(
                backgroundCols,
                selected = ifelse(
                    is.na(which(backgroundCols == curSet$bg)[1]),
                    1,
                    which(backgroundCols == curSet$bg)[1]),
                editable = TRUE)
            
            tbl[ii,  1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii,  2, expand = TRUE] <- backgroundColList
            ii <- ii + 1


            ## HEX SIZE
            lbl <- glabel("Hex grid size :")
            hexSlider <- gslider(from = 5, to = 70,
                                 by = 1, value = curSet$hex.bins)
            tbl[ii, 1, anchor = c(-1,-1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- hexSlider
            ii <- ii + 1


            updateEverything <- function(update = auto) {
                ## To easily diable automatic updating of plot, add this argument,
                ## otherwise would have to block/unblock handlers
                if (!update)
                    return()
                
                GUI$getActiveDoc()$setSettings(
                    list(bg = svalue(backgroundColList),
                         hex.bins = svalue(hexSlider)
                         )
                    )
                updateSettings()
            }

            ii <- ii + 1
            showButton <- gbutton("Show Changes",
                                  handler = function(h, ...) updateEverything(TRUE))
            if (auto) {
                bcoltimer <- NULL
                addHandlerChanged(backgroundColList,
                                  handler = function(h, ...) {
                                      if (!is.null(bcoltimer))
                                          bcoltimer$stop_timer()
                                      bcoltimer <- gtimer(500, function(...) {
                                                               if (nchar(svalue(backgroundColList)) >= 3)
                                                                   updateEverything()
                                                           }, one.shot = TRUE)
                                  })
                
                hextimer <- NULL
                addHandlerChanged(hexSlider,
                                  handler = function(h, ...) {
                                      if (!is.null(hextimer))
                                          hextimer$stop_timer()
                                      hextimer <- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })

                autoCheck <- gcheckbox("Update automatically", checked = auto)
                tbl[ii, 1:2, expand = TRUE] <- autoCheck
                ii <- ii + 1

                addHandlerChanged(autoCheck, handler = function(h, ...) {
                                                 enabled(showButton) <- !svalue(autoCheck)
                                                 auto <<- svalue(autoCheck)
                                             })
            }
            
            
            tbl[ii, 1:2, expand = TRUE] <- showButton
            enabled(showButton) <- !auto
            
            add(optGrp, tbl)
        },
        opt4 = function() {
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Customize Labels")
            font(lbl) <- list(weight="bold",
                               family = "normal",
                               size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            

            curPlSet <- GUI$getActiveDoc()$getSettings()
            oldMain <- curPlSet$main
            oldX <- curPlSet$xlab
            oldY <- curPlSet$ylab
            if (is.null(oldMain)) oldMain <- ''
            if (is.null(oldX)) oldX <- ''
            if (is.null(oldY)) oldY <- ''

            lbl    <- glabel("Main title :")
            labMain <- gedit(oldMain)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labMain
            ii <- ii + 1
            
            lbl    <- glabel("x-axis label :")
            labX    <- gedit(oldX)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labX
            ii <- ii + 1

            lbl    <- glabel("y-axis label :")
            labY    <- gedit(oldY)
            tbl[ii, 1, anchor = c(-1, -1), expand = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- labY
            ii <- ii + 1
            

            lbl <- glabel("Enter a single space to print no label\nLeave blank to print default label")
            font(lbl) <- list(family = "normal",
                               size = 8)
            tbl[ii, 2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            
            lbl <- glabel("Press ENTER/RETURN to apply changes")
            font(lbl) <- list(family = "normal", size = 8)
            tbl[ii, 2, anchor = c(-1, -1), expand = TRUE] <- lbl

            
            updateEverything <- function() {
                mlab <- svalue(labMain)
                xlab <- svalue(labX)
                ylab <- svalue(labY)
                GUI$getActiveDoc()$setSettings(
                    list(main = if (mlab != '') mlab else NULL,
                         xlab = if (xlab != '') xlab else NULL,
                         ylab = if (ylab != '') ylab else NULL)
                    )
                updateSettings()
            }
            
            addHandlerChanged(labMain, handler = function(h, ...) updateEverything())
            addHandlerChanged(labX, handler = function(h, ...) updateEverything())
            addHandlerChanged(labY, handler = function(h, ...) updateEverything())
            
            add(optGrp, tbl)
        },
        ## Adjust axis limits
        opt5 = function(){
            tbl <- glayout()
            ii <- 3
            
            lbl <- glabel("Adjust Axis Limits")
            font(lbl) <- list(weight="bold", family = "normal", size = 9)
            tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            pl <- GUI$curPlot
            xlim <- if (is.null(curSet$xlim))
                pl$xlim
            else
                curSet$xlim
            ylim <- if (is.null(curSet$ylim))
                pl$ylim
            else
                curSet$ylim
           
            lbl <- glabel("x-axis: ")
            xlower <- gedit(xlim[1])
            xupper <- gedit(xlim[2])
            tbl[ii, 1, expand = TRUE, fill = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- xlower
            tbl[ii, 3, expand = TRUE] <- xupper
            ii <- ii + 1

            lbl <- glabel("y-axis: ")
            ylower <- gedit(ylim[1])
            yupper <- gedit(ylim[2])
            tbl[ii, 1, expand = TRUE, fill = TRUE] <- lbl
            tbl[ii, 2, expand = TRUE] <- ylower
            tbl[ii, 3, expand = TRUE] <- yupper
            ii <- ii + 1

            errlbl <- glabel("Limits must be numbers.")
            tbl[ii, 1:3] <- errlbl
            visible(errlbl) <- FALSE

            updateEverything <- function() {
                err <- FALSE
                xl <- suppressWarnings(as.numeric(svalue(xlower)))
                if (is.na(xl)) {
                    xl <- xlim[1]
                    err <- TRUE
                }
                xu <- suppressWarnings(as.numeric(svalue(xupper)))
                if (is.na(xu)) {
                    xu <- xlim[2]
                    err <- TRUE
                }

                yl <- suppressWarnings(as.numeric(svalue(ylower)))
                if (is.na(yl)) {
                    yl <- ylim[1]
                    err <- TRUE
                }
                yu <- suppressWarnings(as.numeric(svalue(yupper)))
                if (is.na(yu)) {
                    yu <- ylim[2]
                    err <- TRUE
                }

                visible(errlbl) <- err
                    
                ## update plot settings
                GUI$getActiveDoc()$setSettings(
                    list(xlim = c(xl, xu),
                         ylim = c(yl, yu))
                    )
                updateSettings()
            }

            timer <- NULL
            updT <- function(h, ...) {
                if (!is.null(timer))
                    timer$stop_timer()
                timer <- gtimer(800, function(...) updateEverything(), one.shot = TRUE)
            }
            addHandlerKeystroke(xlower, updT)
            addHandlerKeystroke(xupper, updT)
            addHandlerKeystroke(ylower, updT)
            addHandlerKeystroke(yupper, updT)
            
            add(optGrp, tbl)

            resetGrp <- ggroup(cont = optGrp)
            addSpring(resetGrp)
            resetbtn <- gbutton("Reset", cont = resetGrp)
            addHandlerClicked(resetbtn, function(h, ...) {
                GUI$getActiveDoc()$setSettings(
                    list(xlim = NULL, ylim = NULL)
                    )
                updateSettings()

                ## reset the values in the boxes:
                pl <- GUI$curPlot
                xlim <-pl$xlim
                ylim <- pl$ylim

                svalue(xlower) <- xlim[1]
                svalue(xupper) <- xlim[2]
                svalue(ylower) <- ylim[1]
                svalue(yupper) <- ylim[2]
            })
        })
    )


