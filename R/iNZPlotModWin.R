## --------------------------------------------
## The super class for the plot modification window
## The different windows that are opened through the
## 'Add to Plot' button are subclasses of this superclass
## The window that is opened depends on the variables
## currently selected in the control widget (or in the iNZDocument,
## which is the same since the two are linked together)
## --------------------------------------------

plot_list <- function(plot_type, x, y) {
  if (plot_type %in% c(
    "scatter",
    "hex",
    "grid"
  )) {
    return_list <- list(
      scatter = "scatter",
      hex = "hexagonal binning",
      grid = "grid-density"
    )
  } else if (plot_type %in% c(
    "dot",
    "hist",
    "gg_boxplot",
    "gg_column2",
    "gg_cumcurve",
    "gg_violin",
    "gg_barcode",
    "gg_barcode2",
    "gg_barcode3",
    "gg_dotstrip",
    "gg_lollipop",
    "gg_poppyramid",
    "gg_density",
    "gg_ridgeline",
    "gg_beeswarm",
    "gg_quasirandom"
  )) {
    return_list <- list(
      dot  = "dot plot",
      hist = "histogram",
      gg_dotstrip = "(gg) dot strip",
      gg_barcode3 = "(gg) barcode",
      gg_boxplot = "(gg) boxplot",
      gg_quasirandom = "(gg) beeswarm",
      gg_violin = "(gg) violin",
      gg_density = "(gg) density",
      gg_cumcurve = "(gg) cumulative curve"
    )

    if (is.null(y)) {
      return_list <- append(return_list, list(gg_column2 = "(gg) column/row bar"), length(return_list) - 1)
      return_list <- append(return_list, list(gg_lollipop = "(gg) lollipop"), length(return_list) - 1)
    }

    if (!is.null(y)) {
      return_list <- append(return_list, list(gg_ridgeline = "(gg) density (ridgeline)"), after = length(return_list) - 1)
    }

    if ((!is.numeric(y) && nlevels(y) == 2) || (!is.numeric(x) && nlevels(x) == 2)) {
      return_list <- append(return_list, list(gg_poppyramid = "(gg) pyramid"), after = 2)
    }

    attr(return_list, "cat.levels") <- ifelse(is.numeric(x), nlevels(y), nlevels(x))
  } else if (plot_type %in% c(
    "gg_mosaic",
    "gg_lollipop2",
    "gg_stackedbar",
    "gg_stackedcolumn",
    "gg_column",
    "gg_bar",
    "gg_pie",
    "gg_donut",
    "gg_freqpolygon",
    "gg_heatmap",
    "gg_spine",
    "gg_gridplot",
    "gg_divergingstackedbar",
    "bar"
  )) {
    return_list <- list(
      bar = "barplot",
      gg_column = "(gg) column/row bar",
      gg_stackedcolumn = "(gg) stacked column/row",
      gg_lollipop2 = "(gg) lollipop"
    )

    if (is.null(y)) {
      return_list <- append(return_list, list(gg_gridplot = "(gg) gridplot", gg_pie = "(gg) pie", gg_donut = "(gg) donut"))
    } else {
      return_list <- append(return_list, list(gg_freqpolygon = "(gg) frequency polygons", gg_heatmap = "(gg) heatmap"))
      if (is.factor(y) && nlevels(y) == 2) {
        return_list <- append(return_list, list(gg_spine = "(gg) spine/pyramid"), length(return_list) - 1)
      }

      if (is.factor(x) && nlevels(x) >= 3) {
        return_list <- append(return_list, list(gg_divergingstackedbar = "(gg) diverging stacked bar (likert)"), length(return_list) - 1)
        attr(return_list, "cat.levels") <- nlevels(x)
      }
    }
  }

  attr(return_list, "null.y") <- is.null(y)

  return_list
}

valid_colour <- function(colour) {
  !inherits(try(col2rgb(colour), silent = TRUE), "try-error")
}

iNZPlotModWin <- setRefClass(
    "iNZPlotModWin",
    fields = list(
        GUI = "ANY",
        modWin = "ANY",
        okButton = "ANY",
        ## grp that will hold the multiple choices for plot mods
        radioGrp = "ANY",
        pageMethods = "list",
        ## depending on selection in radioGrp, options for mod
        ## will be displayed here
        optGrp = "ANY",
        curSet = "list", ## the current plot settings
        auto = "logical",   ## if TRUE, then changes occur automatically
        updateEverything = "ANY",
        locSet = "ANY",
        palettes = "list",
        bgColours = "list",
        pointColours = "list",
        barColours = "list",
        colourPalettes = "list",
        EMPH.LEVEL = "numeric",
        timer = "ANY",
        plot_history = "ANY"
        ),
    methods = list(
        initialize = function(gui = NULL, which = 1,
                              .viridis =
                                requireNamespace("viridis", quietly = TRUE),
                              .rcb =
                                requireNamespace("RColorBrewer", quietly = TRUE)) {
            initFields(
                GUI = gui,
                bgColours = list(
                    white = "white",
                    lightgrey = "#eeeeee",
                    mediumgrey = "grey50",
                    darkgrey = "grey20",
                    black = "black",
                    wheat = "wheat",
                    bisque = "bisque",
                    cornsilk = "cornsilk"
                ),
                pointColours = list(
                    grey = "grey50",
                    darkgrey = "grey20",
                    lightgrey = "grey80",
                    blue = "#004b85",
                    red = "red",
                    green = "green4"
                ),
                barColours = list(
                    darkgreen = "darkgreen",
                    lightgreen = "palegreen3",
                    darkblue = "#004b85",
                    lightblue = "steelblue2",
                    red = "darkred",
                    pink = "pink",
                    lightgrey = "grey80",
                    grey = "grey50",
                    darkgrey = "grey20"
                ),
                colourPalettes = list(
                    cat = iNZightPlots::cat_palette_names(),
                    cont = iNZightPlots::const_palette_names(),
                    emphasize = iNZightPlots::emphasize_pal_colour
                ),
                EMPH.LEVEL = 0,
                timer = NULL,
                plot_history = NULL
            ) # end initFields

            if (!is.null(GUI)) {
                updateSettings()

              plot_history <<- GUI$initializePlotHistory()


                modwin <- GUI$initializeModuleWindow(scroll = FALSE)
                mainGrp <- modwin$body

                topGrp <- modwin$header
                lbl <- glabel("Add to Plot :")
                font(lbl) <- list(weight="bold",
                                  family = "sans",
                                  size = 11)
                radioGrp <<- ggroup(horizontal = FALSE,
                                    expand = TRUE)

                optGrp <<- ggroup(
                    horizontal = FALSE,
                    expand = TRUE,
                    use.scrollwindow = "y"
                )
                add(topGrp, lbl)
                add(topGrp, radioGrp, expand = TRUE, fill = TRUE)

                add(mainGrp, optGrp, expand = TRUE)

                ## auto update checkbox

                ## If sample size is too big, use a button instead of
                ## automatically apply changes
                auto <<- nrow(GUI$getActiveData()) < 100000
                autoGrp <- ggroup(horizontal = TRUE, fill = TRUE)
                addSpring(autoGrp)
                autoChk <- gcheckbox("Update automatically",
                    checked = auto, cont = autoGrp)
                updateBtn <- gbutton("Update Plot",
                    fill = TRUE,
                    cont = autoGrp,
                    handler = function(h, ...) updateEverything(TRUE)
                )
                visible(updateBtn) <- !auto
                add(mainGrp, autoGrp, expand = FALSE, anchor = c(0, 1))
                addHandlerChanged(autoChk, handler = function(h, ...) {
                    auto <<- svalue(h$obj)
                    visible(updateBtn) <- !svalue(h$obj)
                })

                btnGrp <- modwin$footer

                helpButton <- gbutton("Help",
                    expand = TRUE,
                    fill = TRUE,
                    cont = btnGrp,
                    handler = function(h, ...) {
                        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/plot_options/?topic=add_to_plot")
                    }
                )

                okButton <<- gbutton("Home",
                    expand = TRUE,
                    fill = TRUE,
                    cont = btnGrp,
                    handler = function(h, ...) {
                        ## delete the module window
                        delete(GUI$leftMain, GUI$leftMain$children[[2]])
                        ## display the default view (data, variable, etc.)
                        visible(GUI$gp1) <<- TRUE
                    }
                )
            }
        },
        ## up the curSet class variable
        updateSettings = function() {
            curSet <<- GUI$getActiveDoc()$getSettings()
        },
        iNZLocatePoints = function(dot = GUI$plotType == "dot") {
            ## Do checking first
            ## If g1 or g2 = _MULTI, then we can't identify points (yet ...)
            cantDo <- function(msg = "using subsetting variables.") {
                gmessage(paste("Cannot identify points when", msg),
                         icon = "error", title = "Unable to identify",
                         parent = modWin)
                return()
            }
            ## remove random ordering of points ...
            GUI$getActiveDoc()$setSettings(
                list(plot.features = list(order.first = -1))
            )
            updateSettings()

            locSet <<- curSet$locate.settings

            updateEverything <<-
                function(locate = GUI$getActiveDoc()$getSettings()$locate,
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
                    levs <-
                        unique(GUI$getActiveData()[highlight, locSet$matchVar])

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

                GUI$getActiveDoc()$setSettings(list(
                    locate = locate,
                    locate.id = unique(id),
                    locate.col = col,
                    locate.extreme = ext,
                    locate.settings = locSet,
                    highlight = highlight,
                    subtitle = subt,
                    plot.features = list(order.first = -1)
                ))
                updateSettings()
            }

            tbl <- glayout()
            ii <- 3

            lbl <- glabel("How do you want to label points?")
            font(lbl) <- list(weight = "bold", family = "sans")
            tbl[ii, 1:2, expand = TRUE, anchor = c(-1, 0)] <- lbl
            ii <- ii + 1

            txtLabs <- gcheckbox("Text Labels", checked = TRUE)
            varmenu <- gcombobox(c("id", names(GUI$getActiveData())),
                selected = 1, expand = TRUE)
            tbl[ii, 1] <- txtLabs
            tbl[ii, 2, expand = TRUE] <- varmenu
            ii <- ii + 1

            if (!is.null(locSet$txtLabs)) svalue(txtLabs) <- locSet$txtLabs
            if (!is.null(locSet$txtVar))
                if (locSet$txtVar %in% c("id", names(GUI$getActiveData())))
                    svalue(varmenu) <- locSet$txtVar

            colLabs <- gcheckbox("Colour Points", checked = FALSE)
            colmenu <- gcombobox(c("red", "blue", "green4"),
                selected = 1, editable = TRUE, expand = TRUE)
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
                locVar <-
                    if (v == "id") 1:nrow(GUI$getActiveData())
                    else GUI$getActiveData()[, v]
                updateEverything(
                    locate = if (svalue(txtLabs)) locVar else NULL
                )
            })
            addHandlerChanged(varmenu, function(h, ...) {
                v <- svalue(varmenu)
                locVar <-
                    if (v == "id") 1:nrow(GUI$getActiveData())
                    else GUI$getActiveData()[, v]
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

                    matchVar <-
                        as.character(GUI$getActiveData()[, svalue(matchVar)])
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
                matchVar <-
                    as.character(GUI$getActiveData()[, svalue(matchVar)])
                matchVar[is.na(matchVar)] <- "missing"

                matchLvls <- unique(matchVar[locSet$ID])
                newIDS <- which(matchVar %in% matchLvls)

                updateEverything(id = newIDS)
            })



            ii <- ii + 1


            lbl <- glabel("How do you want to select points?")
            font(lbl) <- list(weight = "bold", family = "sans")
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

                matchVar <-
                    as.character(GUI$getActiveData()[, svalue(matchVar)])
                matchVar[is.na(matchVar)] <- "missing"

                ## Entire data set - ignore missing values etc etc
                d <- data.frame(
                    x = curSet$x,
                    locate = locVar,
                    id = 1:nrow(GUI$getActiveData()),
                    match = matchVar,
                    stringsAsFactors = TRUE
                )
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

                dp <- grid.get(ifelse(dot,
                    "inz-DOTPOINTS.1.1.1",
                    "inz-SCATTERPOINTS.1.1"
                ))
                d <- d[w & !isNA, ]
                d$x <- as.numeric(dp$x)
                d$y <- as.numeric(dp$y)

                if (dot) {
                    order <- attr(GUI$curPlot[[1]][[1]]$toplot[[1]], "order")
                    d[, !colnames(d) %in% c("x", "y")] <-
                        d[order, !colnames(d) %in% c("x", "y")]
                }

                seekViewport(ifelse(dot,
                    "VP:plotregion",
                    "VP:locate.these.points"
                ))

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

                    newID <-
                        if (svalue(txtLabs) | !match.all)
                            c(curSet$locate.id, pid)
                        else pid
                }

                updateEverything(
                    locate = if (svalue(txtLabs)) locVar else NULL,
                    id = newID,
                    col = if (svalue(colLabs)) svalue(colmenu) else NULL
                )
            }

            if (attr(GUI$curPlot, "nplots") > 1) {
                locateButton <-
                    glabel("Cannot locate using mouse for multiple graphs.",
                        cont = selectGrp)
                svalue(selectMthd, TRUE) <- 2
            } else if (dot & is.factor(curSet$y)) {
                locateButton <-
                    glabel("Cannot locate when Variable 2 is a factor.",
                        cont = selectGrp)
                svalue(selectMthd, TRUE) <- 2
            } else {
                locateButton <- gbutton("Click to Locate ...",
                    cont = selectGrp)
                addHandlerClicked(locateButton, function(h, ...) {
                    locator(h, btn = locateButton, dot = dot)
                })
            }

            selectListGrp <- ggroup(FALSE,
                cont = selectGrp,
                expand = TRUE,
                fill = TRUE
            )

            selectList <- ggroup(TRUE,
                cont = selectListGrp,
                expand = TRUE,
                fill = TRUE
            )
            selectLab <- glabel("Variable: ", cont = selectList)
            selectVar <- gcombobox(colnames(GUI$getActiveData()),
                selected = 0,
                cont = selectList,
                expand = TRUE
            )

            selectSlideGrp <- ggroup(TRUE,
                cont = selectListGrp,
                expand = FALSE,
                fill = TRUE
            )
            selectGo <- gbutton("Select values ...", cont = selectList)

            enabled(selectGo) <- svalue(selectVar, TRUE) > 0
            addHandlerChanged(selectVar, function(h, ...) {
                enabled(selectGo) <- svalue(selectVar, TRUE) > 0

                selVar <- GUI$getActiveData()[, svalue(selectVar)]

                if (length(selectSlideGrp$children) > 0)
                    selectSlideGrp$remove_child(selectSlideGrp$children[[1]])

                if (is.factor(selVar) | (length(unique(selVar)) <= 20)) {
                    nn <-
                        if (is.factor(selVar)) length(levels(selVar))
                        else length(unique(selVar))
                    selectSlide <- gslider(
                        if (is.factor(selVar)) levels(selVar)
                        else unique(selVar),
                        cont = selectSlideGrp,
                        expand = TRUE,
                        fill = TRUE
                    )

                    addHandlerChanged(selectSlide, function(h, ...) {
                        ids <-
                            which(GUI$getActiveData()[, svalue(selectVar)] ==
                                svalue(selectSlide))
                        locSet$ID <<- ids

                        if (svalue(matchChk)) {
                            levs <-
                                unique(as.character(
                                    GUI$getActiveData()[ids, svalue(matchVar)]
                                ))
                            ids <- which(
                                GUI$getActiveData()[, svalue(matchVar)] %in%
                                    levs
                            )
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


            extremeGrp <- ggroup(FALSE,
                cont = selectGrp, expand = TRUE, fill = TRUE)
            if (dot) {
                extremePts <- ggroup(FALSE,
                    cont = extremeGrp, expand = TRUE, fill = TRUE)

                lowerG <- ggroup(
                    cont = extremePts, expand = TRUE, fill = TRUE)
                lowerLab <- glabel("N Lower: ", cont = lowerG)
                nlowerSld <- gslider(0, 20,
                    expand = TRUE, fill = TRUE, cont = lowerG)

                upperG <- ggroup(
                    cont = extremePts, expand = TRUE, fill = TRUE)
                upperLab <- glabel("N Upper: ", cont = upperG)
                nupperSld <- gslider(0, 20,
                    expand = TRUE, fill = TRUE, cont = upperG)

                updateMe <- function(h, ...) {
                    v <- svalue(varmenu)
                    locVar <-
                        if (v == "id") 1:nrow(GUI$getActiveData())
                        else GUI$getActiveData()[, v]
                    updateEverything(
                        locate = if (svalue(txtLabs)) locVar else NULL,
                        id = NULL,
                        col = if (svalue(colLabs)) svalue(colmenu) else NULL,
                        ext = c(svalue(nlowerSld), svalue(nupperSld))
                    )

                    enabled(addPts) <-
                        svalue(nlowerSld) > 0 | svalue(nupperSld) > 0
                }
                addHandlerChanged(nlowerSld, updateMe)
                addHandlerChanged(nupperSld, updateMe)
            } else {
                extremePts <- ggroup(
                    cont = extremeGrp, expand = TRUE, fill = TRUE)
                extLab <- glabel("Number of points: ", cont = extremePts)
                extN <- gslider(0, 20, cont = extremePts, expand = TRUE)
                if (!is.null(curSet$locate.extreme)) svalue(extN) <-
                    curSet$locate.extreme
                addHandlerChanged(extN, handler = function(h, ...) {
                    v <- svalue(varmenu)
                    locVar <-
                        if (v == "id") 1:nrow(GUI$getActiveData())
                        else GUI$getActiveData()[, v]

                    updateEverything(
                        locate = if (svalue(txtLabs)) locVar else NULL,
                        id = NULL,
                        col = if (svalue(colLabs)) svalue(colmenu) else NULL,
                        ext = if (svalue(extN) > 0) svalue(extN) else NULL
                    )
                    enabled(addPts) <- svalue(extN) > 0
                })
            }
            addPts <- gbutton("Save these points ...",
                cont = extremeGrp, expand = FALSE, anchor = c(0, 1))
            enabled(addPts) <-
                if (dot) svalue(nlowerSld) > 0 | svalue(nupperSld) > 0
                else svalue(extN) > 0

            extLabel <- glabel(
                paste(sep = "\n",
                    "NOTE: related points wont be located until",
                    "you click the above button."
                )
            )
            font(extLabel) <- list(family = "sans", size = 7)
            add(extremeGrp, extLabel, anchor = c(-1, -1))

            addHandlerClicked(addPts, function(h, ...) {
                cp <- GUI$curPlot
                ## drop the last 3 pieces (gen, xlim, ylim)
                cp <- cp[1:(length(cp) - 3)]
                if (dot) {
                    ids <- sapply(cp, function(p)
                        sapply(p, function(q) sapply(q$toplot, function(r)
                            r$extreme.ids)))
                } else {
                    ids <- sapply(cp, function(p) sapply(p,
                        function(q) q$extreme.ids))
                }
                ids <- sapply(ids[!sapply(ids, is.null)], function(x) x)

                locSet$ID <<- ids
                v <- svalue(varmenu)
                locVar <-
                    if (v == "id") 1:nrow(GUI$getActiveData())
                    else GUI$getActiveData()[, v]

                if (svalue(matchChk)) {
                    mVar <-
                        as.character(GUI$getActiveData()[, svalue(matchVar)])
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
                ww <- gwindow("Select levels to label ...",
                    visible = FALSE, width = 200, height = 400,
                    parent = GUI$win)
                wg <- ggroup(FALSE, cont = ww)
                wlbl <- glabel("Select levels to label\n(ctrl for multiple)",
                    cont = wg)

                selectLevels <- gtable(
                    levels(as.factor(GUI$getActiveData()[, svalue(selectVar)])),
                    multiple = TRUE, cont = wg, expand = TRUE)

                wb <- gbutton("Done", cont = wg)
                addHandlerClicked(wb, function(h, ...) {
                    ids <-  which(GUI$getActiveData()[, svalue(selectVar)] %in%
                        svalue(selectLevels))
                    locSet$ID <<- ids

                    if (svalue(matchChk)) {
                        levs <- unique(as.character(
                            GUI$getActiveData()[ids, svalue(matchVar)]
                        ))
                        ids <- which(
                            GUI$getActiveData()[, svalue(matchVar)] %in% levs
                        )
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
            if (is_num(var)) {
                return(NULL)
            } else {
                lvls <- levels(var)
                colWin <- gwindow("Select Colours",
                    visible = FALSE, parent = GUI$win)
                cgrp <- gvbox(spacing = 5, container = colWin)
                cgrp$set_borderwidth(5)
                tbl <- glayout()
                jj <- 1

                lbl <- glabel("Select colours")
                font(lbl) <- list(weight = "bold", family = "sans", size = 9)
                tbl[jj, 1:2, anchor = c(-1, -1), expand = TRUE] <- lbl
                jj <- jj + 1

                ## this really needs changing!!
                default.cols <- c("darkblue", "darkgreen",
                                  "darkmagenta", "darkslateblue", "hotpink4",
                                  "lightsalmon2", "palegreen3", "steelblue3")
                current.cols <- GUI$curPlot$gen$col.args$f.cols

                for (k in 1:length(lvls)) {
                    tbl[jj, 1, expand = TRUE, anchor = c(1, 0)] <-
                        glabel(lvls[k])
                    tbl[jj, 2] <- gcombobox(
                        items = c(current.cols[k], default.cols),
                        editable = TRUE
                    )
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
        },
        sectionTitle = function(title, size = 10) {
            lbl <- glabel(title)
            font(lbl) <- list(weight = "bold", family = "sans", size = size)
            lbl
        }
    )
)


iNZPlotMod <- setRefClass(
    "iNZPlotMod",
    contains = "iNZPlotModWin",
    methods = list(
        initialize = function(GUI, which = 1) {
            callSuper(GUI)
            ## need to specify the methods that we want to use in
            ## do.call later on (see changeOpts())

            ## do.call(usingMethods, pageMethods)
            if (GUI$plotType %in% c("scatter", "hex", "grid")) {
              pageMethods <<- list("Customise Plot Appearance" = appearance,
                                   "Trend Lines and Curves" = features,
                                   "Axes and Labels" = axes,
                                   "Identify Points" = identify,
                                   iNZLocatePoints)
              usingMethods(appearance, features, identify, axes, iNZLocatePoints)
              opts <- gcombobox(names(pageMethods[names(pageMethods) != ""]),
                                selected = which)
            } else if (GUI$plotType %in% c("dot", "hist")) {
              pageMethods <<- list("Customise Plot Appearance" = appearance,
                                   "Axes and Labels" = axes,
                                   "Identify Points" = identify,
                                   iNZLocatePoints)
              usingMethods(appearance, identify, axes, iNZLocatePoints)
              opts <- gcombobox(names(pageMethods[names(pageMethods) != ""]),
                                selected = which)
            } else {
              pageMethods <<- list("Customise Plot Appearance" = appearance,
                                   "Axes and Labels" = axes)
              usingMethods(appearance, axes)
              opts <- gcombobox(names(pageMethods[names(pageMethods) != ""]),
                                selected = which)
            }

            add(radioGrp, opts, expand = TRUE, fill = TRUE)
            pageMethods[[which]]()
            addHandlerChanged(opts,
                              handler = function(h, ...) {
                                  changeOpts(svalue(h$obj,
                                                    index = TRUE))
                              })
        },
        changeOpts = function(index) {
            ## delete current displayed options
            invisible(sapply(optGrp$children, function(x) delete(optGrp, x)))
            pageMethods[[index]]()
        },
        ## Following are the different views for the Add to Plot window:
        appearance = function() {
            tbl <- glayout(spacing = 3)
            ii <- 3

            ## Default settings
            defts <- iNZightPlots:::inzpar()
            TYPE <- PLOTTYPE <- GUI$plotType

            ## ----- GENERAL APPEARANCE ----------------------------------------------------------
            ##
            ##        Plot type : [default, scatter, hex, grid-density]
            ## Background color : [default->"#cccccc", white, darkgrey->"grey25", ...]
            ##  Overal size cex :  0-----------------|-1
            ##
            ## -----------------------------------------------------------------------------------
            tbl[ii,  1:6, anchor = c(-1,-1), expand = TRUE] <- sectionTitle("General Appearance")
            ii <- ii + 1

            ## PLOT TYPE
            lbl <- glabel("Plot type :")

            varnames <- unlist(attr(GUI$curPlot, "varnames"))
            PLOTTYPES <- plot_list(TYPE, GUI$getActiveData()[[varnames["x"]]], GUI$getActiveData()[[varnames["y"]]])

            # if (PLOTTYPE != "bar") {
              plotTypes <- do.call(c, PLOTTYPES)
              plotTypeValues <- names(PLOTTYPES)
              plotTypeList <- gcombobox(
                  plotTypes,
                  selected = which(plotTypeValues == TYPE)
                  )

              addHandlerChanged(plotTypeList, handler = function(h, ...) {
                  newSet <- list(plottype = plotTypeValues[[svalue(plotTypeList, index = TRUE)]])
                  if (!is.null(curSet$colby)) {
                      ## when switching scatter <-> hex, need to swtich numeric colby <-> factor colby
                      newSet$colby <-
                          if (newSet$plottype == "hex")
                              iNZightPlots::convert.to.factor(curSet$colby)
                          else
                              GUI$getActiveData()[[curSet$varnames$colby]]
                  }
                  if (newSet$plottype == "gg_gridplot") {
                    n_fun <- function(n) {
                      if (n > 1000) {
                        if (n > 5 * 10^ceiling(log10(n) - 1) && n > 5 * 10^ceiling(log10(n + 1) - 1)) {
                          10^(floor(log10(n)) - 1)
                        } else {
                          10^(floor(log10(n)) - 2)
                        }
                      } else {
                        1
                      }
                    }

                    newSet$gg_perN <- n_fun(nrow(GUI$getActiveData()))
                  }

                  GUI$getActiveDoc()$setSettings(newSet)
                  updateSettings()

                  plType <- svalue(plotTypeList, index = TRUE)
                  if (curSet$plottype != TYPE) {
                      iNZPlotMod$new(GUI, which = 1)
                  }
              })

              tbl[ii,  1:2, anchor = c(1, 0), expand = TRUE] <- lbl
              tbl[ii,  3:6, expand = TRUE] <- plotTypeList
              ii <- ii + 1
            # }

            ## BACKGROUND COLOUR
            lbl <- "Background colour :"
            bgCols <- do.call(c, bgColours)
            if (!curSet$bg %in% bgCols) {
                bgColours <<- c(bgColours, list(custom = curSet$bg))
                bgCols <- c(bgCols, bgColours$custom)
            }
            bgCol <- gcombobox(names(bgColours), selected = which(bgCols == curSet$bg),
                               editable = TRUE)
            tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 3:6, expand = TRUE] <- bgCol
            ii <- ii + 1


            ## OVERALL CEX
            lbl <- "Overall size scale :"
            cexMain <- gslider(0.5, 2, by = 0.05, value = curSet$cex)
            tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 3:6, expand = TRUE] <- cexMain

            ii <- ii + 1
            ## ----- POINT SIZE ------------------------------------------------------------------
            ##
            ##          Size by : [ { select variable (numerical) } ]                       [s]
            ##    Sizing method : [proportional, emphasize]                                 [s]
            ##                    (info text)
            ##     Overall size : 0------|-------------3                                    [s,h,g]
            ##            Style : [size, alpha]                                             [h]
            ##
            ## -----------------------------------------------------------------------------------
            if (PLOTTYPE %in% c("scatter", "dot")) {
                tbl[ii,  1:6, anchor = c(-1, 0), expand = TRUE] <- sectionTitle("Point Size")
                ii <- ii + 1

                ## OVERALL SIZE
                lbl <- glabel("Overall :")
                cexPt <- gslider(from = 0.05, to = 3.5,
                                 by = 0.05,
                                 value = if (PLOTTYPE == "scatter") curSet$cex.pt else curSet$cex.dotpt
                                 )
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:6, expand = TRUE] <- cexPt
                ii <- ii + 1

                if (PLOTTYPE == "scatter") {
                    ## SIZE BY
                    lbl <- glabel("Resize points by :")
                    sizeVarNames <- names(GUI$getActiveData())[sapply(GUI$getActiveData(), is_num)]
                    sizeVar <-
                        gcombobox(c("", sizeVarNames),
                                  selected = ifelse(
                                      is.null(curSet$sizeby),
                                      1, which(sizeVarNames == curSet$varnames$sizeby)[1] + 1
                                      ))
                    tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                    tbl[ii, 3:6, expand = TRUE] <- sizeVar
                    ii <- ii + 1

                    ## RESIZE METHOD
                    resizeLbl <- glabel("Resize method :")
                    sizeMethods <- c("proportional", "emphasize")
                    sizeMethod <- gcombobox(sizeMethods,
                                            selected = which(sizeMethods == curSet$resize.method))
                    tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- resizeLbl
                    tbl[ii, 3:6, expand = TRUE] <- sizeMethod
                    visible(resizeLbl) <- visible(sizeMethod) <- svalue(sizeVar, index = TRUE) > 1
                    ii <- ii + 1

                    sizeDescs <- list(method1 =
                                          c("Points area proportional to value of variable."),
                                      method2 =
                                          c("Point area linearly sized from 0.25 to 4.",
                                            "Good for exaggerating trends."))
                    sizeDesc <- glabel(paste(sizeDescs[[svalue(sizeMethod, index = TRUE)]]))
                    tbl[ii, 1:6, anchor = c(1, 0), expand = TRUE] <- sizeDesc
                    visible(sizeDesc) <- visible(resizeLbl)
                    ii <- ii + 1
                }

            } else if (PLOTTYPE %in% c("hex", "grid", "hist")) {
                tbl[ii,  1:6, anchor = c(-1, 0), expand = TRUE] <- sectionTitle("Size")
                ii <- ii + 1

                lbl <- glabel(switch(PLOTTYPE,
                                     "hex"  = "Hexagon size :",
                                     "grid" = "Bin size :",
                                     "hist" = "Histogram bin width :"))
                cexPt <- switch(PLOTTYPE,
                                "hex" = {
                                    gslider(from = 0.5, to = 4, by = 0.1,
                                            value = curSet$hex.bins / iNZightPlots::inzpar()$hex.bins)
                                },
                                "grid" = {
                                    gslider(from = 0.2, to = 5, by = 0.1,
                                            value = curSet$scatter.grid.bins / iNZightPlots::inzpar()$scatter.grid.bins)
                                },
                                "hist" = {
                                    gslider(from = 0.05, to = 3.5,
                                            by = 0.05, value = curSet$cex.dotpt)
                                    #gslider(from = 10, to = 100, by = 1,
                                    #        value = length(GUI$curPlot[[1]][[1]]$toplot[[1]][[1]]))
                                })
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:6, expand = TRUE] <- cexPt
                ii <- ii + 1
            }

            if (PLOTTYPE == "hex") {
                lbl <- glabel("Style :")
                hexStyles <- c("size", "alpha")
                hexStyle <- gcombobox(hexStyles, selected = which(hexStyles == curSet$hex.style))
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:6, expand = TRUE] <- hexStyle
                ii <- ii + 1
            }

            ii <- ii + 1
            ## ----- POINT COLOUR ----------------------------------------------------------------
            ##
            ##        Colour by : [ { select variable } ]                                  [s,h]
            ##           Colour : [default->"grey50", black, white, ...]                   [s,h]
            ##         OR
            ##   Colour palette : [default->{num->divergent_hcl, cat->rainbow_hcl}, ...]   [s,h{cat_only}]
            ##         Advanced : [ { Manual colour button for cat }, { Adjust palette }]  [s]
            ##     Transparency : 0|-------------------1                                   [s,h,g]
            ##  [o] Fill symbol interior                                                   [s]
            ##
            ## -----------------------------------------------------------------------------------
            if (PLOTTYPE %in% c("scatter", "hex", "dot", "hist", "bar")) {
                bars <- PLOTTYPE == "bar"
                hist <- PLOTTYPE == "hist"

                tbl[ii,  1:6, anchor = c(-1, 0), expand = TRUE] <-
                    sectionTitle(switch(PLOTTYPE, "dot" = , "scatter" = "Point Colour", "hex" = "Colour",
                                        "hist" = , "bar" = "Bar Colour"))
                ii <- ii + 1

                if (bars | hist) {
                  barCols <- do.call(c, barColours)
                  barCol <- gcombobox(names(barColours), selected = 1, editable = TRUE)
                  if (curSet$bar.fill %in% barCols)
                    svalue(barCol) <- names(barColours)[which(barCols %in% curSet$bar.fill)[1]]
                } else {
                  ptCols <- do.call(c, pointColours)
                  ptCol <- gcombobox(names(pointColours), selected = 1, editable = TRUE)
                  if (curSet$col.pt %in% ptCols)
                      svalue(ptCol) <- names(pointColours)[which(ptCols %in% curSet$col.pt)[1]]
                }

                colLabel <- glabel(switch(PLOTTYPE,
                                          "dot" = ,
                                          "scatter" = "Point colour :",
                                          "hex"     = "Hexagon colour :",
                                          "hist"    = "Bar colour",
                                          "bar"     = ifelse(is.null(curSet$y), "Bar colour :", "Colour palette : ")))
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE, fill = TRUE] <- colLabel
                tbl[ii, 3:6, expand = TRUE] <- if (bars | hist) barCol else ptCol
                ptColROW <- ii  ## save for switching later
                ii <- ii + 1

                if (!hist & (!bars | is.null(curSet$y))) {
                    ## Colour by
                    lbl <- glabel("Colour by :")
                    if (bars)
                        colVarNames <- names(GUI$getActiveData()[sapply(GUI$getActiveData(), is_cat)])
                    else
                      colVarNames <- names(GUI$getActiveData())
                    colVar <-
                        gcombobox(c("", colVarNames),
                                  selected = ifelse(
                                      is.null(curSet$colby),
                                      1, which(colVarNames == curSet$varnames$colby)[1] + 1
                                      ))
                    tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                    tbl[ii, 3:6, expand = TRUE] <- colVar
                    ii <- ii + 1

                    ## rank instead of linear scale
                    useRank <- gcheckbox("Use Percentiles", checked = curSet$col.method == "rank")
                    tbl[ii, 5:6, anchor = c(-1, 0)] <- useRank
                    ii <- ii + 1
                }
                if (!hist) {
                    ## reverse palette direction
                    revPal <- gcheckbox("Reverse palette", checked = curSet$reverse.palette)
                    tbl[ii - exists("useRank"), 3:4, anchor = c(-1, 0)] <- revPal
                }

                ## dropdown for colour palette
                palCont <- gcombobox(as.character(colourPalettes$cont))
                palCat <- gcombobox(as.character(colourPalettes$cat))
                palAdvanced <- gimagebutton(filename = system.file("images/gear.png",
                                                                   package = "iNZight"),
                                            size = "button",
                                            handler = function(h, ...) {
                                                gmessage("Advanced colour palette options ...")
                                            })
                tbl[ptColROW, 3:5, expand = TRUE] <- palCont
                tbl[ptColROW, 3:5, expand = TRUE] <- palCat
                ## tbl[ptColROW, 6, anchor = c(0, 0)] <- palAdvanced

                if (!is.null(curSet$colby) & (!bars | is.null(curSet$y)) & !hist) {
                    ## already set - need to match
                    cval <- curSet$varnames$colby
                    svalue(colVar) <- cval
                    if (bars) visible(barCol) <- FALSE
                    else visible(ptCol) <- FALSE

                    if (is_num(GUI$getActiveData()[[cval]]) & PLOTTYPE != "hex" & !bars) {
                        visible(palCat) <- FALSE
                    } else {
                        visible(useRank) <- visible(palCont) <- FALSE
                    }
                } else if (bars & !is.null(curSet$y) & !hist) {
                    visible(barCol) <- visible(palAdvanced) <- visible(palCont) <- FALSE
                    visible(palCat) <- TRUE
                } else {
                    visible(palAdvanced) <- visible(palCont) <- visible(palCat) <- FALSE
                    if (!hist) {
                        visible(useRank) <- FALSE
                    }
                }
                if (!hist) {
                    visible(revPal) <- visible(palCont) || visible(palCat)
                }

                if (!bars) {
                    ## Cycle through levels:
                    cycleLbl <- glabel("Cycle levels :")
                    cyclePanel <- ggroup()
                    addSpace(cyclePanel, 10)
                    cyclePrev <- gimagebutton(stock.id = "1leftarrow", container = cyclePanel,
                                              handler = function(h, ...) {
                                                  nl <-
                                                      if (is_cat(curSet$colby)) length(levels(curSet$colby))
                                                      else svalue(cycleN)
                                                  EMPH.LEVEL <<- ifelse(EMPH.LEVEL == 0, nl, EMPH.LEVEL - 1)
                                                  updateEverything()
                                              })
                    cycleNext <- gimagebutton(stock.id = "1rightarrow", container = cyclePanel,
                                              handler = function(h, ...) {
                                                  nl <-
                                                      if (is_cat(curSet$colby)) length(levels(curSet$colby))
                                                      else svalue(cycleN)
                                                  EMPH.LEVEL <<- ifelse(EMPH.LEVEL == nl, 0, EMPH.LEVEL + 1)
                                                  updateEverything()
                                              })
                    addSpace(cyclePanel, 20)
                    cycleStop <- gimagebutton(filename = system.file("images/icon-undo.png", package = "iNZight"),
                                              container = cyclePanel,
                                              handler = function(h, ...) {
                                                  EMPH.LEVEL <<- 0
                                                  updateEverything()
                                              })
                    addSpace(cyclePanel, 20)
                    cycleNlab <- glabel("# quantiles :", container = cyclePanel)
                    font(cycleNlab) <- list(size = 9)
                    cycleN <- gspinbutton(4, 10, by = 1, container = cyclePanel)

                    visible(cycleLbl) <- visible(cyclePanel) <- !is.null(curSet$colby)
                    if (is_num(curSet$colby)) {
                        svalue(cycleLbl) <- "Cycle quantiles :"
                    } else {
                        visible(cycleNlab) <- visible(cycleN) <- FALSE
                    }

                    tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- cycleLbl
                    tbl[ii, 3:6, expand = TRUE] <- cyclePanel
                    ii <- ii + 1
                }
            }

            if (
              grepl("^gg_", PLOTTYPE) &&
              (PLOTTYPE %in% c("gg_pie", "gg_donut", "gg_column", "gg_heatmap", "gg_stackedcolumn", "gg_poppyramid", "gg_spine", "gg_mosaic", "gg_divergingstackedbar", "gg_gridplot")) ||
              (!attr(PLOTTYPES, "null.y") && PLOTTYPE %in% c("gg_violin", "gg_barcode", "gg_boxplot", "gg_cumcurve", "gg_freqpolygon", "gg_dotstrip", "gg_density", "gg_quasirandom", "gg_lollipop2", "gg_ridgeline", "gg_barcode3"))
            ) {
              lbl <- glabel("Colour palette :")
              palette_options <- c("default", "greyscale", "viridis", "magma", "plasma", "inferno", "BrBG", "PiYG", "PRGn",
                                   "Accent", "Dark2", "Paired", "Pastel1", "Set1",
                                   "Blues", "BuGn", "BuPu", "GnBu")
              paletteCombobox <- gcombobox(palette_options,
                                           selected = ifelse(!is.null(curSet$palette), which(palette_options == curSet$palette), 1))

              addHandlerChanged(paletteCombobox, function(h, ...) updateEverything())

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
              tbl[ii, 3:6, expand = TRUE] <- paletteCombobox

              ii <- ii + 1
            }

            if (
              PLOTTYPE %in% c("gg_violin", "gg_column2", "gg_lollipop", "gg_boxplot", "gg_density", "gg_cumcurve", "gg_quasirandom", "gg_lollipop2", "gg_barcode3", "gg_barcode", "gg_dotstrip") && attr(PLOTTYPES, "null.y")
            ) {
              if (!(PLOTTYPE %in% c("gg_cumcurve"))) {
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Fill colour:")
              } else {
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Line colour:")
              }

              if (isTRUE(!is.null(curSet$fill_colour))) {
                fill_colour <- curSet$fill_colour
              } else {
                fill_colour <- ""
              }

              # colourCombobox <- gedit(fill_colour, handler = function(h, ...) updateEverything())
              fill_colours <- c("", names(barColours))
              colourCombobox <- gcombobox(
                fill_colours,
                match(fill_colour, fill_colours, nomatch = 0)[1],
                editable = TRUE
              )

              if (fill_colour != "" && svalue(colourCombobox, index = TRUE) < 2) {
                svalue(colourCombobox) <- fill_colour
              }

              addHandlerChanged(colourCombobox, function(h, ...) updateEverything())
              tbl[ii, 3:6, expand = TRUE] <- colourCombobox

              ii <- ii + 1
            }

            if (PLOTTYPE %in% c("scatter", "dot")) {
                lbl <- glabel("Transparency :")
                transpSlider <- gslider(from = 0, to = 100,
                                        by = 1, value = 100 * (1 - curSet$alpha))
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:6, expand = TRUE] <- transpSlider
                ii <- ii + 1

                transpWarning <- glabel(paste("Warning: transparency may freeze iNZight.  ",
                                              "Use circle symbols to avoid this.",
                                              sep = "\n"))
                font(transpWarning) <- list(size = 9)
                tbl[ii, 1:6, anchor = c(1, 0), expand = TRUE] <- transpWarning
                visible(transpWarning) <- FALSE
                ii <- ii + 1
            }

            ii <- ii + 1
            ## ----- POINT SYMBOL ----------------------------------------------------------------
            ##
            ##  [ ] Match symbol to colour-by variable (if categorical, otherwise disabled)  [s]
            ##  {if unchecked:}
            ##          Code by : [ { select variable (categorical) } ]
            ##                    [ { Specify Symbols} ]
            ##
            ## -----------------------------------------------------------------------------------
            if (PLOTTYPE %in% c("scatter", "dot")) {
                tbl[ii,  1:6, anchor = c(-1, 0), expand = TRUE] <- sectionTitle("Point Symbol")
                ii <- ii + 1

                ## MATCH SYMBOL and COLOUR BY
                pchMatch <- gcheckbox("Match with colour variable",
                                      selected = FALSE)#curSet$match.pch)
                tbl[ii, 1:6, anchor = c(-1, 0)] <- pchMatch
                ii <- ii + 1

                pchMsg <- glabel("(requires categorical variable with 5 or fewer levels)")
                font(pchMsg) <- list(size = 8)
                tbl[ii, 1:6, anchor = c(-1, 0), expand = TRUE] <- pchMsg
                ii <- ii + 1

                symbolMatch <- function() {
                    visible(pchMsg) <- visible(pchMatch) <- svalue(colVar, TRUE) > 1
                    if (visible(pchMatch)) {
                        enabled(pchMatch) <- length(levels(GUI$getActiveData()[[svalue(colVar)]])) %in% 1:5
                        visible(pchMsg) <- !enabled(pchMatch)
                    }
                }
                symbolMatch()


                lbl <- glabel("Symbol :")
                symbolList <- list(circle = 21,
                                   square = 22,
                                   diamond = 23,
                                   triangle = 24,
                                   'inverted triangle' = 25)
                symVals <- do.call(c, symbolList)
                symPch <- gcombobox(names(symbolList), selected = 1)
                if (curSet$pch %in% symVals) svalue(symPch, TRUE) <- which(symVals == curSet$pch)
                visible(transpWarning) <- svalue(symPch, index = TRUE) %in% c(3:5)
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:6] <- symPch
                ii <- ii + 1

                symVars <- colnames(GUI$getActiveData())[sapply(GUI$getActiveData(), function(x) length(levels(x)) %in% 1:5)]
                lbl <- glabel("Symbol by :")
                symVar <- gcombobox(c("", symVars), selected = 1)
                if (length(symVars) >= 1) {
                    tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
                    tbl[ii, 2:6] <- symVar
                    ii <- ii + 1
                }

                enabled(symVar) <- enabled(symPch) <- !svalue(pchMatch)

                ## Fill Symbols + line width
                lbl <- glabel("Symbol line width :")
                symLwd <- gspinbutton(1, 4, by = 1, value = curSet$lwd.pt)
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:4] <- symLwd

                fillSym <- gcheckbox("Fill symbols", checked = curSet$fill.pt == "fill")
                enabled(fillSym) <- svalue(transpSlider) == 0
                tbl[ii, 5:6, anchor = c(-1, 0)] <- fillSym
                ii <- ii + 1
            }

            ii <- ii + 1

            if (PLOTTYPE %in% c("dot", "hist")) {
              tbl[ii,  1:6, anchor = c(-1, 0), expand = TRUE] <- sectionTitle("Summaries")
              ii <- ii + 1

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Show :")

              showBoxplot <- gcheckbox("Boxplot", checked = curSet$boxplot, handler = function(h, ...) updateEverything())
              showMean <- gcheckbox("Mean indicator", checked = curSet$mean_indicator, handler = function(h, ...) updateEverything())

              tbl[ii, 3:6, anchor = c(1, 0), expand = TRUE] <- showBoxplot
              ii <- ii + 1

              tbl[ii, 3:6, anchor = c(1, 0), expand = TRUE] <- showMean
              ii <- ii + 1
            }

            ## FT PLOT OPTIONS

            if (grepl("^gg_", PLOTTYPE)) {
              available.themes <- c(
                "Default" = "grey",
                "Black & White" = "bw",
                "Light" = "light",
                "Dark" = "dark",
                "Minimal" = "minimal",
                "Classic" = "classic",
                "Void" = "void",
                "Stata" = "stata",
                "Wall Street Journal" = "wsj",
                "Tufte" = "tufte",
                "Google Docs" = "gdocs",
                "FiveThirtyEight" = "fivethirtyeight",
                "Excel" = "excel",
                "Economist" = "economist"
              )

              if ("ggthemes" %in% installed.packages()) {
                theme.options <- names(available.themes)
              } else {
                theme.options <- c(
                  names(available.themes[1:7] ),
                  "Install additional themes..."
                )
              }

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Theme :")
              themeCombobox <- gcombobox(
                theme.options,
                selected = if (!is.null(curSet$gg_theme)) match(names(available.themes)[which(available.themes == curSet$gg_theme)], theme.options) else 1,
                handler = function(h, ...) {
                  if (svalue(themeCombobox) == "Install additional themes...") {
                    tryCatch({
                      if(gconfirm("Install ggthemes package?")) {
                        install.packages(
                          "ggthemes",
                          repos = c("https://r.docker.stat.auckland.ac.nz",
                                    "https://cran.stat.auckland.ac.nz")
                        )
                      }
                    },
                    finally = {
                      svalue(themeCombobox) <- names(available.themes)[which(available.themes == curSet$gg_theme)]
                    }
                    )
                  } else {
                    updateEverything()
                  }
                }
              )
              tbl[ii, 3:6, expand = TRUE] <- themeCombobox

              ii <- ii + 1
            }

            if (grepl("^gg_", PLOTTYPE) && !(PLOTTYPE %in% c("gg_pie", "gg_donut", "gg_barcode"))) {
              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Rotate :")
              rotateCheck <- gcheckbox("Plot")
              if (isTRUE(!is.null(curSet$rotation))) {
                svalue(rotateCheck) <- curSet$rotation
              }
              tbl[ii, 3:6, expand = TRUE] <- rotateCheck

              addHandlerChanged(rotateCheck, function(h, ...) updateEverything())

              ii <- ii + 1

              rotateLabelsX <- gcheckbox("x-axis Labels")
              tbl[ii, 3:6, expand = TRUE] <- rotateLabelsX
              if (isTRUE(!is.null(curSet$rotate_labels$x))) {
                svalue(rotateLabelsX) <- curSet$rotate_labels$x
              }

              ii <- ii + 1

              rotateLabelsY <- gcheckbox("y-axis Labels")
              tbl[ii, 3:6, expand = TRUE] <- rotateLabelsY
              if (isTRUE(!is.null(curSet$rotate_labels$y))) {
                svalue(rotateLabelsY) <- curSet$rotate_labels$y
              }

              ii <- ii + 1

              addHandlerChanged(rotateLabelsX, function(h, ...) updateEverything())
              addHandlerChanged(rotateLabelsY, function(h, ...) updateEverything())
            }

            if (PLOTTYPE %in% c("gg_violin", "gg_density")) {
              tbl[ii, 1:6, expand = TRUE] <- sectionTitle("Density Options")
              ii <- ii + 1

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Smoothing :")
              smoothSlider <- gslider(0.25, 4, 0.25, value = ifelse(is.null(curSet$adjust), 1, curSet$adjust),
                                      handler = function(h, ...) {
                if (!is.null(timer))
                  if (timer$started) timer$stop_timer()
                timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
              })

              # if (isTRUE(!is.null(curSet$adjust))) {
              #   svalue(smoothSlider) <- curSet$adjust
              # } else {
              #   svalue(smoothSlider) <- 1
              # }

              tbl[ii, 3:6, expand = TRUE] <- smoothSlider

              ii <- ii + 1
            }

            if (PLOTTYPE %in% c("gg_barcode")) {
              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Size :")
              barcodeSize <- gslider(from = 5, to = 20, by = 1, value = 16)
              tbl[ii, 3:6, expand = TRUE] <- barcodeSize

              if (isTRUE(!is.null(curSet$gg_barSize))) {
                svalue(barcodeSize) <- curSet$gg_barSize
              } else {
                svalue(barcodeSize) <- 16
              }

              addHandlerChanged(barcodeSize, handler = function(h, ...) {
                if (!is.null(timer))
                  if (timer$started) timer$stop_timer()
                timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
              })

              ii <- ii + 1
            }

            if (PLOTTYPE %in% c("gg_barcode2", "gg_barcode3")) {
              tbl[ii, 1:6, expand = TRUE] <- sectionTitle("Barcode Options")
              ii <- ii + 1

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Width :")
              barcodeWidth <- gslider(from = 0.25, to = 3, by = 0.25, value = 1)
              tbl[ii, 3:6, expand = TRUE] <- barcodeWidth

              ii <- ii + 1

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Height :")
              barcodeHeight <- gslider(from = 0.1, to = 1, by = 0.1, value = 0.5)
              tbl[ii, 3:6, expand = TRUE] <- barcodeHeight

              ii <- ii + 1

              if (isTRUE(!is.null(curSet$gg_height))) {
                svalue(barcodeHeight) <- curSet$gg_height
              } else {
                svalue(barcodeHeight) <- 0.5
              }

              if (isTRUE(!is.null(curSet$gg_width))) {
                svalue(barcodeWidth) <- curSet$gg_width
              } else {
                svalue(barcodeWidth) <- 1
              }

              addHandlerChanged(barcodeWidth, handler = function(h, ...) {
                if (!is.null(timer))
                  if (timer$started) timer$stop_timer()
                timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
              })

              addHandlerChanged(barcodeHeight, handler = function(h, ...) {
                if (!is.null(timer))
                  if (timer$started) timer$stop_timer()
                timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
              })

            }

            if (PLOTTYPE %in% c("gg_divergingstackedbar")) {
              tbl[ii, 1:6, expand = TRUE] <- sectionTitle("Barchart Options")
              ii <- ii + 1

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Cut-point :")
              stackedCutPoint <- gcombobox(c("Default", 1:(attr(PLOTTYPES, "cat.levels") - 1)))
              tbl[ii, 3:6, expand = TRUE] <- stackedCutPoint
              ii <- ii + 1

              addHandlerChanged(stackedCutPoint, function(h, ...) updateEverything())
            }

            if (PLOTTYPE %in% c("gg_lollipop2", "gg_lollipop", "gg_freqpolygon", "gg_dotstrip", "gg_beeswarm")) {
              tbl[ii, 1:6, expand = TRUE] <- sectionTitle("Point Options")
              ii <- ii + 1

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Size :")
              pointSize <- gslider(from = 1, to = 10, by = 1)
              tbl[ii, 3:6, expand = TRUE] <- pointSize

              if (isTRUE(!is.null(curSet$gg_size))) {
                svalue(pointSize) <- curSet$gg_size
              } else {
                svalue(pointSize) <- 6
              }

              addHandlerChanged(pointSize, handler = function(h, ...) {
                if (!is.null(timer))
                  if (timer$started) timer$stop_timer()
                timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
              })

              ii <- ii + 1
            }

            if (PLOTTYPE %in% c("gg_violin", "gg_barcode", "gg_dotstrip", "gg_barcode2", "gg_barcode3")) {
              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Transparency :")
              transpSlider <- gslider(from = 0, to = 100,
                                      by = 1, value = 100 * (1 - curSet$alpha))
              tbl[ii, 3:6, expand = TRUE] <- transpSlider

              addHandlerChanged(transpSlider,
                                handler = function(h, ...) {
                                  if (!is.null(timer))
                                    if (timer$started) timer$stop_timer()
                                  timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                })

              ii <- ii + 1
            }

            if (PLOTTYPE %in% c("gg_density", "gg_ridgeline")) {
              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Transparency :")
              transpSlider <- gslider(from = 0, to = 100,
                                      by = 1,
                                      value = ifelse(
                                        attr(PLOTTYPES, "null.y"),
                                        100 * (1 - curSet$alpha),
                                        ifelse(is.null(curSet$alpha_densitygroup), 60, 100 * (1 - curSet$alpha_densitygroup))
                                      )
              )
              tbl[ii, 3:6, expand = TRUE] <- transpSlider

              addHandlerChanged(transpSlider,
                                handler = function(h, ...) {
                                  if (!is.null(timer))
                                    if (timer$started) timer$stop_timer()
                                  timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                })

              ii <- ii + 1
            }

            if (PLOTTYPE %in% c("gg_poppyramid")) {
              tbl[ii, 1:6, expand = TRUE] <- sectionTitle("Pyramid Options")
              ii <- ii + 1

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Number of bins :")
              pyramidBins <- gslider(5, 50, by = 5, value = 30)
              tbl[ii, 3:6, expand = TRUE] <- pyramidBins

              if (isTRUE(!is.null(curSet$gg_bins))) {
                svalue(pyramidBins) <- curSet$gg_bins
              } else {
                svalue(pyramidBins) <- 30
              }

              addHandlerChanged(pyramidBins, handler = function(h, ...) {
                if (!is.null(timer))
                  if (timer$started) timer$stop_timer()
                timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
              })

              ii <- ii + 1
            }

            if (PLOTTYPE %in% c("gg_lollipop", "gg_boxplot", "gg_cumcurve", "gg_lollipop2", "gg_freqpolygon")) {
              tbl[ii, 1:6, expand = TRUE] <- sectionTitle("Line Options")
              ii <- ii + 1

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Width :")
              lwdSlider <- gslider(1, 5, value = 1)
              tbl[ii, 3:6, expand = TRUE] <- lwdSlider

              if (isTRUE(!is.null(curSet$gg_lwd))) {
                svalue(lwdSlider) <- curSet$gg_lwd
              } else {
                svalue(lwdSlider) <- 1
              }

              addHandlerChanged(lwdSlider, handler = function(h, ...) {
                if (!is.null(timer))
                  if (timer$started) timer$stop_timer()
                timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
              })

              ii <- ii + 1
            }

            if (PLOTTYPE %in% c("gg_column", "gg_lollipop2", "gg_pie", "gg_donut")) {
              tbl[ii, 1:6, expand = TRUE] <- sectionTitle("Sorting")
              ii <- ii + 1

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Sort categories by size :")
              # sortCheck <- gcheckbox(handler = function(h, ...) updateEverything())
              sortCheck <- gcombobox(c("None", "Ascending", "Descending"), handler = function(h, ...) updateEverything())
              tbl[ii, 3:6, expand = TRUE] <- sortCheck

              if (isTRUE(!is.null(curSet$ordered))) {
                print(curSet$ordered)
                svalue(sortCheck, index = TRUE) <- ifelse(curSet$ordered == "asc", 2, ifelse(curSet$ordered == "desc", 3, 1))
              } else {
                svalue(sortCheck, TRUE) <- 1
              }

              ii <- ii + 1
            }

            if (PLOTTYPE %in% c("gg_gridplot")) {
              tbl[ii, 1:6, expand = TRUE] <- sectionTitle("Gridplot Options")
              ii <- ii + 1

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("N observations/square :")
              n_fun <- function(n) {
                if (n > 1000) {
                  if (n > 5 * 10^ceiling(log10(n) - 1) && n > 5 * 10^ceiling(log10(n + 1) - 1)) {
                    10^(floor(log10(n)) - 1)
                  } else {
                    10^(floor(log10(n)) - 2)
                  }
                } else {
                  1
                }
              }
              gridNPerSquare <- gedit(n_fun(nrow(GUI$getActiveData())))
              addHandlerChanged(gridNPerSquare, function(h, ...) updateEverything())
              tbl[ii, 3:6, expand = TRUE] <- gridNPerSquare

              ii <- ii + 1
            }

            if (PLOTTYPE %in% c("gg_quasirandom")) {
              tbl[ii, 1:6, expand = TRUE] <- sectionTitle("Beeswarm Options")
              ii <- ii + 1

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Method :")
              swarmMethod <- gcombobox(c("quasirandom", "pseudorandom", "smiley", "frowney"))
              tbl[ii, 3:6, expand = TRUE] <- swarmMethod
              addHandlerChanged(swarmMethod, handler = function(h, ...) updateEverything())

              ii <- ii + 1

              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Swarm width :")
              swarmWidth <- gslider(0, 1, 0.1, value = if (!is.null(curSet$gg_swarmwidth)) curSet$gg_swarmwidth else 0.4)
              tbl[ii, 3:6, expand = TRUE] <- swarmWidth

              addHandlerChanged(swarmWidth, handler = function(h, ...) {
                if (!is.null(timer))
                  if (timer$started) timer$stop_timer()
                timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
              })

              ii <- ii + 1
            }

            if (grepl("^gg_", PLOTTYPE)) {


              tbl[ii, 1:6, expand = TRUE] <- sectionTitle("Export Plot")
              ii <- ii + 1

              tbl[ii, 3, expand = TRUE] <- gbutton("Store Code", handler = function(h, ...) {
                GUI$plot_history$add(GUI$curPlot)
                GUI$rhistory$add(paste0(attr(GUI$curPlot, "code"), collapse = "\n\n"))
              })

              tbl[ii, 4, expand = TRUE] <- gbutton("View Code", handler = function(h, ...) {
                GUI$plot_history$show()
              })

              ii <- ii + 1

              if (!PLOTTYPE %in% c("gg_pie", "gg_donut", "gg_gridplot", "gg_barcode2", "gg_barcode", "gg_ridgeline")) {
                tbl[ii, 3:4, anchor = c(1, 0), expand = TRUE] <- gbutton("Interactive Plot (via plotly)", handler = function(h, ...) {
                  suppressWarnings(
                    print(plotly::ggplotly())
                  )
                })

                ii <- ii + 1
              }
            }

            # if (PLOTTYPE %in% c("gg_column2", "gg_lollipop")) {
            #   label_options <- colnames(GUI$getActiveData())[sapply(GUI$getActiveData(), function(x) !is.numeric(x) && length(unique(x)) == length(x))]
            #   tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Label by:")
            #   labelVar <- gcombobox(c("", label_options), handler = function(h, ...) updateEverything())
            #   tbl[ii, 3:6, expand = TRUE] <- labelVar
            # }

            updateEverything <<- function(update = auto) {
                ## To easily diable automatic updating of plot, add this argument,
                ## otherwise would have to block/unblock handlers
                if (!update)
                    return()

                ## Things that don't need checking:
                newSet <- list(cex = svalue(cexMain))

                ## General appearance:
                newSet$bg <-
                    if (svalue(bgCol) %in% names(bgColours))
                        bgColours[[svalue(bgCol, index = TRUE)]]
                    else if (!inherits(try(col2rgb(svalue(bgCol)), silent = TRUE), "try-error"))
                        svalue(bgCol)
                    else
                        curSet$bg

                ## Size
                if (!PLOTTYPE %in% c("bar")) {
                  switch(PLOTTYPE,
                         "scatter" = newSet$cex.pt <- svalue(cexPt),
                         "dot"     =, "hist" = newSet$cex.dotpt <- svalue(cexPt),
                         "grid"    = newSet$scatter.grid.bins <- iNZightPlots::inzpar()$scatter.grid.bins / svalue(cexPt),
                         "hex"     = newSet$hex.bins <- iNZightPlots::inzpar()$hex.bins / svalue(cexPt))
                }
                if (PLOTTYPE == "scatter") {
                    newSet <- c(newSet, list(sizeby = GUI$getActiveData()[[svalue(sizeVar)]]))
                    newSet$varnames <- c(newSet$varnames,
                                         list(sizeby = svalue(sizeVar)))
                    newSet$resize.method <- svalue(sizeMethod)
                }
                if (PLOTTYPE == "hex")
                    newSet <- c(newSet, list(hex.style = svalue(hexStyle)))

                ## Colour
                if (PLOTTYPE %in% c("scatter", "hex", "dot", "bar")) {
                    setPal <- FALSE
                    if (bars & !is.null(curSet$y)) {
                      setPal <- TRUE
                    } else if (svalue(colVar, TRUE) > 1) {
                      setPal <- TRUE
                    }
                    if (setPal) {
                        ## colouring by a variable - and a palette
                        if (!bars | is.null(curSet$y)) {
                          newSet$colby <-
                              if (PLOTTYPE == "hex")
                                  iNZightPlots::convert.to.factor(
                                      GUI$getActiveData()[[svalue(colVar)]])
                              else GUI$getActiveData()[[svalue(colVar)]]
                          newSet$varnames <- c(newSet$varnames,
                                               list(colby = svalue(colVar)))
                          newSet$col.method <- ifelse(svalue(useRank), "rank", "linear")
                        }
                        newSet$reverse.palette <- svalue(revPal)
                        palCatName <-
                            names(colourPalettes$cat)[svalue(palCat, index = TRUE)]
                        palContName <-
                            names(colourPalettes$cont)[svalue(palCont, index = TRUE)]
                        if (bars) {
                          newSet$col.fun <- iNZightPlots::inzpalette(palCatName)
                        } else {
                          newSet$col.fun <-
                              if (EMPH.LEVEL > 0)
                                  function(n)
                                      iNZightPlots::emphasize_pal_colour(
                                          n, k = EMPH.LEVEL, cat = is.factor(newSet$colby),
                                          ncat = svalue(cycleN),
                                          fn = if (is.numeric(newSet$colby))
                                                   iNZightPlots::inzpalette(palContName)
                                               else iNZightPlots::inzpalette(palCatName)
                                      )
                              else if (is.numeric(newSet$colby))
                                  iNZightPlots::inzpalette(palContName)
                              else iNZightPlots::inzpalette(palCatName)
                        }

                        newSet$plot.features <- list(order.first = NULL)
                        if (!bars) {
                          if (EMPH.LEVEL > 0) {
                              ## need to add "order.first" to plot features:
                              if (is_cat(newSet$colby)) {
                                  newSet$plot.features <-
                                      list(order.first = which(newSet$colby ==
                                                               levels(newSet$colby)[EMPH.LEVEL]))
                              } else if (is_num(newSet$colby)) {
                                  Qs <- seq(min(newSet$colby, na.rm = TRUE),
                                            max(newSet$colby, na.rm = TRUE),
                                            length = svalue(cycleN) + 1)
                                  newSet$plot.features <-
                                      list(order.first = which(newSet$colby >= Qs[EMPH.LEVEL] &
                                                               newSet$colby < Qs[EMPH.LEVEL + 1]))
                              }
                          }

                          visible(cycleLbl) <- visible(cyclePanel) <- TRUE
                          visible(cycleNlab) <- visible(cycleN) <- is_num(newSet$colby)
                          svalue(cycleLbl) <- ifelse(visible(cycleN),
                                                     "Cycle quantiles :", "Cycle levels :")
                        }
                    } else {
                        newSet <- c(newSet, list(colby = NULL))
                        newSet$varnames <- c(newSet$varnames, list(colby = NULL))
                        if (bars) {
                          newSet$bar.fill <-
                              if (svalue(barCol) %in% names(barColours))
                                  barColours[[svalue(barCol, index = TRUE)]]
                              else if (!inherits(try(col2rgb(svalue(barCol)), silent = TRUE),
                                                 "try-error"))
                                  svalue(barCol)
                              else
                                  curSet$bar.fill
                        } else {
                          newSet$col.pt <-
                              if (svalue(ptCol) %in% names(pointColours))
                                  pointColours[[svalue(ptCol, index = TRUE)]]
                              else if (!inherits(try(col2rgb(svalue(ptCol)), silent = TRUE),
                                                 "try-error"))
                                  svalue(ptCol)
                              else
                                  curSet$col.pt
                          visible(cycleLbl) <- visible(cyclePanel) <- FALSE
                        }
                    }
                }
                if (PLOTTYPE == "hist") {
                    newSet$bar.fill <-
                        if (svalue(barCol) %in% names(barColours))
                            barColours[[svalue(barCol, index = TRUE)]]
                        else if (!inherits(try(col2rgb(svalue(barCol)), silent = TRUE),
                                           "try-error"))
                            svalue(barCol)
                        else
                            curSet$bar.fill
                }

                if (PLOTTYPE %in% c("dot", "scatter")) {
                    newSet$alpha <- 1 - svalue(transpSlider) / 100
                }

                ## Plotting Symbol
                if (PLOTTYPE %in% c("scatter", "dot")) {
                    newSet <- c(newSet, list(symbolby = NULL))
                    newSet$varnames <- c(newSet$varnames, list(symbolby = NULL))
                    if (svalue(pchMatch) & !is.null(newSet$colby)) {
                        if (length(levels(newSet$colby)) %in% 1:5) {
                            newSet$symbolby <- newSet$colby
                            newSet$varnames$symbolby = newSet$varnames$colby
                        }
                    } else if (svalue(symVar, TRUE) > 1) {
                        newSet$symbolby <- GUI$getActiveData()[[svalue(symVar)]]
                        newSet$varnames$symbolby <- svalue(symVar)
                    }
                    newSet$pch <- symVals[svalue(symPch, index = TRUE)]
                    newSet$fill.pt <- ifelse(svalue(fillSym), "fill", "transparent")
                    newSet$lwd.pt <- svalue(symLwd)
                }

                if (grepl("^gg_", PLOTTYPE)) {
                  if (!(PLOTTYPE %in% c("gg_pie", "gg_donut", "gg_cumcurve", "gg_barcode"))) {
                    newSet$rotation <- svalue(rotateCheck)
                  }

                  if(grepl("^gg_", PLOTTYPE) &&
                     (PLOTTYPE %in% c("gg_pie", "gg_donut", "gg_column", "gg_heatmap", "gg_stackedcolumn", "gg_poppyramid", "gg_spine", "gg_mosaic", "gg_divergingstackedbar", "gg_gridplot")) ||
                     (!attr(PLOTTYPES, "null.y") && PLOTTYPE %in% c("gg_violin", "gg_barcode", "gg_boxplot", "gg_cumcurve", "gg_freqpolygon", "gg_dotstrip", "gg_density", "gg_quasirandom", "gg_lollipop2", "gg_ridgeline", "gg_barcode3"))
                  ) {
                    newSet$palette <- svalue(paletteCombobox)
                  }

                  if (PLOTTYPE %in% c("gg_violin", "gg_column2", "gg_lollipop", "gg_boxplot", "gg_density", "gg_cumcurve", "gg_quasirandom", "gg_lollipop2", "gg_barcode3", "gg_barcode", "gg_dotstrip") && attr(PLOTTYPES, "null.y")) {
                    if (svalue(colourCombobox) != "" && valid_colour(svalue(colourCombobox))) {
                      newSet$fill_colour <- svalue(colourCombobox)
                    } else if (svalue(colourCombobox) == "") {
                      newSet$fill_colour <- ""
                    }
                  }

                  if (PLOTTYPE %in% c("gg_column", "gg_lollipop2", "gg_pie", "gg_donut")) {
                    newSet$ordered <- if(svalue(sortCheck, index = TRUE) == 1) FALSE else c("asc", "desc")[svalue(sortCheck, TRUE) - 1]
                  }

                  if (PLOTTYPE %in% c("gg_lollipop", "gg_column2")) {
                    # newSet$desc <- svalue(sortOrder) == "Descending"
                  }

                  if (PLOTTYPE %in% c("gg_violin", "gg_density")) {
                    newSet$adjust <- svalue(smoothSlider)
                  }

                  if (PLOTTYPE %in% c("gg_violin", "gg_barcode", "gg_dotstrip", "gg_barcode2", "gg_barcode3")) {
                    newSet$alpha <- 1 - svalue(transpSlider) / 100
                  }

                  if (PLOTTYPE %in% c("gg_density", "gg_ridgeline")) {
                    if (attr(PLOTTYPES, "null.y")) {
                      newSet$alpha <- 1 - svalue(transpSlider) / 100
                    } else {
                      newSet$alpha_densitygroup <- 1 - svalue(transpSlider) / 100
                    }
                  }

                  if (PLOTTYPE %in% c("gg_barcode")) {
                    newSet$gg_barSize <- svalue(barcodeSize)
                  }

                  if (PLOTTYPE %in% c("gg_barcode2", "gg_barcode3")) {
                    newSet$gg_width <- svalue(barcodeWidth)
                    newSet$gg_height <- svalue(barcodeHeight)
                  }

                  if (PLOTTYPE %in% c("gg_lollipop2", "gg_lollipop", "gg_freqpolygon", "gg_dotstrip", "gg_beeswarm")) {
                    newSet$gg_size <- svalue(pointSize)
                  }

                  if (PLOTTYPE %in% c("gg_poppyramid")) {
                    newSet$gg_bins <- svalue(pyramidBins)
                  }

                  if (PLOTTYPE %in% c("gg_lollipop", "gg_boxplot", "gg_cumcurve", "gg_lollipop2", "gg_freqpolygon")) {
                    newSet$gg_lwd <- svalue(lwdSlider)
                  }

                  if (PLOTTYPE %in% c("gg_gridplot")) {
                    newSet$gg_perN <- svalue(gridNPerSquare)
                  }

                  if (PLOTTYPE %in% c("gg_quasirandom")) {
                    newSet$gg_swarmwidth <- svalue(swarmWidth)
                    newSet$gg_method <- svalue(swarmMethod)
                  }

                  newSet$gg_theme <- available.themes[svalue(themeCombobox)]

                  if (!(PLOTTYPE %in% c("gg_pie", "gg_donut"))) {
                    newSet$rotate_labels <- list()

                    newSet$rotate_labels$x <- svalue(rotateLabelsX)
                    newSet$rotate_labels$y <- svalue(rotateLabelsY)
                  }

                  if (PLOTTYPE %in% c("gg_divergingstackedbar")) {
                    newSet$gg_cutpoint <- svalue(stackedCutPoint)
                  }
                }

                if (PLOTTYPE %in% c("dot", "hist")) {
                  newSet$boxplot <- svalue(showBoxplot)
                  newSet$mean_indicator <- svalue(showMean)
                }

                GUI$getActiveDoc()$setSettings(newSet)
                updateSettings()
            }

            addHandlerChanged(bgCol,
                              handler = function(h, ...) {
                                  if (!is.null(timer))
                                      if (timer$started) timer$stop_timer()
                                  timer <<- gtimer(500, function(...) {
                                      if (nchar(svalue(bgCol)) >= 3)
                                          updateEverything()
                                  }, one.shot = TRUE)
                              })

            addHandlerChanged(cexMain,
                              handler = function(h, ...) {
                                  if (!is.null(timer))
                                      if (timer$started) timer$stop_timer()
                                  timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                              })

            if (!(PLOTTYPE %in% c("bar") || grepl("^gg_", PLOTTYPE))) {
                addHandlerChanged(cexPt,
                                  handler = function(h, ...) {
                                      if (!is.null(timer))
                                          if (timer$started)
                                              if (timer$started) timer$stop_timer()
                                      timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })
            }

            if (PLOTTYPE == "scatter") {
                addHandlerChanged(sizeVar, handler = function(h, ...) {
                    visible(sizeDesc) <- visible(resizeLbl) <- visible(sizeMethod) <-
                        svalue(sizeVar, index = TRUE) > 1

                    updateEverything()
                })
                addHandlerChanged(sizeMethod, handler = function(h, ...) {
                    svalue(sizeDesc) <- paste(sizeDescs[[svalue(sizeMethod, index = TRUE)]])
                    updateEverything()
                })
            }
            if (PLOTTYPE == "hex") {
                addHandlerChanged(hexStyle, handler = function(h, ...) updateEverything())
            }

            if (PLOTTYPE %in% c("scatter", "hex", "dot", "bar", "hist")) {
                if (bars | hist) {
                    addHandlerChanged(barCol,
                                      handler = function(h, ...) {
                                          if (!is.null(timer))
                                              if (timer$started) timer$stop_timer()
                                          timer <<- gtimer(500, function(...) {
                                              if (nchar(svalue(barCol)) >= 3)
                                                  updateEverything()
                                          }, one.shot = TRUE)
                                      })
                } else {
                    addHandlerChanged(ptCol,
                                      handler = function(h, ...) {
                                          if (!is.null(timer))
                                              if (timer$started) timer$stop_timer()
                                          timer <<- gtimer(500, function(...) {
                                              if (nchar(svalue(ptCol)) >= 3)
                                                  updateEverything()
                                          }, one.shot = TRUE)
                                      })
                }
                if (!hist & (!bars | is.null(curSet$y))) {
                    addHandlerChanged(colVar, handler = function(h, ...) {
                        EMPH.LEVEL <<- 0
                        if (PLOTTYPE %in% c("dot", "scatter")) symbolMatch()
                        if (svalue(h$obj, index = TRUE) == 1) {
                            svalue(colLabel) <- ifelse(bars, "Bar colour : ", "Point colour :")
                            visible(useRank) <- visible(palAdvanced) <- visible(palCont) <-
                                visible(palCat) <- FALSE
                            if (bars) visible(barCol) <- TRUE
                            else visible(ptCol) <- TRUE
                        } else {
                            svalue(colLabel) <- "Palette :"
                            if (bars) visible(barCol) <- FALSE
                            else visible(ptCol) <- FALSE
                            if (is_num(GUI$getActiveData()[[svalue(h$obj)]]) &
                                PLOTTYPE != "hex") {
                                visible(palCat) <- FALSE
                                visible(useRank) <- visible(palCont) <- TRUE
                            } else {
                                visible(useRank) <- visible(palCont) <- FALSE
                                visible(palCat) <- TRUE
                            }
                            visible(palAdvanced) <- TRUE
                        }
                        visible(revPal) <- visible(palCat) || visible(palCont)
                        updateEverything()
                    })
                    addHandlerChanged(useRank, handler = function(h, ...) updateEverything())
                }
                if (!hist) {
                    addHandlerChanged(palCat, handler = function(h, ...) updateEverything())
                    addHandlerChanged(palCont, handler = function(h, ...) updateEverything())
                    addHandlerChanged(revPal, handler = function(h, ...) updateEverything())
                }
            }
            if (PLOTTYPE %in% c("scatter", "dot")) {
                addHandlerChanged(transpSlider,
                                  handler = function(h, ...) {
                                      enabled(fillSym) <- svalue(transpSlider) == 0
                                      if (!is.null(timer))
                                          if (timer$started) timer$stop_timer()
                                      timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })
            }

            if (PLOTTYPE %in% c("scatter", "dot")) {
                addHandlerChanged(pchMatch, handler = function(h, ...) {
                    enabled(symVar) <- enabled(symPch) <- !svalue(pchMatch)
                    updateEverything()
                })
                addHandlerChanged(symPch, handler = function(h, ...) {
                    if (svalue(symPch, index = TRUE) %in% c(3:5) && nrow(GUI$getActiveData()) > 2000) {
                        ## TRANSPARENCY VERY SLOW!
                        if (svalue(transpSlider) > 0) {
                            gmessage("Transparency reset to zero.\n\nWARNING: drawing can be VERY slow if using transparent symbols that are NOT circles or squares.")
                            blockHandlers(transpSlider)
                            svalue(transpSlider) <- 0
                            unblockHandlers(transpSlider)
                        }
                        visible(transpWarning) <- TRUE
                    } else {
                        visible(transpWarning) <- FALSE
                    }
                    updateEverything()
                })
                addHandlerChanged(symVar, handler = function(h, ...) updateEverything())
                addHandlerChanged(symLwd, handler = function(h, ...) updateEverything())
                addHandlerChanged(fillSym, handler = function(h, ...) updateEverything())
            }

            add(optGrp, tbl)
        },
        features = function() {
            tbl <- glayout()
            ii <- 3

            PLOTTYPE <- GUI$plotType

            ## PLOT APPEARANCE
            tbl[ii,  1:6, anchor = c(-1,-1), expand = TRUE] <- sectionTitle("Trend Curves")
            ii <- ii + 1

            tbl[ii, 4:5, anchor = c(-1, 0), expand = TRUE] <- glabel("Line colour")
            tbl[ii, 6, anchor = c(-1, 0), expand = TRUE] <- glabel("Line type")
            ii <- ii + 1

            lineColours <- c("red", "black", "blue", "green4", "magenta",
                             "yellow", "pink", "grey", "orange")
            colBoxWidth <- 100

            trendCurves <- c("linear", "quadratic", "cubic")
            trendLin <- gcheckbox("linear", checked = "linear" %in% curSet$trend)
            trendLinCol <- gcombobox(
                c(if (!curSet$col.trend$linear %in% lineColours) curSet$col.trend$linear, lineColours),
                editable = TRUE,
                selected = which(lineColours == curSet$col.trend$linear))
            tbl[ii, 1:3, anchor = c(-1, 0), expand = TRUE] <- trendLin
            tbl[ii, 4:5] <- trendLinCol
            trendLinCol$widget$setSizeRequest(colBoxWidth, -1)
            trendLinLTY <- gspinbutton(1, 6, by = 1, value = curSet$lty.trend[["linear"]])
            tbl[ii, 6] <- trendLinLTY
            ii <- ii + 1

            trendQuad <- gcheckbox("quadratic", checked = "quadratic" %in% curSet$trend)
            trendQuadCol <- gcombobox(
                c(if (!curSet$col.trend$quadratic %in% lineColours) curSet$col.trend$quadratic, lineColours),
                editable = TRUE,
                selected = which(lineColours == curSet$col.trend$quadratic))
            tbl[ii, 1:3, anchor = c(-1, 0), expand = TRUE] <- trendQuad
            tbl[ii, 4:5] <- trendQuadCol
            trendQuadCol$widget$setSizeRequest(colBoxWidth, -1)
            trendQuadLTY <- gspinbutton(1, 6, by = 1, value = curSet$lty.trend[["quadratic"]])
            tbl[ii, 6] <- trendQuadLTY
            ii <- ii + 1

            trendCub <- gcheckbox("cubic", checked = "cubic" %in% curSet$trend)
            trendCubCol <- gcombobox(
                c(if (!curSet$col.trend$cubic %in% lineColours) curSet$col.trend$cubic, lineColours),
                editable = TRUE,
                selected = which(lineColours == curSet$col.trend$cubic))
            tbl[ii, 1:3, anchor = c(-1, 0), expand = TRUE] <- trendCub
            tbl[ii, 4:5] <- trendCubCol
            trendCubCol$widget$setSizeRequest(colBoxWidth, -1)
            trendCubLTY <- gspinbutton(1, 6, by = 1, value = curSet$lty.trend[["cubic"]])
            tbl[ii, 6] <- trendCubLTY
            ii <- ii + 1


            ii <- ii + 1
            tbl[ii,  1:6, anchor = c(-1,-1), expand = TRUE] <- sectionTitle("Smoother")
            ii <- ii + 1

            smooth <- gcheckbox("Add smoother",
                                checked = curSet$smooth != 0 | !is.null(curSet$quant.smooth))
            smoothCol <- gcombobox(lineColours, editable = TRUE,
                                   selected =
                                       if (curSet$col.smooth %in% lineColours)
                                           which(lineColours == curSet$col.smooth)
                                       else 1)
            tbl[ii, 1:3, anchor = c(-1, 0), expand = TRUE] <- smooth
            tbl[ii, 4:5] <- smoothCol
            smoothCol$widget$setSizeRequest(colBoxWidth, -1)
            ii <- ii + 1

            qsmooth <- gcheckbox("Use Quantiles",
                                 checked = !is.null(curSet$quant.smooth))
            tbl[ii, 1:3, anchor = c(-1, 0), expand = TRUE] <- qsmooth

            smoothF <- gslider(from = 0.1, to = 1, by = 0.01,
                               value = ifelse(curSet$smooth == 0, 0.7, curSet$smooth))
            tbl[ii, 4:6] <- smoothF
            ii <- ii + 1

            visible(qsmooth) <- visible(smoothF) <- svalue(smooth)
            enabled(smoothF) <- !svalue(qsmooth)

            if (PLOTTYPE == "scatter") {
                ## join points
                tbl[ii,  1:6, anchor = c(-1,-1), expand = TRUE] <- sectionTitle("Join Points")
                ii <- ii + 1

                joinPoints <- gcheckbox("Join points by lines", checked = curSet$join)
                joinPointsCol <- gcombobox(lineColours, editable = TRUE,
                                           selected =
                                               if (curSet$col.line %in% lineColours)
                                                   which(lineColours == curSet$col.line)
                                               else 1)
                tbl[ii, 1:4, anchor = c(-1, 0), expand = TRUE] <- joinPoints
                tbl[ii, 5:6] <- joinPointsCol
                joinPointsCol$widget$setSizeRequest(colBoxWidth, -1)
                ii <- ii + 1

                if (is_cat(curSet$colby)) {
                    joinPointsBy <- gcheckbox(paste("For each level of", curSet$varnames$colby),
                                              selected = curSet$lines.by)
                    tbl[ii, 1:6, anchor = c(-1, 0), expand = TRUE] <- joinPointsBy
                    ii <- ii + 1
                }
            }

            ## extra settings ...
            tbl[ii,  1:6, anchor = c(-1,-1), expand = TRUE] <- sectionTitle("Trend Line Options")
            ii <- ii + 1

            ## For each level of COLBY
            if (is_cat(curSet$colby)) {
                trendBy <- gcheckbox(paste("For each level of", curSet$varnames$colby),
                                     checked = curSet$trend.by)
                trendParallel <- gcheckbox("Parallel trend lines (common slope)",
                                           checked = curSet$trend.parallel)
                tbl[ii, 1:6] <- trendBy
                ii <- ii + 1
                tbl[ii, 1:6] <- trendParallel
                ii <- ii + 1
            }
            activateOptions <- function() {
                if (is_cat(curSet$colby)) {
                    if (PLOTTYPE == "scatter") {
                        enabled(joinPointsBy) <- svalue(joinPoints)
                        enabled(joinPointsCol) <- !svalue(joinPointsBy)
                    }
                    enabled(trendBy) <- svalue(trendLin) | svalue(trendQuad) | svalue(trendCub) |
                        (svalue(smooth) & !svalue(qsmooth))
                    enabled(trendParallel) <- svalue(trendBy) &
                        (svalue(trendLin) | svalue(trendQuad) | svalue(trendCub))

                    enabled(trendLinCol) <- enabled(trendQuadCol) <- enabled(trendCubCol) <-
                        enabled(smoothCol) <- !(enabled(trendBy) & svalue(trendBy))
                }
            }
            activateOptions()

            lbl <- glabel("Line Width Multiplier :")
            lwdSpin <- gspinbutton(1, 4, by = 1, value = curSet$lwd)
            tbl[ii, 1:4, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 5, anchor = c(-1, 0), expand = FALSE] <- lwdSpin
            ii <- ii + 1

            loe <- gcheckbox("Add line of equality (x = y)", checked = curSet$LOE)
            tbl[ii, 1:6, anchor = c(-1, 0), expand = TRUE] <- loe
            ii <- ii + 1


            updateEverything <<- function(update = auto) {
                ## To easily diable automatic updating of plot, add this argument,
                ## otherwise would have to block/unblock handlers
                if (!update)
                    return()

                activateOptions()

                ## Things that don't need checking:
                newSet <- list(trend = trendCurves[c(svalue(trendLin),
                                                     svalue(trendQuad),
                                                     svalue(trendCub))],
                               LOE = svalue(loe),
                               lty.trend =
                                   list(linear = svalue(trendLinLTY),
                                        quadratic = svalue(trendQuadLTY),
                                        cubic = svalue(trendCubLTY)))
                ## if no trend specified, set to NULL
                if (length(newSet$trend) == 0) {
                    newSet <- modifyList(newSet, list(trend = NULL), keep.null = TRUE)
                }

                ## Trend line colours - editable:
                tCols <- curSet$col.trend
                if (!inherits(try(col2rgb(svalue(trendLinCol)), silent = TRUE), "try-error"))
                    tCols$linear <- svalue(trendLinCol)
                if (!inherits(try(col2rgb(svalue(trendQuadCol)), silent = TRUE), "try-error"))
                    tCols$quadratic <- svalue(trendQuadCol)
                if (!inherits(try(col2rgb(svalue(trendCubCol)), silent = TRUE), "try-error"))
                    tCols$cubic <- svalue(trendCubCol)
                newSet$col.trend <- tCols

                qsmth <- if (svalue(qsmooth) & svalue(smooth)) "default" else NULL
                newSet <- c(newSet, list(quant.smooth = qsmth))
                newSet$smooth <- ifelse(svalue(smooth) & is.null(qsmth),
                                        svalue(smoothF), 0)

                newSet$col.smooth <-
                    if (!inherits(try(col2rgb(svalue(smoothCol)), silent = TRUE), "try-error"))
                        svalue(smoothCol)
                    else curSet$col.smooth

                if (PLOTTYPE == "scatter") {
                    newSet$join <- svalue(joinPoints)
                    if (!inherits(try(col2rgb(svalue(joinPointsCol)), silent = TRUE), "try-error"))
                        newSet$col.line <- svalue(joinPointsCol)
                }

                newSet$lines.by <- FALSE
                if (is_cat(curSet$colby)) {
                    newSet$trend.by <- svalue(trendBy)
                    newSet$trend.parallel <- svalue(trendParallel)
                    if (PLOTTYPE == "scatter")
                        newSet$lines.by <- svalue(joinPointsBy)
                }

                newSet$lwd <- svalue(lwdSpin)


                GUI$getActiveDoc()$setSettings(newSet)
                updateSettings()
            }

            addHandlerChanged(trendLin, handler = function(h, ...) updateEverything())
            addHandlerChanged(trendQuad, handler = function(h, ...) updateEverything())
            addHandlerChanged(trendCub, handler = function(h, ...) updateEverything())
            addHandlerChanged(trendLinLTY, handler = function(h, ...) updateEverything())
            addHandlerChanged(trendQuadLTY, handler = function(h, ...) updateEverything())
            addHandlerChanged(trendCubLTY, handler = function(h, ...) updateEverything())

            addHandlerChanged(trendLinCol,
                              handler = function(h, ...) {
                                  if (!is.null(timer))
                                      if (timer$started) timer$stop_timer()
                                  timer <<- gtimer(500, function(...) {
                                      if (nchar(svalue(trendLinCol)) >= 3)
                                          updateEverything()
                                  }, one.shot = TRUE)
                              })

            addHandlerChanged(trendQuadCol,
                              handler = function(h, ...) {
                                  if (!is.null(timer))
                                      if (timer$started) timer$stop_timer()
                                  timer <<- gtimer(500, function(...) {
                                      if (nchar(svalue(trendQuadCol)) >= 3)
                                          updateEverything()
                                  }, one.shot = TRUE)
                              })

            addHandlerChanged(trendCubCol,
                              handler = function(h, ...) {
                                  if (!is.null(timer))
                                      if (timer$started) timer$stop_timer()
                                  timer <<- gtimer(500, function(...) {
                                      if (nchar(svalue(trendCubCol)) >= 3)
                                          updateEverything()
                                  }, one.shot = TRUE)
                              })

            addHandlerChanged(smooth, function(h, ...) {
                visible(qsmooth) <- visible(smoothF) <- svalue(smooth)
                enabled(smoothF) <- !svalue(qsmooth)
                updateEverything()
            })
            addHandlerChanged(qsmooth, function(h, ...) {
                enabled(smoothF) <- !svalue(qsmooth)
                updateEverything()
            })

            addHandlerChanged(smoothF,
                              handler = function(h, ...) {
                                  if (!is.null(timer))
                                      if (timer$started) timer$stop_timer()
                                  timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                              })

            addHandlerChanged(smoothCol,
                              handler = function(h, ...) {
                                  if (!is.null(timer))
                                      if (timer$started) timer$stop_timer()
                                  timer <<- gtimer(500, function(...) {
                                      if (nchar(svalue(smoothCol)) >= 3)
                                          updateEverything()
                                  }, one.shot = TRUE)
                              })

            if (PLOTTYPE == "scatter") {
                addHandlerChanged(joinPoints, function(h, ...) updateEverything())
                addHandlerChanged(joinPointsCol,
                                  handler = function(h, ...) {
                                      if (!is.null(timer))
                                          if (timer$started) timer$stop_timer()
                                      timer <<- gtimer(500, function(...) {
                                          if (nchar(svalue(joinPointsCol)) >= 3)
                                              updateEverything()
                                      }, one.shot = TRUE)
                                  })
            }
            if (is_cat(curSet$colby)) {
                addHandlerChanged(trendBy, function(h, ...) updateEverything())
                addHandlerChanged(trendParallel, function(h, ...) updateEverything())
                if (PLOTTYPE == "scatter")
                    addHandlerChanged(joinPointsBy, function(h, ...) updateEverything())
            }


            addHandlerChanged(lwdSpin, function(h, ...) updateEverything())
            addHandlerChanged(loe, function(h, ...) updateEverything())

            add(optGrp, tbl)
        },
        axes = function() {
            tbl <- glayout()
            ii <- 3

            PLOTTYPE <- GUI$plotType
            YAX <- TRUE
            YAXlbl <- FALSE
            if (PLOTTYPE %in% c("dot", "hist", "bar")) {
              YAXlbl <- YAX <- PLOTTYPE %in% c("dot", "hist") & !is.null(curSet$y)
            }

            ## AXIS LABELS
            tbl[ii,  1:2, anchor = c(-1,-1), expand = TRUE] <- sectionTitle("Axis Labels")
            ii <- ii + 1

            lbl <- glabel("Title :")
            labMain <- gedit(ifelse(is.null(curSet$main), "", curSet$main))
            tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 3:6, expand = TRUE] <- labMain
            ii <- ii + 1

            lbl <- glabel("x-axis :")
            labXlab <- gedit(ifelse(is.null(curSet$xlab), "", curSet$xlab))
            tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 3:6, expand = TRUE] <- labXlab
            ii <- ii + 1

            if (YAX) {
              lbl <- glabel("y-axis :")
              labYlab <- gedit(ifelse(is.null(curSet$ylab), "", curSet$ylab))
              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
              tbl[ii, 3:6, expand = TRUE] <- labYlab
              ii <- ii + 1

              if (YAXlbl) {
                intLabs <- gcheckbox("Display group labels inside graph",
                                     checked = curSet$internal.labels)
                tbl[ii, 3:6, anchor = c(-1, -1), expand= TRUE] <- intLabs
                ii <- ii + 1
              }
            }

            lbl <- glabel("TAB or ENTER/RETURN to apply changes")
            font(lbl) <- list(family = "sans", size = 8)
            tbl[ii, 3:6, anchor = c(-1, 0), expand = TRUE] <- lbl
            ii <- ii + 2

            lbl <- glabel("Enter a single space to print no label\nLeave blank to print default label")
            font(lbl) <- list(family = "sans", size = 8)
            tbl[ii, 3:6, anchor = c(-1, 0), expand = TRUE] <- lbl
            ii <- ii + 1


            if (PLOTTYPE == "scatter") {
                ## JITTER and RUGS
                tbl[ii,  1:2, anchor = c(-1,-1), expand = TRUE] <- sectionTitle("Axis Features")
                ii <- ii + 1

                lbl <- glabel("Jitter :")
                if (any(sapply(curSet$varnames[c("x", "y")], nchar) > 15)) {
                    xJit <- gcheckbox("x-variable", checked = curSet$jitter %in% c("x", "xy"))
                    yJit <- gcheckbox("y-variable", checked = curSet$jitter %in% c("y", "xy"))
                } else {
                    xJit <- gcheckbox(curSet$varnames$y, checked = curSet$jitter %in% c("x", "xy"))
                    yJit <- gcheckbox(curSet$varnames$x, checked = curSet$jitter %in% c("y", "xy"))
                }
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:4, anchor = c(-1, 0), expand = TRUE] <- xJit
                tbl[ii, 5:6, anchor = c(-1, 0), expand = TRUE] <- yJit
                ii <- ii + 1

                lbl <- glabel("Rugs :")
                if (any(sapply(curSet$varnames[c("x", "y")], nchar) > 15)) {
                    xRug <- gcheckbox("x-variable", checked = curSet$rug %in% c("x", "xy"))
                    yRug <- gcheckbox("y-variable", checked = curSet$rug %in% c("y", "xy"))
                } else {
                    xRug <- gcheckbox(curSet$varnames$y, checked = curSet$rug %in% c("x", "xy"))
                    yRug <- gcheckbox(curSet$varnames$x, checked = curSet$rug %in% c("y", "xy"))
                }
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:4, anchor = c(-1, 0), expand = TRUE] <- xRug
                tbl[ii, 5:6, anchor = c(-1, 0), expand = TRUE] <- yRug
                ii <- ii + 1
            }

            if (PLOTTYPE == "bar") {
                ii <- ii + 1
                tbl[ii, 1:2, anchor = c(-1, -1), expand = TRUE] <-
                    sectionTitle("Y axis options")

                ## percentages or counts
                ii <- ii + 1
                lbl <- glabel("Display values as: ")
                ycounts <- gradio(
                    c("Percentages (%)", "Counts"),
                    selected = 1 + curSet$bar.counts,
                    horizontal = TRUE
                )
                tbl[ii, 1:2,
                    expand = TRUE,
                    fill = TRUE,
                    anchor = c(1, 0)] <- lbl
                tbl[ii, 3:6, expand = TRUE] <- ycounts

                ii <- ii + 1
                if (length(levels(curSet$x)) > 2) {
                    ## Number of bars
                    tbl[ii,  1:2, anchor = c(-1,-1), expand = TRUE] <- sectionTitle("Number of Bars")
                    ii <- ii + 1

                    zoom <- if (!is.null(curSet$zoombars)) curSet$zoombars else NULL

                    lbl <- glabel("Number of bars: ")
                    NBARS <- gslider(2, min(30, length(levels(curSet$x))),
                                     by = 1, value = min(30, length(levels(curSet$x))))
                    tbl[ii, 1:2, expand = TRUE, fill = TRUE, anchor = c(-1, 0)] <- lbl
                    tbl[ii, 3:6, expand = TRUE] <- NBARS
                    ii <- ii + 1

                    lbl <- glabel("Starting point: ")
                    START <- gslider(levels(curSet$x)[1:(length(levels(curSet$x)) - 1)])
                    tbl[ii, 1:2, expand = TRUE, fill = TRUE, anchor = c(-1, 0)] <- lbl
                    tbl[ii, 3:6, expand = TRUE] <- START
                    ii <- ii + 1

                    if (!is.null(zoom)) {
                        svalue(NBARS) <- zoom[2]
                        svalue(START, index = TRUE) <- zoom[1]
                    }

                    ii <- ii + 1
                    resetbtn <- gbutton("Reset")
                    tbl[ii, 5:6] <- resetbtn
                    addHandlerClicked(resetbtn, function(h, ...) {
                        blockHandlers(START)
                        svalue(START, index = TRUE) <- 1
                        unblockHandlers(START)
                        blockHandlers(NBARS)
                        svalue(NBARS) <- min(30, length(levels(curSet$x)))
                        unblockHandlers(NBARS)

                        GUI$getActiveDoc()$setSettings(
                           list(zoombars = NULL)
                           )
                        updateSettings()
                    })
                    ii <- ii + 1
                }
            } else if (grepl("^gg_", PLOTTYPE)) {
              tbl[ii, 1:2, anchor = c(-1,-1), expand = TRUE] <- sectionTitle("Caption")
              ii <- ii + 1
              tbl[ii, 1:2, expand = TRUE, fill = TRUE, anchor = c(1, 0)] <- glabel("Caption/Source:")
              captionText <- gedit(
                text = if (!is.null(curSet$caption)) curSet$caption else "",
                handler = function(h, ...) updateEverything()
              )
              tbl[ii, 3:6, expand = TRUE] <- captionText
              ii <- ii + 1
            } else {
                ## Axis Limits
                tbl[ii,  1:2, anchor = c(-1,-1), expand = TRUE] <- sectionTitle("Axis Limits")
                ii <- ii + 1

                if (PLOTTYPE %in% c("scatter", "hex", "grid")) {
                    isNA <- is.na(curSet$x) | is.na(curSet$y)
                    xrange <- range(curSet$y[!isNA])
                    yrange <- range(curSet$x[!isNA])
                } else {
                    isNA <- is.na(curSet$x)
                    xrange <- range(curSet$x[!isNA])
                }

                xlim <- curSet$xlim
                if (is.null(xlim)) xlim <- signif(xrange, 5)

                if (PLOTTYPE %in% c("scatter", "hex", "grid")) {
                    ylim <- curSet$ylim
                    if (is.null(ylim)) ylim <- signif(yrange, 5)
                }

                lbl <- glabel("x axis :")
                xlower <- gedit(xlim[1], width = 8)
                xupper <- gedit(xlim[2], width = 8)
                tbl[ii, 1:2, expand = TRUE, anchor = c(1, 0)] <- lbl
                tbl[ii, 3:4, expand = TRUE] <- xlower
                tbl[ii, 5:6, expand = TRUE] <- xupper
                ii <- ii + 1

                if (PLOTTYPE %in% c("scatter", "hex", "grid")) {
                    lbl <- glabel("y axis :")
                    ylower <- gedit(ylim[1], width = 8)
                    yupper <- gedit(ylim[2], width = 8)
                    tbl[ii, 1:2, expand = TRUE, anchor = c(1, 0)] <- lbl
                    tbl[ii, 3:4, expand = TRUE] <- ylower
                    tbl[ii, 5:6, expand = TRUE] <- yupper
                    ii <- ii + 1
                }

                errlbl <- glabel("Limits must be numbers.")
                tbl[ii, 3:6, expand = TRUE, anchor = c(-1, 0)] <- errlbl
                visible(errlbl) <- FALSE
                ii <- ii + 1

                ## Transform axes (log)
                tbl[ii,  1:2, anchor = c(-1,-1), expand = TRUE] <-
                    sectionTitle("Axis Transformation")
                ii <- ii + 1

                lbl <- glabel("Log (base 10) :")
                ctrans <- curSet$transform
                cvn <- curSet$varnames$x
                if (PLOTTYPE %in% c("scatter", "hex", "grid"))
                    cvn <- c(cvn, curSet$varnames$y)
                if (any(sapply(cvn, nchar) > 15)) {
                    xLog <- gcheckbox("x-variable",
                        checked = !is.null(ctrans$x) && ctrans$x == "log10"
                    )
                    if (PLOTTYPE %in% c("scatter", "hex", "grid"))
                        yLog <- gcheckbox("y-variable",
                            checked = !is.null(ctrans$y) && ctrans$x == "log10"
                        )
                } else {
                    XY <- PLOTTYPE %in% c("scatter", "hex", "grid")
                    xLog <- gcheckbox(curSet$varnames[[ifelse(XY, "y", "x")]],
                        checked = !is.null(ctrans$x) && ctrans$x == "log10"
                    )
                    if (XY)
                        yLog <- gcheckbox(curSet$varnames$x,
                            checked = !is.null(ctrans$y) && ctrans$x == "log10"
                        )
                }
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:4, anchor = c(-1, 0), expand = TRUE] <- xLog
                if (PLOTTYPE %in% c("scatter", "hex", "grid"))
                    tbl[ii, 5:6, anchor = c(-1, 0), expand = TRUE] <- yLog
                ii <- ii + 1

            }

            updateEverything <<- function(update = auto) {
                ## To easily diable automatic updating of plot, add this argument,
                ## otherwise would have to block/unblock handlers
                if (!update)
                    return()

                ## Things that don't need checking:
                newSet <- list(
                    main = if (svalue(labMain) == "") NULL else svalue(labMain),
                    xlab = if (svalue(labXlab) == "") NULL else svalue(labXlab),
                    transform = list()
                )

                if (YAX) newSet$ylab <- if (svalue(labYlab) == "") NULL else svalue(labYlab)
                if (YAXlbl) newSet$internal.labels <- svalue(intLabs)

                if (PLOTTYPE == "scatter") {
                    newSet$jitter <- paste0(ifelse(svalue(xJit), "x", ""),
                                            ifelse(svalue(yJit), "y", ""))
                    newSet$rugs <- paste0(ifelse(svalue(xRug), "x", ""),
                                            ifelse(svalue(yRug), "y", ""))
                }

                if (PLOTTYPE == "bar") {
                    newSet$bar.counts <- svalue(ycounts, index = TRUE) == 2
                    if (length(levels(curSet$x)) > 2) {
                        newSet$zoombars <-
                            if (svalue(NBARS) == length(levels(curSet$x)) & svalue(START, index = TRUE) == 1)
                                NULL
                            else
                                c(svalue(START, index = TRUE), svalue(NBARS))
                    }
                } else if (grepl("^gg_", PLOTTYPE)) {
                  if (!is.null(svalue(captionText)) && svalue(captionText) != "") {
                    newSet$caption <- svalue(captionText)
                  } else {
                    newSet$caption <- ""
                  }
                } else {
                    err <- FALSE
                    xl <- suppressWarnings(as.numeric(svalue(xlower)))
                    if (is.na(xl)) {
                        xl <- if (svalue(xlower) == "") xrange[1] else xlim[1]
                        if (svalue(xlower) != "") err <- TRUE
                    }
                    xu <- suppressWarnings(as.numeric(svalue(xupper)))
                    if (is.na(xu)) {
                        xu <- if (svalue(xupper) == "") xrange[2] else xlim[2]
                        if (svalue(xupper) != "") err <- TRUE
                    }
                    if (xl == xu) {
                        xl <- xrange[1]
                        xu <- xrange[2]
                    }

                    # need to explicitely add NULL to the list
                    newSet$transform["x"] <- list(
                        if (svalue(xLog)) "log10" else NULL
                    )

                    if (PLOTTYPE %in% c("scatter", "hex", "grid")) {
                        yl <- suppressWarnings(as.numeric(svalue(ylower)))
                        if (is.na(yl)) {
                            yl <- if (svalue(ylower) == "") yrange[1] else ylim[1]
                            if (svalue(ylower) != "") err <- TRUE
                        }
                        yu <- suppressWarnings(as.numeric(svalue(yupper)))
                        if (is.na(yu)) {
                            yu <- if (svalue(yupper) == "") yrange[2] else ylim[2]
                            if (svalue(yupper) != "") err <- TRUE
                        }
                        if (yl == yu) {
                            yl <- yrange[1]
                            yu <- yrange[2]
                        }

                        newSet$transform["y"] <- list(
                            if (svalue(yLog)) "log10" else NULL
                        )
                    }

                    visible(errlbl) <- err
                    newSet$xlim <- c(xl, xu)

                    if (PLOTTYPE %in% c("scatter", "hex", "grid"))
                        newSet$ylim <- c(yl, yu)

                    # newSet$xlim <- NULL
                    # newSet$ylim <- NULL
                }

                GUI$getActiveDoc()$setSettings(newSet)
                updateSettings()
            }

            updT <- function(h, ...) {
                if (!is.null(timer))
                    if (timer$started) timer$stop_timer()
                timer <<- gtimer(800, function(...) updateEverything(), one.shot = TRUE)
            }

            addHandlerKeystroke(labMain, updT)
            addHandlerKeystroke(labXlab, updT)
            if (YAX) {
              addHandlerKeystroke(labYlab, updT)
            }
            if (YAXlbl) addHandlerChanged(intLabs, function(h, ...) updateEverything())

            if (PLOTTYPE == "scatter") {
                addHandlerChanged(xJit, function(h, ...) updateEverything())
                addHandlerChanged(yJit, function(h, ...) updateEverything())
                addHandlerChanged(xRug, function(h, ...) updateEverything())
                addHandlerChanged(yRug, function(h, ...) updateEverything())
            }

            if (grepl("^gg_", PLOTTYPE)) {
              addHandlerChanged(captionText, function(h, ...) updateEverything())
            }



            if (PLOTTYPE == "bar") {
                addHandlerChanged(ycounts, function(h, ...) updateEverything())
                if (length(levels(curSet$x)) > 2) {
                    addHandlerChanged(NBARS, function(h, ...) updateEverything())
                    addHandlerChanged(START, function(h, ...) updateEverything())
                }
            } else if (grepl("^gg_", PLOTTYPE)) {

            } else {
                addHandlerKeystroke(xlower, updT)
                addHandlerKeystroke(xupper, updT)
                addHandlerChanged(xLog, function(h, ...) {
                    # log/exp axis limits
                    blockHandlers(xlower)
                    blockHandlers(xupper)
                    svalue(xlower) <-
                        if (svalue(xLog)) signif(log10(as.numeric(svalue(xlower))), 5)
                        else signif(10^(as.numeric(svalue(xlower))), 5)
                    svalue(xupper) <-
                        if (svalue(xLog)) signif(log10(as.numeric(svalue(xupper))), 5)
                        else signif(10^(as.numeric(svalue(xupper))), 5)
                    unblockHandlers(xlower)
                    unblockHandlers(xupper)
                    updateEverything()
                })
                if (PLOTTYPE %in% c("scatter", "hex", "grid")) {
                    addHandlerKeystroke(ylower, updT)
                    addHandlerKeystroke(yupper, updT)
                    addHandlerChanged(yLog, function(h, ...) {
                        # log/exp axis limits
                        blockHandlers(ylower)
                        blockHandlers(yupper)
                        svalue(ylower) <-
                            if (svalue(yLog)) signif(log10(as.numeric(svalue(ylower))), 5)
                            else signif(10^(as.numeric(svalue(ylower))), 5)
                        svalue(yupper) <-
                            if (svalue(yLog)) signif(log10(as.numeric(svalue(yupper))), 5)
                            else signif(10^(as.numeric(svalue(yupper))), 5)
                        unblockHandlers(ylower)
                        unblockHandlers(yupper)
                        updateEverything()
                    })
                }
            }

            add(optGrp, tbl)
        },
        identify = function() {
            iNZLocatePoints()
        })
    )
