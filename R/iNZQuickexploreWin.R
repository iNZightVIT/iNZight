## --------------------------------------------
## The super class for the Quick explore window
## The different windows that are opened through the
## 'Quick Explore' sub-menu are subclasses of this superclass
## --------------------------------------------

iNZQuickexploreWin <- setRefClass(
    "iNZQuickexploreWin",
    fields = list(
        GUI = "ANY"
        ),
    methods = list(
        initialize = function(gui=NULL) {
            initFields(GUI = gui)
            if (!is.null(GUI)) {
                GUI$modWin <<- gwindow(visible = FALSE,
                                   parent = GUI$win)
                size(GUI$modWin) <<- c(700, 400)
            }
        })
    )

iNZExploreMissing <- setRefClass(
    "iNZExploreMissing",
    contains = "iNZQuickexploreWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Explore Missing Values"
            oldWd <- options(width = 1000)  # so it doesn't wrap
            dd <- GUI$getActiveData()
            g <- gtext(text = paste(iNZightMR::calcmissing.data.frame(dd,
                           print = FALSE,
                           final = FALSE),
                           collapse = "\n"),
                       expand = TRUE, cont = GUI$modWin, wrap = FALSE,
                       font.attr = list(family = "monospace"))
            visible(GUI$modWin) <<- TRUE
            dev.new()
            iNZightMR::plotcombn(dd)
            options(width = oldWd$width)
        })
    )

iNZallSummaries <- setRefClass(
    "iNZallSummaries",
    contains = "iNZQuickexploreWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Explore all 1-way Summaries"
            oldWd <- options(width = 1000)  # so it doesn't wrap
            g <- gtext(text = iNZightPlots::exploreAllSummaries(GUI$getActiveData()),
                       expand = TRUE, cont = GUI$modWin, wrap = FALSE,
                       font.attr = list(family = "monospace"))
            visible(GUI$modWin) <<- TRUE
            options(width = oldWd$width)
        })
    )

iNZallPlots <- setRefClass(
    "iNZallPlots",
    contains = "iNZQuickexploreWin",
    methods = list(
        initialize = function(gui) {
            ## Instead, we will make a gui that cycles through them ...
            ign <- gwindow("...", visible = FALSE)
            tag(ign, "dataSet") <- gui$getActiveData()
            e <- list(obj = ign)
            e$win <- gui$win
            allUniPlots(e)
        })
    )

iNZIdentifyPoints <- setRefClass(
    "iNZIdentifyPoints",
    contains = "iNZQuickexploreWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Identify Points"
            size(GUI$modWin) <<- c(200, 200)
            curSet <- GUI$getActiveDoc()$getSettings()
            grp <- ggroup(horizontal = FALSE)
            grp$set_borderwidth(15)

            ## Do checking first
            ## If g1 or g2 = _MULTI, then we can't identify points (yet ...)
            cantDo <- function() {
                gmessage("Cannot identify points when subset by = _MULTI",
                         icon = "error", title = "Unable to identify")
                dispose(GUI$modWin)
                return()
            }
            if (!is.null(curSet$g1)) {
                if (is.null(curSet$g1.level)) {
                    cantDo()
                } else if (curSet$g1.level == 0 | curSet$g1.level == "_MULTI") {
                    cantDo()
                }
            }
            if (!is.null(curSet$g2)) {
                if (is.null(curSet$g2.level)) {
                    cantDo()
                } else if (curSet$g2.level == "_MULTI") {
                    cantDo()
                }
            }

            lbl1 <- "Select variable to identify:"
            font(lbl1) <- list(weight="bold", family = "normal")
            varmenu <- gcombobox(c("id", names(GUI$getActiveData())), selected = 1)

            lbl2 <- "Click \"Locate\" to locate a point"
            locateButton <- gbutton("Locate", handler = function(h, ...) {
                x <- curSet$x  # used for removing missing values ...
                y <- curSet$y
                v <- svalue(varmenu)

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
                
                w <- rep(TRUE, length(v))
                if (!is.null(curSet$g1)) {
                    w[curSet$g1 != curSet$g1.level] <- FALSE
                }
                if (!is.null(curSet$g2)) {
                    if (curSet$g2.level != "_ALL") {
                        w[curSet$g2 != curSet$g2.level] <- FALSE
                    }
                }
               
                seekViewport("MAINVP")

                if (is.null(y)) {
                    ## dotplot
                    dp <- grid.get("DOTPOINTS")
                    d <- data.frame(x = as.numeric(dp$x),
                                    y = as.numeric(dp$y),
                                    v = v[w & !is.na(x)])
                    seekViewport("DOTPLOTVP")  # need correct coordinate system
                } else {
                    dp <- grid.get("SCATTERPOINTS")
                    d <- data.frame(x = as.numeric(dp$x),
                                    y = as.numeric(dp$y),
                                    v = v[w & !(is.na(x) | is.na(y))])
                    seekViewport("SCATTERVP")
                }

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

                if (is.null(y)) {
                    grid.text(o$v, o$x, o$y, just = "left", rot = 45,
                              default.units = "native", gp = gpar(cex=0.5))
                } else {
                    grid.text(o$v, o$x, unit(o$y, "native") + unit(2, "char"),
                              default.units = "native", gp = gpar(cex=0.5))
                }
            })

            tbl <- glayout()
            tbl[1, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl1
            tbl[2, 1, expand = TRUE, anchor = c(1, 0)] <- varmenu
            tbl[4, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl2
            tbl[5, 1, expand = TRUE, anchor = c(1, 0)] <- locateButton

            add(grp, tbl)
            add(GUI$modWin, grp)

            visible(GUI$modWin) <<- TRUE
        })
    )

iNZscatterMatrix <- setRefClass(
    "iNZscatterMatrix",
    contains = "iNZQuickexploreWin",
    methods = list(
        initialize = function(gui) {
            ign <- gwindow("...", visible = FALSE)
            tag(ign, "dataSet") <- gui$getActiveData()
            e <- list(obj = ign)
            e$win <- gui$win
            scatterPlotMatrix(e)
        })
    )
