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

            lbl1 <- "Select variable to identify:"
            font(lbl1) <- list(weight="bold", family = "normal")
            varmenu <- gcombobox(c("id", names(GUI$getActiveData())), selected = 1)

            lbl2 <- "Click \"Locate\" to locate a point"
            locateButton <- gbutton("Locate", handler = function(h, ...) {
                x <- curSet$x
                y <- curSet$y
                v <- svalue(varmenu)

                if (is.null(v))
                    v <- as.character(1:length(x))
                else {
                    if (v == "id")
                        v <- as.character(1:length(x))
                    else
                        v <- as.character(GUI$getActiveData()[, v])
                }

                xy <- as.numeric(grid.locator())

                if (is.null(y)) {
                                        # dotplot
                    seekViewport("DOTPLOTVP")
                    na <- is.na(x) | is.na(v)
                    pd <- iNZightPlots:::makePoints(x[!na], cols = v[!na])
                    d <- data.frame(x = pd$x, y = pd$y, v = pd$cols)
                } else {
                    ## The x and y are swapped because of scatter plot
                    d <- data.frame(x = y, y = x, v = v)
                }

                na <- apply(d, 1, function(x) any(is.na(x)))
                d <- d[!na, ]

                ## So now, d = data.frame with x, y, and the label
                ## Standardise it:
                x.s <- (d$x - min(d$x)) / (max(d$x) - min(d$x))
                y.s <- (d$y - min(d$y)) / (max(d$y) - min(d$x))

                xy.s <- numeric(2)
                xy.s[1] <- (xy[1] - min(d$x)) / (max(d$x) - min(d$x))
                xy.s[2] <- (xy[2] - min(d$y)) / (max(d$y) - min(d$x))

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
