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
        initialize = function(gui = NULL) {
            initFields(GUI = gui)
            if (is.null(GUI)) {
                return()
            }
            GUI$modWin <<- gwindow(
                visible = FALSE,
                parent = GUI$win
            )
            size(GUI$modWin) <<- c(700, 400)
        }
    )
)

iNZExploreMissing <- setRefClass(
    "iNZExploreMissing",
    contains = "iNZQuickexploreWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Explore Missing Values"
            oldWd <- options(width = 1000) # so it doesn't wrap
            dd <- GUI$getActiveData(lazy = FALSE)
            g <- gtext(
                text = paste(
                    capture.output(
                        print(
                            iNZightMR::calcmissing(dd)
                        )
                    ),
                    collapse = "\n"
                ),
                expand = TRUE,
                cont = GUI$modWin,
                wrap = FALSE,
                font.attr = list(family = "monospace")
            )
            visible(GUI$modWin) <<- TRUE
            dev.new()
            iNZightMR::plotcombn(dd)
            options(width = oldWd$width)
        }
    )
)

iNZallSummaries <- setRefClass(
    "iNZallSummaries",
    contains = "iNZQuickexploreWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Explore all 1-way Summaries"
            oldWd <- options(width = 1000) # so it doesn't wrap
            g <- gtext(
                text = iNZightPlots::exploreAllSummaries(GUI$getActiveData(lazy = FALSE)),
                expand = TRUE,
                cont = GUI$modWin,
                wrap = FALSE,
                font.attr = list(family = "monospace")
            )
            visible(GUI$modWin) <<- TRUE
            options(width = oldWd$width)
        }
    )
)

iNZallPlots <- setRefClass(
    "iNZallPlots",
    contains = "iNZQuickexploreWin",
    methods = list(
        initialize = function(gui) {
            ## Instead, we will make a gui that cycles through them ...
            ign <- gwindow("...", visible = FALSE)
            tag(ign, "dataSet") <- gui$getActiveData(lazy = FALSE)
            e <- list(obj = ign)
            e$win <- gui$win
            iNZightModules::allUniPlots(e)
        }
    )
)

iNZall2Plots <- setRefClass(
    "iNZall2Plots",
    contains = "iNZQuickexploreWin",
    methods = list(
        initialize = function(gui) {
            ## Instead, we will make a gui that cycles through them ...
            ign <- gwindow("...", visible = FALSE)
            tag(ign, "dataSet") <- gui$getActiveData(lazy = FALSE)
            e <- list(obj = ign)
            e$win <- gui$win
            iNZightModules::allBivarPlots(e)
        }
    )
)

iNZscatterMatrix <- setRefClass(
    "iNZscatterMatrix",
    contains = "iNZQuickexploreWin",
    methods = list(
        initialize = function(gui) {
            ign <- gwindow("...", visible = FALSE)
            tag(ign, "dataSet") <- gui$getActiveData(lazy = FALSE)
            e <- list(obj = ign)
            e$win <- gui$win
            iNZightModules::scatterPlotMatrix(e)
        }
    )
)
