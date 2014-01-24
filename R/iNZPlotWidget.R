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
        savePlot = function() {
            iNZSaveWin$new(GUI, type = "plot",
                           which = tabDevLink[svalue(plotNb)])
        })
    )
