## Class to display the window that enables the user to remove
## modifications that were previously made to the plot

iNZPlotRmveModWin <- setRefClass(
    "iNZPlotRmveModWin",
    fields = list(
        GUI = "ANY",
        curSet = "ANY", ## the current plot settings
        defSet = "ANY" ## the default plot settings
        ),
    methods = list(
        initialize = function(gui=NULL) {
            initFields(GUI = gui)
            if(!is.null(GUI)) {
                ## close a modification window if one is open
                try(dispose(GUI$modWin), silent = TRUE)
                curSet <<- GUI$getActiveDoc()$getSettings()
                defSet <<- iNZightPlots:::inzpar()
                ## labels for all possible additions
                additions <- c(
                    "Remove all additions",
                    paste("Remove colour coding by", curSet$varnames$by),
                    paste("Remove resizing by",curSet$varnames$sizeby),
                    "Remove trend curves",
                    "Remove y = x line",
                    "Remove smoothers",
                    "Remove jitter",
                    "Remove rugs",
                    "Remove connecting lines",
                    paste("Remove colour by", curSet$varnames$by),
                    "Remove all inference information",  # "confidence intervals",
                    "Remove symbol interior colouring",
                    "Restore default symbol colours",
                    "Restore default plotting symbol sizes",
                    "Restore default symbol transparency",
                    "Restore default background colour",
                    "Restore default line thickness",
                    "Restore default plot type",
                    "Restore default plot labels",
                    "Restore default bar colours",
                    "Restore default number of bins",
                    "Restore default number of grid bins",
                    "Restore default number of hexs",
                    "Restore default plot type")
                ## check for presence of all additions
                curAdditions <- c(
                    TRUE, ## all additiions
                    !is.null(curSet$colby) && ## colour coding dotplots
                    (is.numeric(curSet$x) ||
                     is.numeric(curSet$y)),
                    !is.null(curSet$sizeby), ## resize
                    !is.null(curSet$trend), ## trend
                    curSet$LOE, ## x=y line
                    curSet$smooth != 0 || !is.null(curSet$quant.smooth), ## smoother
                    curSet$jitter != "", ## jitter
                    curSet$rugs != "", ## rugs
                    curSet$join, ## connecting lines
                    !is.null(curSet$colby) && ## colour coding barchart
                    !is.numeric(curSet$x) &&
                    is.null(curSet$y),
                    !is.null(curSet$inference.type) ||
                    curSet$bs.inference, ## confidence intervals
                    curSet$fill.pt != defSet$fill.pt | curSet$pch != defSet$pch, ## point filling
                    curSet$col.pt != defSet$col.pt, ## point colour
                    (all.equal(curSet$cex.pt, defSet$cex.pt) != TRUE), ## point size
                    (all.equal(curSet$alpha, defSet$alpha) != TRUE), ## transparency
                    curSet$bg != defSet$bg, ## bg colour
                    curSet$lwd.pt != defSet$lwd.pt, ## point line thickness
                    !is.null(curSet$largesample), ## plot type
                    !is.null(curSet$xlab) | !is.null(curSet$ylab) | !is.null(curSet$main),  ## plot labels
                    curSet$bar.fill != defSet$bar.fill,  ## bar colours
                    !is.null(curSet$hist.bins),  ## number of histogram bins
                    curSet$scatter.grid.bins != defSet$scatter.grid.bins,  ## number of grid bins
                    curSet$hex.bins != defSet$hex.bins,  ## number of hexs
                    curSet$plottype != defSet$plottype   ## plot type
                    )

                proceedButton <- gbutton(
                    "Proceed",
                    handler = function(h, ...) {
                        ## the checkboxes are accessed as
                        ## children of the selectGrp
                        ## first child refers to remove all adds
                        if (svalue(selectGrp$children[[1]]))
                            rmv <- TRUE
                        else
                            rmv <- which(curAdditions)[sapply(
                                selectGrp$children,
                                svalue)] - 1
                        ## update the plot settings
                        removeAdditions(rmv)
                        dispose(GUI$modWin)
                    })
                GUI$modWin <<- gwindow(title = "Remove additions",
                                       visible = TRUE,
                                       parent = GUI$win)
                mainGrp <- ggroup(horizontal = FALSE,
                                  container = GUI$modWin,
                                  expand = FALSE)
                selectGrp <- ggroup(horizontal = FALSE,
                                  container = mainGrp,
                                  expand = FALSE)
                btnGrp <- ggroup(container = mainGrp)
                mainGrp$set_borderwidth(15)
                sapply(additions[curAdditions], function(x) {
                    gcheckbox(x, cont = selectGrp)})
                addSpring(btnGrp)
                add(btnGrp, proceedButton)
                ## add observer to the data
                ## if it changes, remove all current additions
                GUI$getActiveDoc()$addDataObserver(
                    function() removeAdditions(TRUE)
                    )
            }
        },
        ## remove plot additions from the plot settings
        ## additions: logical vector representing which
        ##            addition to remove
        removeAdditions = function(additions) {
            ## list with entries to remove additions (set to default)
            rmvAdditions <- list(list(colby = NULL, ## colour coding dotplots
                                      varnames = list(colby = NULL)),
                                 list(sizeby = NULL, ## resize proportional
                                      varnames = list(sizeby = NULL)),
                                 list(trend = defSet$trend), ## trend
                                 list(LOE = defSet$LOE), ## x=y line
                                 list(smooth = defSet$smooth, quant.smooth = defSet$quant.smooth), ## smoother
                                 list(jitter = defSet$jitter), ## jitter
                                 list(rugs = ""), ## rugs
                                 list(join = defSet$join), ## connecting lines
                                 list(colby = NULL, ## colour coding barchart
                                      varnames = list(colby = NULL)),
                                 list(inference.type = defSet$inference.type,
                                      inference.par = defSet$inference.par,
                                      bs.inference = defSet$bs.inference), ## confidence intervals
                                 list(pch = defSet$pch, fill.pt = defSet$fill.pt), ## point filling
                                 list(col.pt = defSet$col.pt), ## point colour
                                 list(cex.pt = defSet$cex.pt), ## point size
                                 list(alpha = defSet$alpha), ## transparency
                                 list(bg = defSet$bg), ## bg colour
                                 list(lwd.pt = defSet$lwd.pt), ## point line thickness
                                 list(largesample = defSet$largesample), ## plot type
                                 list(xlab = NULL, ylab = NULL, main = NULL),  ## labels
                                 list(bar.fill = defSet$bar.fill), ## bar colours
                                 list(hist.bins = NULL),  ## histogram bins
                                 list(scatter.grid.bins = defSet$scatter.grid.bins),  ## grid bins
                                 list(hex.bins = defSet$hex.bins),  ## n. hexs
                                 list(plottype = defSet$plottype)  ## plot type
                                 )

            GUI$getActiveDoc()$setSettings(
                unlist(rmvAdditions[additions], recursive = FALSE)
                )
        })
    )
