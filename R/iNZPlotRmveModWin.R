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
        initialize = function(gui = NULL, new = TRUE) {
            initFields(GUI = gui)
            if(!is.null(GUI)) {
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
                    "Remove symbol coding by", curSet$varnames$symbolby,
                    "Restore default symbol transparency",
                    "Restore default background colour",
                    "Restore default line thickness",
                    "Restore default plot type",
                    "Restore default plot labels",
                    "Restore default bar colours",
                    "Restore default number of bins",
                    "Restore default number of grid bins",
                    "Restore default number of hexs",
                    "Restore default plot type",
                    "Remove point labels",
                    "Restore default axis limits",
                    "Restore all bars of bar chart",
                    "Restore default size scale")
                ## check for presence of all additions
                curAdditions <- c(
                    TRUE, ## all additiions
                    !is.null(curSet$colby) && ## colour coding dotplots
                    (is_num(curSet$x) ||
                     is_num(curSet$y)),
                    !is.null(curSet$sizeby), ## resize
                    !is.null(curSet$trend), ## trend
                    curSet$LOE, ## x=y line
                    curSet$smooth != 0 || !is.null(curSet$quant.smooth), ## smoother
                    curSet$jitter != "", ## jitter
                    curSet$rugs != "", ## rugs
                    curSet$join, ## connecting lines
                    !is.null(curSet$colby) && ## colour coding barchart
                    !is_num(curSet$x) &&
                    is.null(curSet$y),
                    !is.null(curSet$inference.type) ||
                    curSet$bs.inference, ## confidence intervals
                    curSet$fill.pt != defSet$fill.pt,  ## point filling
                    curSet$col.pt != defSet$col.pt, ## point colour
                    (all.equal(curSet$cex.pt, defSet$cex.pt) != TRUE), ## point size
                    !is.null(curSet$symbolby),  ## symbol by
                    (all.equal(curSet$alpha, defSet$alpha) != TRUE), ## transparency
                    curSet$bg != defSet$bg, ## bg colour
                    curSet$lwd.pt != defSet$lwd.pt, ## point line thickness
                    !is.null(curSet$largesample), ## plot type
                    !is.null(curSet$xlab) | !is.null(curSet$ylab) | !is.null(curSet$main),  ## plot labels
                    curSet$bar.fill != defSet$bar.fill,  ## bar colours
                    !is.null(curSet$hist.bins),  ## number of histogram bins
                    curSet$scatter.grid.bins != defSet$scatter.grid.bins,  ## number of grid bins
                    curSet$hex.bins != defSet$hex.bins,  ## number of hexs
                    curSet$plottype != defSet$plottype && curSet$plottype != "default",  ## plot type
                    (!is.null(curSet$locate) | !is.null(curSet$locate.id) |
                     !is.null(curSet$locate.col)),  ## locate labels
                    (!is.null(curSet$xlim) | !is.null(curSet$ylim)),  ## axis limits
                    (!is.null(curSet$zoombars)),  ## bar plot zooming
                    curSet$cex != defSet$cex  ## cex overall
                    )

                proceedButton <- gbutton(
                    "OK",
                    handler = function(h, ...) {
                        ## the checkboxes are accessed as
                        ## children of the selectGrp
                        ## first child refers to remove all adds
                        try({
                            if (svalue(selectGrp$children[[1]]))
                                rmv <- TRUE
                            else
                                rmv <- which(curAdditions)[sapply(
                                    selectGrp$children,
                                    svalue)] - 1
                            ## update the plot settings
                            removeAdditions(rmv)
                        }, silent = TRUE)

                        ## only destroy the window if there are no more additions left to remove
                        iNZPlotRmveModWin$new(GUI, new = FALSE)
                    })

                closeButton <- gbutton(
                    "Home",
                    handler = function(h, ...) {
                        delete(GUI$leftMain, GUI$leftMain$children[[2]])
                        visible(GUI$gp1) <<- TRUE
                    })

                if (sum(curAdditions) <= 1) {
                    if (new) {
                        ## User has just clicked the "Remove additions" button
                        gmessage("There are no plot additions to remove.",
                                 title = "Nothing to remove",
                                 parent = GUI$win)
                    } else {
                        ## User has just removed a bunch of additions, and there are none left
                        delete(GUI$leftMain, GUI$leftMain$children[[2]])
                        visible(GUI$gp1) <<- TRUE
                    }
                    return()
                }
                
                ## open in leftMain
                modwin <- GUI$initializeModuleWindow(title = "Remove additions", scroll = TRUE)
                
                mainGrp <- modwin$body
                
                selectGrp <- ggroup(horizontal = FALSE,
                                    container = mainGrp,
                                    expand = FALSE)
                
                
                btnGrp <- modwin$footer
                mainGrp$set_borderwidth(5)

                add(btnGrp, proceedButton, expand = TRUE, fill = TRUE)
                add(btnGrp, closeButton, expand = TRUE, fill = TRUE)

                ## Checking function:
                checkOK <- function() {
                    checked <- sapply(selectGrp$children, svalue)
                    enabled(proceedButton) <- any(checked)
                }
                checkOK()

                sapply(additions[curAdditions], function(x) {
                       gcheckbox(x, cont = selectGrp, handler = function(h, ...) checkOK())})
                
                
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
                                 list(symbolby = NULL,
                                      varnames = list(symbolby = NULL)), ## symbol by
                                 list(alpha = defSet$alpha), ## transparency
                                 list(bg = defSet$bg), ## bg colour
                                 list(lwd.pt = defSet$lwd.pt), ## point line thickness
                                 list(largesample = defSet$largesample), ## plot type
                                 list(xlab = NULL, ylab = NULL, main = NULL),  ## labels
                                 list(bar.fill = defSet$bar.fill), ## bar colours
                                 list(hist.bins = NULL),  ## histogram bins
                                 list(scatter.grid.bins = defSet$scatter.grid.bins),  ## grid bins
                                 list(hex.bins = defSet$hex.bins),  ## n. hexs
                                 list(plottype = defSet$plottype),  ## plot type
                                 list(locate = NULL, locate.id = NULL, locate.col = NULL,
                                      locate.settings = NULL),
                                 list(xlim = NULL, ylim = NULL),  ## axis limits
                                 list(zoombars = NULL),  ## bar plot zooming
                                 list(cex = defSet$cex)  ## overall cex
                                 )

            GUI$getActiveDoc()$setSettings(
                unlist(rmvAdditions[additions], recursive = FALSE)
                )
        })
    )
