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
                defSet <<- iNZightPlots:::inzPlotDefaults()
                ## labels for all possible additions
                additions <- c(
                    "Remove all additions",
                    paste("Remove colour coding by", curSet$varnames$by),
                    paste("Remove resizing by",curSet$varnames$prop.size),
                    "Remove trend curves",
                    "Remove y = x line",
                    "Remove smoothers",
                    "Remove jitter",
                    "Remove rugs",
                    "Remove connecting lines",
                    "Remove segmentation in bar charts",
                    "Remove all inference information",  # "confidence intervals",
                    "Remove symbol interior colouring",
                    "Restore default symbol colours",
                    "Restore default plotting symbol sizes",
                    "Restore default symbol transparency",
                    "Restore default background colour",
                    "Restore default line thickness",
                    "Restore default plot type",
                    "Restore default plot labels")
                ## check for presence of all additions
                curAdditions <- c(
                    TRUE, ## all additiions
                    !is.null(curSet$by) && ## colour coding dotplots
                    (is.numeric(curSet$x) ||
                     is.numeric(curSet$y)),
                    !is.null(curSet$prop.size), ## resize
                    !is.null(curSet$trend), ## trend
                    curSet$LOE, ## x=y line
                    curSet$smooth != 0, ## smoother
                    curSet$jitter != "", ## jitter
                    curSet$rugs != "", ## rugs
                    curSet$join, ## connecting lines
                    !is.null(curSet$by) && ## colour coding barchart
                    !is.numeric(curSet$x) &&
                    is.null(curSet$y),
                    !is.null(curSet$inference.type) ||
                    curSet$bs.inference, ## confidence intervals
                    curSet$pch != defSet$pch, ## point filling
                    curSet$col.pt != defSet$col.pt, ## point colour
                    (all.equal(curSet$cex.pt, defSet$cex.pt) != TRUE), ## point size
                    (all.equal(curSet$alpha, defSet$alpha) != TRUE), ## transparency
                    curSet$bg != defSet$bg, ## bg colour
                    curSet$lwd.pt != defSet$lwd.pt, ## point line thickness
                    !is.null(curSet$largesample), ## plot type
                    !is.null(curSet$xlab) & !is.null(curSet$ylab) & !is.null(curSet$main)  ## plot labels
                    )

                proceedButton <- gbutton(
                    "-Proceed-",
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
            rmvAdditions <- list(list(by = NULL, ## colour coding dotplots
                                      varnames = list(by = NULL)),
                                 list(prop.size = NULL, ## resize proportional
                                      varnames = list(prop.size = NULL)),
                                 list(trend = defSet$trend), ## trend
                                 list(LOE = defSet$LOE), ## x=y line
                                 list(smooth = defSet$smooth), ## smoother
                                 list(jitter = defSet$jitter), ## jitter
                                 list(rugs = ""), ## rugs
                                 list(join = defSet$join), ## connecting lines
                                 list(by = NULL, ## colour coding barchart
                                      varnames = list(by = NULL)),
                                 list(inference.type = defSet$inference.type,
                                      inference.par = defSet$inference.par,
                                      bs.inference = defSet$bs.inference), ## confidence intervals
                                 list(pch = defSet$pch), ## point filling
                                 list(col.pt = defSet$col.pt), ## point colour
                                 list(cex.pt = defSet$cex.pt), ## point size
                                 list(alpha = defSet$alpha), ## transparency
                                 list(bg = defSet$bg), ## bg colour
                                 list(lwd.pt = defSet$lwd.pt), ## point line thickness
                                 list(largesample = defSet$largesample), ## plot type
                                 list(xlab = NULL, ylab = NULL, main = NULL)  ## labels
                                 )

            GUI$getActiveDoc()$setSettings(
                unlist(rmvAdditions[additions], recursive = FALSE)
                )
        })
    )
