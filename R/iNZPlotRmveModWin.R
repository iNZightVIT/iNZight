## Class to display the window that enables the user to remove
## modifications that were previously made to the plot

iNZPlotRmveModWin <- setRefClass(
    "iNZPlotRmveModWin",
    fields = list(
        GUI = "ANY",
        modWin = "ANY",
        curSet = "ANY", ## the current plot settings
        defSet = "ANY", ## the default plot settings
        add_cur = "list",
        add_other = "list",
        g_cur = "ANY",
        g_other = "ANY",
        rmvBtn = "ANY"
    ),
    methods = list(
        initialize = function(gui = NULL, new = TRUE) {
            initFields(GUI = gui)

            curSet <<- GUI$getActiveDoc()$getSettings()
            defSet <<- iNZightPlots:::inzpar()
            ptypes <- iNZightPlots:::plot_types

            additions <- lapply(names(plot_modifications),
                function(name) {
                    x <- plot_modifications[[name]]
                    list(
                        text = if (is.character(x$text)) x$text else x$text(curSet),
                        remove =
                            if (is.null(x$remove)) {
                                if (!is.null(x$default) && is.character(x$default)) {
                                    c <- curSet[x$default]
                                    d <- defSet[x$default]
                                } else {
                                    c <- curSet[name]
                                    d <- defSet[name]
                                }

                                if (all(sapply(c, is.null)) || identical(c, d)) 0L
                                else if (is.null(x$plot_types) && !name %in% rownames(ptypes)) 2L
                                else if (is.null(x$plot_types))
                                    grepl("p", ptypes[name, GUI$plotType]) + 1L
                                else (GUI$plotType %in% x$plot_types) + 1L
                            } else x$remove(curSet, GUI$curPlot),
                        default =
                            if (is.null(x$default))
                                structure(list(defSet[[name]]), .Names = name)
                            else if (is.character(x$default))
                                structure(defSet[x$default], .Names = x$default)
                            else x$default
                    )
                }
            )
            add_cur <<- additions[sapply(additions, function(x) x$remove == 2L)]
            add_other <<- additions[sapply(additions, function(x) x$remove == 1L)]

            modWin <<- GUI$initializeModuleWindow(title = "Remove additions", scroll = TRUE)

            # main group with all the check boxes
            mainGrp <- modWin$body
            g_cur <<- gvbox()
            g_other <<- gvbox()

            no_add <- TRUE
            if (length(add_cur)) {
                no_add <- FALSE
                lbl <- glabel("Additions for the current plot")
                font(lbl) <- list(weight = "bold")
                add(mainGrp, lbl, anchor = c(-1, 0))
                add(mainGrp, g_cur)

                sapply(add_cur,
                    function(x) {
                        gcheckbox(x$text,
                            container = g_cur,
                            handler = .self$check_box
                        )
                    }
                )

                addSpace(mainGrp, 10)
            }

            if (length(add_other)) {
                no_add <- FALSE
                lbl <- glabel("Other additions currently ignored")
                font(lbl) <- list(weight = "bold")
                add(mainGrp, lbl, anchor = c(-1, 0))
                add(mainGrp, g_other)

                sapply(add_other,
                    function(x) {
                        gcheckbox(x$text,
                            container = g_other,
                            handler = .self$check_box
                        )
                    }
                )
            }

            if (no_add) {
                lbl <- glabel("No additions to remove")
                font(lbl) <- list(weight = "bold")
                add(mainGrp, lbl, anchor = c(-1, 0))
            }

            # footer group
            btnGrp <- modWin$footer
            mainGrp$set_borderwidth(5)

            # if any checkboxes ticked, rename to "Remove selected"
            rmvBtn <<- gbutton("Remove all",
                container = btnGrp,
                expand = TRUE,
                fill = TRUE,
                handler = function(h, ...) {
                    remove_additions()
                }
            )
            rmvBtn$set_icon("delete")
            enabled(rmvBtn) <<- !no_add

            closeButton <- gbutton("Home",
                container = btnGrp,
                expand = TRUE,
                fill = TRUE,
                handler = function(h, ...) {
                    delete(GUI$leftMain, GUI$leftMain$children[[2]])
                    visible(GUI$gp1) <<- TRUE
                }
            )

            return()

            if(!is.null(GUI)) {
                curSet <<- GUI$getActiveDoc()$getSettings()
                defSet <<- iNZightPlots:::inzpar()
                ## labels for all possible additions
                additions <- c(
                    # "Remove all additions",
                    # paste("Remove colour coding by", curSet$varnames$by),
                    # paste("Remove resizing by",curSet$varnames$sizeby),
                    # "Remove trend curves",
                    # "Remove y = x line",
                    # "Remove smoothers",
                    # "Remove jitter",
                    # "Remove rugs",
                    # "Remove connecting lines",
                    # paste("Remove colour by", curSet$varnames$by),
                    # "Remove all inference information",  # "confidence intervals",
                    "Remove symbol interior colouring",
                    # "Restore default symbol colours",
                    # "Restore default plotting symbol sizes",
                    # "Remove symbol coding by", curSet$varnames$symbolby,
                    # "Restore default symbol transparency",
                    # "Restore default background colour",
                    "Restore default line thickness",
                    "Restore default plot type",
                    "Restore default plot labels",
                    "Restore default bar colours",
                    "Restore default number of bins",
                    "Restore default number of grid bins",
                    "Restore default number of hexs",
                    "Restore default plot type",
                    # "Remove point labels",
                    # "Restore default axis limits",
                    # "Restore all bars of bar chart",
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
        check_box = function(h, ...) {
            any_checked <- any(sapply(c(g_cur$children, g_other$children), svalue))

            blockHandler(rmvBtn)
            rmvBtn$set_value(ifelse(any_checked, "Remove selected", "Remove all"))
            unblockHandler(rmvBtn)
        },
        remove_additions = function(confirm = FALSE) {
            # get selected
            checked <- which(sapply(c(g_cur$children, g_other$children), svalue))

            newSet <- list()
            rmvSet <- c(add_cur, add_other)
            if (length(checked)) {

                for (i in checked)
                    newSet <- modifyList(newSet, rmvSet[[i]]$default, keep.null = TRUE)
            } else {
                if (!confirm && !gconfirm("Are you sure you want to reset all plot modifications?")) return()
                for (i in seq_along(rmvSet))
                    newSet <- modifyList(newSet, rmvSet[[i]]$default, keep.null = TRUE)
            }
            GUI$getActiveDoc()$setSettings(newSet)
            iNZPlotRmveModWin$new(GUI, new = FALSE)
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


# each item in the list
# - is named by the associated parameter for plotting
# - has two components:
#   * the text displayed (either static or a function of the settings list)
#   * a function (of the settings list and current plot) which returns
#     - 0 if setting is not set,
#     - 1 if setting is set, but unused in the current plot,
#     - 2 if setting is set and used in current plot
#   * a default value (if NULL, replaced by default settings list)

plot_modifications <- list(
    colby = list(
        text = function(settings)
            sprintf("Remove colour coding by %s", as.character(settings$colby)),
        default = list(colby = NULL, varnames = list(colby = NULL))
    ),
    sizeby = list(
        text = function(settings)
            sprintf("Remove resizing by %s", as.character(settings$sizeby)),
        default = list(sizeby = NULL, varnames = list(sizeby = NULL))
    ),
    symbolby = list(
        text = function(settings)
            sprintf("Remove symbol coding by %s", as.character(settings$symbolby)),
        default = list(symbolby = NULL, varnames = list(symbolby = NULL))
    ),
    locate = list(
        text = "Remove point labels",
        default = c("locate", "locate.id", "locate.col", "locate.extreme",
            "locate.same.level", "highlight")
    ),
    xlim = list(text = "Reset x-axis limits"),
    ylim = list(text = "Reset y-axis limits"),
    zoombars = list(text = "Restore all bars of barchart"),
    pch = list(text = "Restore default plot symbol"),
    col.pt = list(text = "Restore default point colour"),
    col.fun = list(text = "Restore default colour palette"),
    col.emph = list(
        text = "Remove emphasised colour",
        default = c("col.emph", "col.emphn", "emph.on.top")
    ),
    reverse.palette = list(text = "Remove colour palette reveral"),
    col.method = list(text = "Restore default colour-by method"),
    cex = list(text = "Reset default scale"),
    cex.pt = list(text = "Reset default point size"),
    cex.dotpt = list(text = "Reset default dot size"),
    resize.method = list(text = "Restore default point sizing method"),
    alpha = list(text = "Remove point transparency"),
    bg = list(text = "Restore default background colour"),
    fill.pt = list(text = "Remove point fill"),
    lwd = list(text = "Restore default line width multiplier"),
    lwd.pt = list(text = "Restore default point border width"),
    col.line = list(text = "Restore default colour for connecting lines"),
    jitter = list(text = "Remove jitter"),
    rugs = list(text = "Remove rugs"),
    trend = list(
        text = "Remove trend curves",
        default = c("trend", "trend.by", "trend.parallel")
    ),
    smooth = list(
        text = "Remove smoothers",
        default = c("smooth", "quant.smooth")
    ),
    LOE = list(text = "Remove y = x line"),
    join = list(
        text = "Remove connecting lines",
        default = c("join")
    ),
    col.trend = list(text = "Restore default trend line colours"),
    lty.trend = list(text = "Restore default trend line patterns"),
    col.smooth = list(text = "Restore default smoother colour"),
    mean_indicator = list(text = "Remove mean indicator"),
    boxplot = list(text = "Restore boxplot"),
    bar.fill = list(text = "Restore default bar colour"),
    bar.counts = list(text = "Restore percentages for barchart y-axis"),
    inference.type = list(
        text = "Remove plot inference",
        default = c("inference.type", "inference.par", "bs.inference")
    ),
    scatter.grid.bins = list(text = "Restore default number of grid bins"),
    hex.bins = list(text = "Restore default number of hexagonal bins"),
    hex.style = list(text = "Restore default hexagon style"),
    hist.bins = list(text = "Restore default number of histogram bins"),
    internal.labels = list(text = "Restore internal group labels"),
    plottype = list(text = "Restore default plot type")
)
