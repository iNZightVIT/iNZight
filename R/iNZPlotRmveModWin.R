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

            if (GUI$plotType == "custom") {
                gmessage("Remove Additions only works with `inzplot` functions.",
                    title = "Plot type not supported"
                )
                return()
            }

            curSet <<- GUI$getActiveDoc()$getSettings()
            defSet <<- c(iNZightPlots:::inzpar(), iNZightPlots:::gg_defaults)
            ptypes <- iNZightPlots:::plot_types

            additions <- lapply(
                names(plot_modifications),
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

                                if (all(sapply(c, is.null)) || identical(c, d)) {
                                    0L
                                } else if (is.null(x$plot_types) && !name %in% rownames(ptypes)) {
                                    2L
                                } else if (is.null(x$plot_types)) {
                                    grepl("p", ptypes[name, GUI$plotType]) + 1L
                                } else {
                                    (GUI$plotType %in% x$plot_types) + 1L
                                }
                            } else {
                                x$remove(curSet, GUI$curPlot)
                            },
                        default =
                            if (is.null(x$default)) {
                                structure(list(defSet[[name]]), .Names = name)
                            } else if (is.character(x$default)) {
                                structure(defSet[x$default], .Names = x$default)
                            } else {
                                x$default
                            }
                    )
                }
            )
            add_cur <<- additions[sapply(additions, function(x) x$remove == 2L)]
            add_other <<- additions[sapply(additions, function(x) x$remove == 1L)]

            modWin <<- GUI$initializeModuleWindow(
                title = "Actions to remove additions",
                scroll = TRUE
            )

            # main group with all the check boxes
            mainGrp <- modWin$body
            g_cur <<- gvbox()
            g_other <<- gvbox()

            no_add <- TRUE
            if (length(add_cur)) {
                no_add <- FALSE
                lbl <- glabel("Active in current plot")
                font(lbl) <- list(weight = "bold")
                add(mainGrp, lbl, anchor = c(-1, 0))
                add(mainGrp, g_cur)

                sapply(
                    add_cur,
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
                lbl <- glabel("Inactive in current plot")
                font(lbl) <- list(weight = "bold")
                add(mainGrp, lbl, anchor = c(-1, 0))
                add(mainGrp, g_other)

                sapply(
                    add_other,
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
                handler = function(h, ...) GUI$close_module()
            )

            return()
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
                for (i in checked) {
                    newSet <- modifyList(newSet, rmvSet[[i]]$default, keep.null = TRUE)
                }
            } else {
                if (!confirm && !gconfirm("Are you sure you want to reset all plot modifications?")) {
                    return()
                }
                for (i in seq_along(rmvSet)) {
                    newSet <- modifyList(newSet, rmvSet[[i]]$default, keep.null = TRUE)
                }
            }
            GUI$getActiveDoc()$setSettings(newSet)
            iNZPlotRmveModWin$new(GUI, new = FALSE)
        }
    )
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
        text = function(settings) {
            sprintf("Remove colour coding by %s", as.character(settings$colby))
        },
        default = list(colby = NULL, varnames = list(colby = NULL))
    ),
    sizeby = list(
        text = function(settings) {
            sprintf("Remove resizing by %s", as.character(settings$sizeby))
        },
        default = list(sizeby = NULL, varnames = list(sizeby = NULL))
    ),
    symbolby = list(
        text = function(settings) {
            sprintf("Remove symbol coding by %s", as.character(settings$symbolby))
        },
        default = list(symbolby = NULL, varnames = list(symbolby = NULL))
    ),
    locate = list(
        text = "Remove point labels",
        default = c(
            "locate", "locate.id", "locate.col", "locate.extreme",
            "locate.same.level", "highlight"
        )
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
    plottype = list(text = "Restore default plot type"),
    adjust = list(text = "Reset adjustment (gg)"),
    alpha_densitygroup = list(text = "Reset density transparency (gg)"),
    gg_lwd = list(text = "Restore default line width (gg)"),
    gg_size = list(text = "Restore deafault point size (gg)"),
    ordered = list(text = "Restore default order (gg)"),
    gg_barSize = list(text = "Restore default bar size (gg)"),
    gg_bins = list(text = "Restore default number of bins (gg)"),
    gg_height = list(text = "Reset barcode bar height (gg)"),
    gg_width = list(text = "Reset barcode bar width (gg)"),
    gg_perN = list(text = "Restore default number of observations per grid square (gg)"),
    gg_swarmwidth = list(text = "Restore default swarm width (gg)"),
    gg_method = list(text = "Reset default beeswarm method (gg)"),
    gg_cutpoint = list(text = "Reset cut point (gg)"),
    gg_theme = list(text = "Reset default theme (gg)"),
    fill_colour = list(text = "Restore default fill colour (gg)"),
    rotation = list(text = "Restore default plot orientation (gg)"),
    rotate_labels = list(text = "Restore default plot label orientation (gg)"),
    caption = list(text = "Remove caption (gg)"),
    desc = list(text = "Restore default sort order (gg)"),
    palette = list(text = "Restore default colour palette (gg)")
)
