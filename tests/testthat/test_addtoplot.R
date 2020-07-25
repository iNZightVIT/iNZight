context("Add to Plot window")

ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

test_that("Message shown if no data loaded", {
    expect_false(ui$plotToolbar$addToPlot(message = FALSE))
})

ui$setDocument(iNZDocument$new(data = census.at.school.500), reset = TRUE)
# leave time for the treeview to display
Sys.sleep(5)
svalue(ui$ctrlWidget$V1box) <- "height"
test_that("Window loads with data", {
    expect_true(ui$plotToolbar$addToPlot(message = FALSE))
})

atpOpts <- c(
    "Customise Plot Appearance",
    "Trend Lines and Curves",
    "Axes and Labels",
    "Identify Points"
)

test_that("Add to Plot shows correct options by plot", {
    cmbo <- ui$moduleWindow$header$children[[2]]$children[[1]]
    expect_equal(svalue(cmbo), "Customise Plot Appearance")
    expect_equal(cmbo$get_items(), atpOpts[-2])

    atptbl <- ui$moduleWindow$body$children[[1]]$children[[1]]$children
    expect_equal(svalue(atptbl[[3]]), "dot plot")
    expect_equal(
        atptbl[[3]]$get_items(),
        c(
            "dot plot", "histogram", "(gg) dot strip", "(gg) barcode",
            "(gg) boxplot", "(gg) beeswarm", "(gg) violin", "(gg) density",
            "(gg) column/row bar", "(gg) lollipop", "(gg) cumulative curve"
        )
    )

    # change to scatter plot
    ui$moduleWindow$footer$children[[2]]$invoke_change_handler()
    svalue(ui$ctrlWidget$V2box) <- "rightfoot"
    expect_true(ui$plotToolbar$addToPlot(message = FALSE))

    cmbo <- ui$moduleWindow$header$children[[2]]$children[[1]]
    expect_equal(svalue(cmbo), "Customise Plot Appearance")
    expect_equal(cmbo$get_items(), atpOpts)

    atptbl <- ui$moduleWindow$body$children[[1]]$children[[1]]$children
    expect_equal(svalue(atptbl[[3]]), "scatter")
    expect_equal(atptbl[[3]]$get_items(),
        c("scatter", "hexagonal binning", "grid-density")
    )

    # change to bar plot
    ui$moduleWindow$footer$children[[2]]$invoke_change_handler()
    svalue(ui$ctrlWidget$V2box, index = TRUE) <- 1
    svalue(ui$ctrlWidget$V1box) <- "travel"
    expect_true(ui$plotToolbar$addToPlot(message = FALSE))

    cmbo <- ui$moduleWindow$header$children[[2]]$children[[1]]
    expect_equal(svalue(cmbo), "Customise Plot Appearance")
    expect_equal(cmbo$get_items(), atpOpts[c(1, 3)])

    atptbl <- ui$moduleWindow$body$children[[1]]$children[[1]]$children
    expect_equal(atptbl[[3]]$get_items(),
        c(
            "barplot", "(gg) column/row bar", "(gg) stacked column/row",
            "(gg) lollipop", "(gg) gridplot", "(gg) pie", "(gg) donut"
        )
    )

    ui$moduleWindow$footer$children[[2]]$invoke_change_handler()
    svalue(ui$ctrlWidget$V2box, TRUE) <- 1
    svalue(ui$ctrlWidget$V1box, TRUE) <- 1
})

test_that("Axes and Labels - dot plots", {
    svalue(ui$ctrlWidget$V1box) <- "height"
    ui$plotToolbar$addToPlot(message = FALSE)
    svalue(ui$moduleWindow$header$children[[2]]$children[[1]], TRUE) <- 2

    axtbl <- ui$moduleWindow$body$children[[1]]$children[[1]]$children
    expect_equal(svalue(axtbl[[9]]), "x axis :")
    pl.lims <- ui$curPlot[[1]][[1]]$xlim
    expect_equal(as.numeric(svalue(axtbl[[10]])), pl.lims[1])
    expect_equal(as.numeric(svalue(axtbl[[11]])), pl.lims[2])

    # update button
    svalue(ui$moduleWindow$body$children[[2]]$children[[1]]) <- FALSE
    upd <- ui$moduleWindow$body$children[[2]]$children[[2]]

    # set new limits
    svalue(axtbl[[10]]) <- "150"
    svalue(axtbl[[11]]) <- "160"
    upd$invoke_change_handler()
    expect_equal(ui$getActiveDoc()$getSettings()$xlim, c(150, 160))

    svalue(axtbl[[10]]) <- pl.lims[1]
    svalue(axtbl[[11]]) <- pl.lims[2]
    upd$invoke_change_handler()

    # logging x
    expect_equal(svalue(axtbl[[14]]), "Log (base 10) :")
    expect_false(svalue(axtbl[[15]]))
    svalue(axtbl[[15]]) <- TRUE
    upd$invoke_change_handler()
    expect_equal(ui$curPlot[[1]][[1]]$xlim, log10(c(100, 200)))

    # unlogging x
    svalue(axtbl[[15]]) <- FALSE
    upd$invoke_change_handler()
    expect_equal(ui$curPlot[[1]][[1]]$xlim, c(100, 200))

    ui$moduleWindow$footer$children[[2]]$invoke_change_handler()
    ui$getActiveDoc()$setSettings(list(xlim = NULL, ylim = NULL))
    svalue(ui$ctrlWidget$V1box, TRUE) <- 1
})

test_that("Changing variable resets axis limits", {
    svalue(ui$ctrlWidget$V1box) <- "height"
    ui$plotToolbar$addToPlot(message = FALSE)
    svalue(ui$moduleWindow$header$children[[2]]$children[[1]], TRUE) <- 2

    upd <- ui$moduleWindow$body$children[[2]]$children[[2]]
    axtbl <- ui$moduleWindow$body$children[[1]]$children[[1]]$children
    svalue(axtbl[[10]]) <- "150"
    svalue(axtbl[[11]]) <- "200"
    upd$invoke_change_handler()

    ui$moduleWindow$footer$children[[2]]$invoke_change_handler()

    expect_equal(ui$getActiveDoc()$getSettings()$xlim, c(150, 200))
    expect_silent(svalue(ui$ctrlWidget$V1box) <- "rightfoot")
    expect_null(ui$getActiveDoc()$getSettings()$xlim)
})

if (FALSE) {
    # try(ui$close()); load_all()
    ui <- iNZGUI$new()
    ui$initializeGui(census.at.school.500)
    Sys.sleep(5)
}


test_that("Axes and Labels - scatter plots", {
    svalue(ui$ctrlWidget$V1box) <- "height"
    svalue(ui$ctrlWidget$V2box) <- "armspan"
    ui$plotToolbar$addToPlot(message = FALSE)
    svalue(ui$moduleWindow$header$children[[2]]$children[[1]], TRUE) <- 3

    axtbl <- ui$moduleWindow$body$children[[1]]$children[[1]]
    vals <- sapply(seq_len(axtbl$get_dim()[1L]),
        function(x) {
            x <- svalue(axtbl[x, 1L])
            if (is.null(x)) NA else x
        }
    )
    xi <- grep("x axis", vals)
    yi <- grep("y axis", vals)
    pl.xlims <- ui$curPlot[[1]][[1]]$xlim
    pl.ylims <- ui$curPlot[[1]][[1]]$ylim
    expect_equal(as.numeric(svalue(axtbl[xi, 3])), pl.xlims[1])
    expect_equal(as.numeric(svalue(axtbl[xi, 5])), pl.xlims[2])
    expect_equal(as.numeric(svalue(axtbl[yi, 3])), pl.ylims[1])
    expect_equal(as.numeric(svalue(axtbl[yi, 5])), pl.ylims[2])

    # update button
    svalue(ui$moduleWindow$body$children[[2]]$children[[1]]) <- FALSE
    upd <- ui$moduleWindow$body$children[[2]]$children[[2]]

    # set new limits
    xli <- which(sapply(axtbl$child_positions, function(x) identical(x$child, axtbl[xi, 3])))
    xui <- which(sapply(axtbl$child_positions, function(x) identical(x$child, axtbl[xi, 5])))
    yli <- which(sapply(axtbl$child_positions, function(x) identical(x$child, axtbl[yi, 3])))
    yui <- which(sapply(axtbl$child_positions, function(x) identical(x$child, axtbl[yi, 5])))
    xLower <- axtbl$child_positions[[xli]]$child
    xUpper <- axtbl$child_positions[[xui]]$child
    yLower <- axtbl$child_positions[[yli]]$child
    yUpper <- axtbl$child_positions[[yui]]$child

    svalue(xLower) <- "150"
    svalue(xUpper) <- "160"
    svalue(yLower) <- "140"
    svalue(yUpper) <- "160"
    upd$invoke_change_handler()
    expect_equal(ui$getActiveDoc()$getSettings()$xlim, c(150, 160))
    expect_equal(ui$getActiveDoc()$getSettings()$ylim, c(140, 160))

    svalue(xLower) <- pl.xlims[1]
    svalue(xUpper) <- pl.xlims[2]
    svalue(yLower) <- pl.ylims[1]
    svalue(yUpper) <- pl.ylims[2]
    upd$invoke_change_handler()

    # log x/y
    expect_equal(svalue(axtbl$children[[26]]), "Log (base 10) :")
    expect_false(svalue(axtbl$children[[27]]))
    expect_false(svalue(axtbl$children[[28]]))
    svalue(axtbl$children[[27]]) <- TRUE
    svalue(axtbl$children[[28]]) <- TRUE
    upd$invoke_change_handler()
    expect_equal(ui$curPlot[[1]][[1]]$xlim, log10(c(20, 200)))
    expect_equal(ui$curPlot[[1]][[1]]$ylim, log10(c(100, 200)))

    # unlogging x/x
    svalue(axtbl$children[[27]]) <- FALSE
    svalue(axtbl$children[[28]]) <- FALSE
    upd$invoke_change_handler()
    expect_equal(ui$curPlot[[1]][[1]]$xlim, c(20, 200))
    expect_equal(ui$curPlot[[1]][[1]]$ylim, c(100, 200))

    ui$moduleWindow$footer$children[[2]]$invoke_change_handler()

    svalue(ui$ctrlWidget$V2box, TRUE) <- 1
    svalue(ui$ctrlWidget$V1box, TRUE) <- 1
})

test_that("Axes and Labels - bar plots", {
    svalue(ui$ctrlWidget$V1box) <- "travel"
    svalue(ui$ctrlWidget$V2box, TRUE) <- 1
    ui$plotToolbar$addToPlot(message = FALSE)
    svalue(ui$moduleWindow$header$children[[2]]$children[[1]], TRUE) <- 2

    axtbl <- ui$moduleWindow$body$children[[1]]$children[[1]]$children
    expect_equal(svalue(axtbl[[9]]), "Display values as: ")
    expect_equal(svalue(axtbl[[10]]), "Percentages (%)")

    svalue(axtbl[[10]], TRUE) <- 2
    expect_equal(round(ui$curPlot$ylim), c(0, 229))

    svalue(axtbl[[10]], TRUE) <- 1
    expect_equal(round(ui$curPlot$ylim, 2), c(0, 0.46))

    ui$moduleWindow$footer$children[[2]]$invoke_change_handler()

    svalue(ui$ctrlWidget$V2box) <- "cellsource"
    ui$plotToolbar$addToPlot(message = FALSE)
    svalue(ui$moduleWindow$header$children[[2]]$children[[1]], TRUE) <- 2

    axtbl <- ui$moduleWindow$body$children[[1]]$children[[1]]$children
    expect_equal(svalue(axtbl[[9]]), "Display values as: ")
    expect_equal(svalue(axtbl[[10]]), "Percentages (%)")

    svalue(axtbl[[10]], TRUE) <- 2
    expect_equal(round(ui$curPlot$ylim), c(0, 81))

    svalue(axtbl[[10]], TRUE) <- 1
    expect_equal(round(ui$curPlot$ylim, 2), c(0, 0.47))

    ui$moduleWindow$footer$children[[2]]$invoke_change_handler()
    svalue(ui$ctrlWidget$V2box, TRUE) <- 1
    svalue(ui$ctrlWidget$V1box, TRUE) <- 1
})
