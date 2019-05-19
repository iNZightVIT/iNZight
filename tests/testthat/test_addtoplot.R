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
    expect_equal(atptbl[[3]]$get_items(), c("dot plot", "histogram"))

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
    ## bar plot doesn't have plot type options, yet
    
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

test_that("Axes and Labels - scatter plots", {
    svalue(ui$ctrlWidget$V1box) <- "height"
    svalue(ui$ctrlWidget$V2box) <- "armspan"
    ui$plotToolbar$addToPlot(message = FALSE)
    svalue(ui$moduleWindow$header$children[[2]]$children[[1]], TRUE) <- 3

    axtbl <- ui$moduleWindow$body$children[[1]]$children[[1]]$children
    # quick check that indices are valid
    # note: future testing should detect these via iteration
    #       to make it easier to modify in future
    expect_equal(svalue(axtbl[[18]]), "x axis :")
    expect_equal(svalue(axtbl[[21]]), "y axis :")
    pl.xlims <- ui$curPlot[[1]][[1]]$xlim
    pl.ylims <- ui$curPlot[[1]][[1]]$ylim
    expect_equal(as.numeric(svalue(axtbl[[19]])), pl.xlims[1])
    expect_equal(as.numeric(svalue(axtbl[[20]])), pl.xlims[2])
    expect_equal(as.numeric(svalue(axtbl[[22]])), pl.ylims[1])
    expect_equal(as.numeric(svalue(axtbl[[23]])), pl.ylims[2])

    # update button
    svalue(ui$moduleWindow$body$children[[2]]$children[[1]]) <- FALSE
    upd <- ui$moduleWindow$body$children[[2]]$children[[2]]

    # set new limits
    svalue(axtbl[[19]]) <- "150"
    svalue(axtbl[[20]]) <- "160"
    svalue(axtbl[[22]]) <- "140"
    svalue(axtbl[[23]]) <- "160"
    upd$invoke_change_handler()
    expect_equal(ui$getActiveDoc()$getSettings()$xlim, c(150, 160))
    expect_equal(ui$getActiveDoc()$getSettings()$ylim, c(140, 160))

    svalue(axtbl[[19]]) <- pl.xlims[1]
    svalue(axtbl[[20]]) <- pl.xlims[2]
    svalue(axtbl[[22]]) <- pl.ylims[1]
    svalue(axtbl[[23]]) <- pl.ylims[2]
    upd$invoke_change_handler()

    # log x/y
    expect_equal(svalue(axtbl[[26]]), "Log (base 10) :")
    expect_false(svalue(axtbl[[27]]))
    expect_false(svalue(axtbl[[28]]))
    svalue(axtbl[[27]]) <- TRUE
    svalue(axtbl[[28]]) <- TRUE
    upd$invoke_change_handler()
    expect_equal(ui$curPlot[[1]][[1]]$xlim, log10(c(20, 200)))
    expect_equal(ui$curPlot[[1]][[1]]$ylim, log10(c(100, 200)))

    # unlogging x/x
    svalue(axtbl[[27]]) <- FALSE
    svalue(axtbl[[28]]) <- FALSE
    upd$invoke_change_handler()
    expect_equal(ui$curPlot[[1]][[1]]$xlim, c(20, 200))
    expect_equal(ui$curPlot[[1]][[1]]$ylim, c(100, 200))

    ui$moduleWindow$footer$children[[2]]$invoke_change_handler()
    ui$getActiveDoc()$setSettings(list(xlim = NULL, ylim = NULL))

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
