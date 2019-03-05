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
    # expect_equal(svalue(axtbl[[14]]), "Log (base 10) :")
    # expect_false(svalue(axtbl[[15]]))
    # svalue(axtbl[[15]]) <- TRUE
    # upd$invoke_change_handler()
})

