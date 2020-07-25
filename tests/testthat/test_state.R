context("Save and load application state")

# skip_on_ci()

tmp <- tempfile(fileext = ".inzsave")
on.exit(unlink(tmp))

test_that("Application state can be saved", {
    ui <- iNZGUI$new()
    ui$initializeGui()
    on.exit(gWidgets2::dispose(ui$win))
    Sys.sleep(2)

    ui$setDocument(
        iNZDocument$new(data = census.at.school.500),
        reset = TRUE
    )
    svalue(ui$ctrlWidget$V1box) <- "height"
    svalue(ui$ctrlWidget$V2box) <- "gender"

    expect_silent(ui$saveState(tmp))

    e <- new.env()
    load(tmp, envir = e)
    expect_is(e$state, "list")
})

test_that("Application state can be loaded", {
    ui <- iNZGUI$new()
    ui$initializeGui()
    on.exit(gWidgets2::dispose(ui$win))
    Sys.sleep(2)

    ui$loadState(tmp, .alert = FALSE)
    Sys.sleep(5)

    expect_equivalent(
        ui$getActiveData(),
        census.at.school.500
    )
    expect_equal(
        svalue(ui$ctrlWidget$V1box),
        "height"
    )
    expect_equal(
        svalue(ui$ctrlWidget$V2box),
        "gender"
    )
})

test_that("Reload works", {
    # try(ui$close()); load_all()
    ui <- iNZGUI$new()
    ui$initializeGui(gapminder)
    on.exit(gWidgets2::dispose(ui$win))
    Sys.sleep(2)

    ui$ctrlWidget$V1box$set_value("Region")

    s <- ui$getState()
    expect_silent(ui$reload())
    # expect_equal(ui$getState(), s)

    expect_silent(ui$reload())
    # expect_equal(ui$getState(), s)

    expect_equivalent(ui$getActiveData(), gapminder)
    expect_equal(ui$plotType, "bar")
    expect_equal(svalue(ui$ctrlWidget$V1box), "Region")

    expect_true(enabled(ui$menuBarWidget$menubar$menu_list$Dataset$filter))
})
