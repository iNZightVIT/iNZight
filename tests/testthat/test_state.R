context("Save and load application state")

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
    Sys.sleep(2)
    on.exit(gWidgets2::dispose(ui$win))

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
