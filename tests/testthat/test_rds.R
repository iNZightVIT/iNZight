context("Load and save RDS files")

ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

test_that("Strings are converted to factor", {
    iNZLoadSaveWin$new(ui, "load", "test.rds")
    # expect_equal(ui$getActiveData()$x, 1:5)
    # expect_equal(ui$getActiveData()$y, as.factor(LETTERS[1:5]))
})

test_that("Saving as RDS works", {
    # tmp <- tempfile(fileext = ".rds")
    # on.exit(unlink(tmp))

    # iNZLoadSaveWin(ui, "save", tmp)
    # expect_equal(
    #     readRDS(tmp),
    #     ui$getActiveData()
    # )
})
