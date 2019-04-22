context("Load and save RDS files")

ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

test_that("Strings are converted to factor", {
    expect_silent(iNZLoadSaveWin$new(ui, "load", "test.rds"))
    expect_equal(ui$getActiveData()$x, 1:5)
    expect_equal(ui$getActiveData()$y, as.factor(LETTERS[1:5]))
})

test_that("Saving as RDS works", {
    expect_silent(iNZLoadSaveWin(ui, "save", "test2.rds"))
    expect_equal(
        readRDS("test2.rds"),
        structure(
            data.frame(
                x = 1:5,
                y = as.factor(LETTERS[1:5])
            ),
            name = "data",
            code = ""
        )
    )
    unlink("test2.rds")
})
