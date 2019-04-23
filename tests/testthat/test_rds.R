context("Load and save RDS files")

ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

tmp <- tempfile(fileext = ".rds")
on.exit(unlink(tmp), add = TRUE)

d <- data.frame(x = 1:5, y = LETTERS[1:5])
saveRDS(d, tmp)

test_that("Data can be saved as an RDS", {
    tmp2 <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp2))

    iNZLoadSaveWin(ui, "save", tmp2)
    expect_equal(
        readRDS(tmp2),
        ui$getActiveData()
    )
})

test_that("Data can be loaded from RDS", {
    iNZLoadSaveWin$new(ui, "load", tmp)
    expect_equal(dim(ui$getActiveData()), c(5, 2))
})

test_that("Strings are converted to factor", {
    expect_is(ui$getActiveData()$y, "factor")
})
