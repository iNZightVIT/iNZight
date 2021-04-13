context("iNZight User Preferences")

# skip_on_cran()

# try(ui$close(), TRUE); devtools::load_all()
# ui <- iNZGUI$new()
# ui$initializeGui(census.at.school.500)
# on.exit(try(ui$close(), TRUE))

test_that("Preferences load", {
    p <- iNZPrefsWin$new(ui <- NULL)
    expect_is(p, "iNZPrefsWin")
})
