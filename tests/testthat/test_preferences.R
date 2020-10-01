context("iNZight User Preferences")

# try(ui$close(), TRUE); load_all()
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(try(ui$close(), TRUE))

test_that("Preferences load", {
    p <- iNZPrefsWin$new(ui)
    expect_is(p, "iNZPrefsWin")
})