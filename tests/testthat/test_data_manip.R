context("Data manipulation and information")

# try(ui$close(), TRUE); load_all()
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)

test_that("Data Maid report", {
    w <- iNZDataReportWin$new(ui)
    on.exit(try(gWidgets2::dispose(w$win), TRUE))
})
