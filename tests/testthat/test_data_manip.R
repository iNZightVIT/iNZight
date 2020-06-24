context("Data manipulation and information")

# try(ui$close(), TRUE); load_all()
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)

test_that("Data Maid report", {
    skip_if_not(rmarkdown::pandoc_available())
    w <- iNZDataReportWin$new(ui)
    on.exit(try(gWidgets2::dispose(w$win), TRUE))
})

test_that("Subsetting and reordering columns", {
    # try(gWidgets2::dispose(ui$modWin), TRUE); load_all()
    w <- iNZReorderVarsWin$new(ui)

})
