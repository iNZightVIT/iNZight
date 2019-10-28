context("Data is exported from the UI")

ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(gWidgets2::dispose(ui$win))

test_that("Export RDA", {

})