context("Data is exported from the UI")

# try(ui$close())
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(gWidgets2::dispose(ui$win))

test_that("Export RDA", {
    on.exit(unlink("test.rda"))
    iNZSaveFile("test.rda", "rda", data = census.at.school.500, dataname = "cas")
    load("test.rda")
    expect_equal(cas, census.at.school.500)
})
