context("Get Summary window")

# try(ui$close(), TRUE); devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

ui$setDocument(iNZDocument$new(data = census.at.school.500), reset = TRUE)
if (!interactive()) Sys.sleep(5)

test_that("Get summary window opens", {
    ui$ctrlWidget$V1box$set_value("height")
    sw <- ui$sumBtn$invoke_change_handler()
    # expect_is(sw, "iNZInfoWindow")

})
