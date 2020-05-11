context("Get Summary window")

# ui$close()
# devtools::load_all("../..")
ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

ui$setDocument(iNZDocument$new(data = census.at.school.500), reset = TRUE)
Sys.sleep(5)

test_that("Get summary window opens", {

})