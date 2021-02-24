context("Variable manipulation menu")

# devtools::load_all("../iNZightTools")
# devtools::load_all(); try(ui$close(), TRUE)
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(try(ui$close(), TRUE))

test_that("Form Class Intervals", {
    # source("R/iNZDataModWin.R")
    w <- iNZformClassIntervals$new(ui)

})
