context("Various UI elements")

ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(try(ui$close(), TRUE))

test_that("Dataset can be viewed using system viewer", {
    expect_silent(ui$view_dataset())
})
