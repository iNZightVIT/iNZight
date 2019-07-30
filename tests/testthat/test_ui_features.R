context("Various UI elements")

ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(try(ui$close(), TRUE))

test_that("Dataset can be viewed using system viewer", {
    skip_if(Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "") %in% c("stop", "warn"))
    expect_silent(ui$view_dataset())
})
