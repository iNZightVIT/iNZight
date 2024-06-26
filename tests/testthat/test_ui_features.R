context("Various UI elements")

skip_on_cran()

ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(ui$close())

test_that("Dataset can be viewed using system viewer", {
    skip_if(Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "") %in% c("stop", "warn"))
    expect_silent(ui$view_dataset())
})

test_that("iNZWindow returns FALSE if gui is NULL", {
    expect_error(iNZWindow$new(NULL), "should be an iNZGUI object")
})
