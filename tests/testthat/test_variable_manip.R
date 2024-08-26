context("Variable manipulation menu")
skip
skip_on_cran()

# devtools::load_all("../iNZightTools")
# devtools::load_all(); try(ui$close(), TRUE)
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(try(ui$close(), TRUE))

test_that("Form Class Intervals", {
    # source("R/iNZDataModWin.R")
    w <- iNZFormClassIntervalsWin$new(ui)
    expect_silent(w$variable$set_value("height"))
    expect_silent(w$n_interval$set_value(5))
    expect_equal(w$start_point$get_value(), 100)
    expect_equal(w$end_point$get_value(), 200)
    lbls <- "[100,119], [120,139], [140,159], [160,179], [180,200]"
    expect_equal(w$preview_levels$get_value(), lbls)
    expect_silent(w$ok_button$invoke_change_handler())
    expect_equal(
        levels(ui$getActiveData()$height.f),
        strsplit(lbls, ", ", fixed = TRUE)[[1]]
    )
})
