# devtools::load_all("../iNZightPlots"); devtools::load_all(); try(ui$close(), TRUE)
cas5k <- iNZightMR::census.at.school.5000
ui <- iNZight(cas5k)

on.exit({
    ui$preferences$multiple_x <- FALSE
    ui$savePreferences()
    ui$close()
})

if (ui$preferences$multiple_x) {
    ui$preferences$multiple_x <- FALSE
    ui$savePreferences()
    ui$reload()
}


test_that("V1 is dropdown when multiple_x is FALSE", {
    expect_s4_class(ui$ctrlWidget$V1box, "GComboBoxNoEntry")
})

ui$preferences$multiple_x <- TRUE
ui$savePreferences()
ui$reload()
Sys.sleep(0.1)

test_that("V1 is a multi label when multiple_x is TRUE", {
    expect_true(inherits(ui$ctrlWidget$V1box, "GMultiLabel"))

})
