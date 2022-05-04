# devtools::load_all(); try(ui$close(), TRUE)
cas <- iNZightTools::smart_read('cas500_coded.csv')
ui <- iNZight(cas)
Sys.sleep(0.1)

test_that("Data dictionary can be imported", {
    # devtools::load_all()
    expect_is(ui$getActiveData()$travel, "numeric")
    d <- iNZDataDict$new(ui)
    d$file_picker$set_value("casdict.csv")
    expect_silent(d$ok_button$invoke_change_handler())
    expect_is(ui$getActiveData()$travel, "factor")
})

test_that("Data dictionary can be viewed", {
    # devtools::load_all()
    expect_silent(d <- iNZDDView$new(ui))
    on.exit(d$close())
})
