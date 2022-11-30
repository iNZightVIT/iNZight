
# devtools::load_all(); try(ui$close(), TRUE)
cas <- iNZightTools::smart_read("cas500_coded.csv")
ui <- iNZight(cas)
on.exit(try(ui$close(), silent = TRUE))
Sys.sleep(0.1)

test_that("Data dictionary can be imported", {
    # devtools::load_all()
    expect_is(ui$getActiveData()$travel, "numeric")
    d <- iNZDataDict$new(ui)
    d$file_picker$set_value("casdict.csv")
    expect_true(svalue(d$apply_dict))
    expect_silent(d$ok_button$invoke_change_handler())
    expect_is(ui$getActiveData()$travel, "factor")
})

test_that("Data dictionary can be viewed", {
    # devtools::load_all()
    expect_silent(d <- iNZDDView$new(ui))
    on.exit(d$close())
})

test_that("DD can be applied to multiple datasets", {
    ui$deleteDataset(ask = FALSE)
    ui$new_document(cas, name = "cas")
    ui$new_document(cas, "dup")

    expect_is(ui$getActiveData()$travel, "numeric")
    d <- iNZDataDict$new(ui)
    d$file_picker$set_value("casdict.csv")
    expect_true(svalue(d$apply_dict))
    expect_false(svalue(d$apply_to_all))
    d$apply_to_all$set_value(TRUE)
    expect_silent(d$ok_button$invoke_change_handler())
    expect_is(ui$getActiveData()$travel, "factor")
    expect_true(
        all(
            sapply(
                ui$iNZDocuments,
                function(x) is.factor(x$dataModel$dataSet$travel)
            )
        )
    )
})
