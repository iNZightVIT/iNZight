context("Data wrangling with survey designs")

# devtools::load_all("../iNZightTools")
ncsr_svy <- iNZightTools::import_survey('ncsr.svydesign')
# ncsr_svy <- iNZightTools::import_survey('tests/testthat/ncsr.svydesign')

# try(ui$close(), TRUE); devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui(ncsr_svy$data)
on.exit(try(ui$close(), TRUE))
Sys.sleep(5)

ui$getActiveDoc()$getModel()$setDesign(ncsr_svy$spec, ui)

test_that("Convert to cat", {
    # source("R/iNZDataModWin.R")
    w <- iNZconToCatWin$new(ui)
    svalue(w$varLbl) <- "weight"
    w$okButton$invoke_change_handler()
})


test_that("Filtering surveys", {
    expect_false(all(ui$getActiveData()$HHincome < 1e5))

    # filter income < 1e5
    w <- iNZFilterWin$new(ui)
    expect_silent(w$filter_var$set_value("HHincome"))
    expect_silent(svalue(w$num_cond) <- "<")
    expect_silent(svalue(w$num_value) <- 1e5)
    expect_true(enabled(w$okBtn))
    expect_silent(w$okBtn$invoke_change_handler())
    expect_true(all(ui$getActiveData()$HHincome < 1e5))
    expect_equal(ui$dataNameWidget$datName, "ncsr.filtered")
    expect_equal(svalue(ui$dataNameWidget$nameLabel), "ncsr.filtered.svy (survey design)")
})


# source("R/iNZChangeDataWin.R")
# w <- iNZAggregateWin$new(ui)

## move this test to a new file eventually ('test_survey_manip.R')
test_that("Aggregating survey data is valid", {
    # to do:
    # - need to use survey objects more uniquely (i.e., stop using data and replace with design)
    #   -> the 'dataname' widget should show that it's a design
    #   -> survey might somehow need a 'name' (possibly attached to the survey spec?)
    w <- iNZAggregateWin$new(ui)
    w$aggvars$set_items(data.frame(Selected = c("race", "education")))
    svalue(w$smryvars) <- c("HHincome", "height")
    w$smryvars$invoke_change_handler()
    svalue(w$gsmry$children[[1]][3,1]) <- TRUE
    w$ok_btn$invoke_change_handler()
    expect_null(ui$getActiveDoc()$getModel()$getDesign())
})
