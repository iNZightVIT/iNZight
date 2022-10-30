context("Data wrangling with survey designs")

skip_on_os("windows")
skip_on_cran()

# devtools::load_all("../iNZightTools")
ncsr_svy <- surveyspec::import_survey("ncsr.svydesign")
# ncsr_svy <- surveyspec::import_survey('tests/testthat/ncsr.svydesign')

# try(ui$close(), TRUE); devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui(ncsr_svy$data)
on.exit(try(ui$close(), TRUE))
Sys.sleep(5)

ui$getActiveDoc()$getModel()$setDesign(ncsr_svy$spec, ui)

test_that("Convert to cat", {
    # source("R/iNZDataModWin.R")
    w <- iNZConToCatWin$new(ui)
    svalue(w$varLbl) <- "weight"
    expect_silent(w$ok_button$invoke_change_handler())
})


test_that("Filtering surveys", {
    expect_false(all(ui$getActiveData()$HHincome < 1e5))

    # filter income < 1e5
    w <- iNZFilterWin$new(ui)
    expect_silent(w$filter_var$set_value("HHincome"))
    expect_silent(svalue(w$num_cond) <- "<")
    expect_silent(svalue(w$num_value) <- 1e5)
    expect_true(enabled(w$ok_button))
    expect_silent(w$ok_button$invoke_change_handler())
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
    w$gsmry$children[[1]][3, 1]$set_value(TRUE)
    w$ok_button$invoke_change_handler()
    expect_null(ui$getActiveDoc()$getModel()$getDesign())
})

ui$close()

ui <- iNZGUI$new()
ui$initializeGui(ncsr_svy$data)
Sys.sleep(5)
ui$getActiveDoc()$getModel()$setDesign(ncsr_svy$spec, ui)

test_that("Uniting columns works", {
    w <- iNZUniteWin$new(ui)
    svalue(w$var1) <- c("race", "marital")
    w$var1$invoke_change_handler()
    expect_equal(svalue(w$var2), "race_marital")
    expect_true("race_marital" %in% w$newview$get_names())
    expect_silent(w$ok_button$invoke_change_handler())
    expect_equal(
        ui$getActiveData()$race_marital,
        with(ncsr_svy$data, as.factor(paste(race, marital, sep = "_")))
    )
    expect_true(iNZightTools::is_survey(ui$get_data_object()))
})

test_that("Separating columns works", {
    w <- iNZSeparateWin$new(ui)
    svalue(w$var1) <- "race_marital"
    expect_silent(w$var1$invoke_change_handler())
    expect_true(w$var2$set_value("_"))
    w$sep <- "_"
    expect_silent(w$updateView())
    expect_equal(svalue(w$leftCol), "race")
    expect_equal(svalue(w$rightCol), "marital")
    expect_true(w$leftCol$set_value("new_race"))
    expect_true(w$rightCol$set_value("new_marital"))
    expect_silent(w$updateView())
    expect_true(all(c("new_race", "new_marital") %in% w$newview$get_names()))
    expect_silent(w$ok_button$invoke_change_handler())
    expect_true(iNZightTools::is_survey(ui$get_data_object()))
    expect_equal(
        ui$get_data_object()$variables$new_race,
        as.character(ui$get_data_object()$variables$race)
    )
    expect_equal(
        ui$get_data_object()$variables$new_marital,
        as.character(ui$get_data_object()$variables$marital)
    )
})
