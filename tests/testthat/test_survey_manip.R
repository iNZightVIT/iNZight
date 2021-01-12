context("Data wrangling with survey designs")

# devtools::load_all("../iNZightTools")
ncsr_svy <- iNZightTools::import_survey('ncsr.svydesign')

# try(ui$close(), TRUE); devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui(ncsr_svy$data)
on.exit(try(ui$close(), TRUE))
Sys.sleep(5)

ui$getActiveDoc()$getModel()$setDesign(ncsr_svy$spec)


test_that("Filtering surveys", {
    # filter income < 1e5

    # source("R/iNZChangeDataWin.R")
    w <- iNZFilterWin$new(ui)


})


# source("R/iNZChangeDataWin.R")
# w <- iNZAggregateWin$new(ui)

## move this test to a new file eventually ('test_survey_manip.R')
test_that("Aggregating survey data is valid", {
    # to do:
    # - need to use survey objects more uniquely (i.e., stop using data and replace with design)
    #   -> the 'dataname' widget should show that it's a design
    #   -> survey might somehow need a 'name' (possibly attached to the survey spec?)


})
