context("Data manipulation and information")

# try(ui$close(), TRUE); load_all()
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(try(ui$close(), TRUE))
Sys.sleep(5)

test_that("Data Maid report", {
    skip_if_not(rmarkdown::pandoc_available())
    w <- iNZDataReportWin$new(ui)
    on.exit(try(gWidgets2::dispose(w$win), TRUE))
})

test_that("Subsetting and reordering columns", {
    # try(gWidgets2::dispose(ui$modWin), TRUE); load_all()
    w <- iNZReorderVarsWin$new(ui)
    on.exit(try(gWidgets2::dispose(ui$modWin), TRUE))
})

test_that("Filtering data leaves code OK", {
    ui$ctrlWidget$V1box$set_value("height")
    ui$ctrlWidget$V2box$set_value("armspan")
    ui$getActiveDoc()$setSettings(list(colby = as.name("gender")))
    expect_match(
        svalue(ui$code_panel$input),
        "inzplot(height ~ armspan, colby = gender, data = data)",
        fixed = TRUE
    )

    w <- iNZFilterWin$new(ui)
    gWidgets2::dispose(ui$modWin)
    w$opt1()
    ui$modWin$children[[1]]$children[[1]]$children[[2]]$set_value("gender")
    svalue(ui$modWin$children[[1]]$children[[2]]) <- "male"
    expect_silent(
        ui$modWin$children[[1]]$children[[3]]$children[[1]]$invoke_change_handler()
    )
    expect_match(
        svalue(ui$code_panel$input),
        "inzplot(height ~ armspan, colby = gender, data = data.filtered)",
        fixed = TRUE
    )
})

# source("R/iNZChangeDataWin.R")
# w <- iNZAggregateWin$new(ui)

test_that("Aggregating data adds correct code", {
    w <- iNZAggregateWin$new(ui)
    # w <- ui$modWin
    # w$children[[1]]$children[[3]]$children[[2]]$set_value("gender")
    # w$children[[1]]$children[[3]]$children[[4]]$set_value("travel")
    # w$children[[1]]$children[[3]]$children[[7]]$set_value("Mean")
    # expect_silent(
    #     w$children[[1]]$children[[4]]$children[[1]]$invoke_change_handler()
    # )
})


ncsr_svy <- iNZightTools::import_survey('ncsr.svydesign')

# try(ui$close(), TRUE); load_all()
ui <- iNZGUI$new()
ui$initializeGui(ncsr_svy$data)
on.exit(try(ui$close(), TRUE))
Sys.sleep(5)

ui$getActiveDoc()$getModel()$setDesign(ncsr_svy$spec)

# source("R/iNZChangeDataWin.R")
# w <- iNZAggregateWin$new(ui)

## move this test to a new file eventually ('test_survey_manip.R')
test_that("Aggregating survey data is valid", {
    # to do:
    # - need to use survey objects more uniquely (i.e., stop using data and replace with design)
    #   -> the 'dataname' widget should show that it's a design
    #   -> survey might somehow need a 'name' (possibly attached to the survey spec?)


})
