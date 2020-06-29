context("Data manipulation and information")

# try(ui$close(), TRUE); load_all()
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)

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
        "iNZPlot(height ~ armspan, colby = gender, data = data)",
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
        "iNZPlot(height ~ armspan, colby = gender, data = data.filtered)",
        fixed = TRUE
    )
})
