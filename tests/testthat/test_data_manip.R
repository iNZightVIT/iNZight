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

ui$close()

# try(ui$close(), silent = TRUE); devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui()

test_that("Existing atasets can be joined", {
    # first, set two datasets:
    d1 <- data.frame(x = c("A", "B", "C", "D"), y = 1:4,
        stringsAsFactors = TRUE)
    d2 <- data.frame(x = c("A", "B", "C", "D"), z = 1:4 * 1234,
        stringsAsFactors = TRUE)

    attr(d1, "name") <- "data1"
    attr(d2, "name") <- "data2"
    doc1 <- iNZDocument$new(data = d1)
    doc2 <- iNZDocument$new(data = d2)
    ui$setDocument(doc1)
    expect_equivalent(ui$getActiveData(), d1)
    ui$setDocument(doc2)
    expect_equivalent(ui$getActiveData(), d2)

    # merge data1 to data2
    # source('R/iNZChangeDataWin.R'); try(dispose(jw), TRUE)
    jw <- iNZjoinDataWin$new(ui)
    expect_silent(jw$data_name$set_value("data1"))
    expect_silent(jw$joinbtn$invoke_change_handler())
    expect_equivalent(
        ui$getActiveData()[,c("x", "y", "z")],
        dplyr::inner_join(d1, d2, by = "x")
    )
    expect_equal(
        iNZightTools::code(ui$getActiveData()),
        ""
    )
})
