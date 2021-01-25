context("Data manipulation and information")

# try(ui$close(), TRUE); devtools::load_all()
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
    expect_silent(w$filter_var$set_value("gender"))
    expect_silent(svalue(w$cat_levels) <- "male")
    expect_silent(w$cat_levels$invoke_change_handler())
    expect_silent(w$okBtn$invoke_change_handler())
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

test_that("Existing datasets can be joined", {
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

ui$close()

# try(ui$close(), silent = TRUE); devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)

test_that("Uniting columns works", {
    w <- iNZUniteDataWin$new(ui)
    svalue(w$var1) <- c("travel", "gender")
    w$var1$invoke_change_handler()
    expect_equal(svalue(w$var2), "travel_gender")
    expect_true("travel_gender" %in% w$newview$get_names())
    expect_silent(w$unitebtn$invoke_change_handler())
    expect_equal(
        ui$getActiveData()$travel_gender,
        with(census.at.school.500, as.factor(paste(travel, gender, sep = "_")))
    )
})

test_that("Separating columns works", {
    # source("R/iNZChangeDataWin.R")
    w <- iNZSeparateDataWin$new(ui)
    w$format$set_index(2L)
    svalue(w$var1) <- "travel_gender"
    expect_silent(w$var1$invoke_change_handler())
    expect_true(w$var2$set_value("_"))
    w$sep <- "_"
    expect_silent(w$updateView())
    expect_equal(svalue(w$leftCol), "travel")
    expect_equal(svalue(w$rightCol), "gender")
    expect_true(w$leftCol$set_value("mode_of_travel"))
    expect_true(w$rightCol$set_value("sex"))
    expect_silent(w$updateView())
    expect_true(all(c("mode_of_travel", "sex") %in% w$newview$get_names()))
    expect_silent(w$separatebtn$invoke_change_handler())
})
