context("Data is loaded into the UI")

# setwd('tests/testthat'); devtools::load_all()
wd <- getwd()
ui <- iNZGUI$new()
ui$initializeGui()
on.exit(try(gWidgets2::dispose(ui$win), silent = TRUE))
setwd(wd)

doc <- NULL
test_that("New document is created correctly when data loaded", {
    testdata <- data.frame(A = 1:10, B = LETTERS[1:10], stringsAsFactors = TRUE)
    attr(testdata, "name") <- "testdata"

    expect_silent(doc <<- iNZDocument$new(data = testdata))
    expect_is(doc, "iNZDocument")
    expect_equivalent(
        doc$dataModel$getData(),
        testdata
    )
    expect_equal(doc$dataModel$name, "testdata")
})

### to here's good

test_that("UI correctly displays the data", {
    expect_silent(ui$setDocument(doc, reset = TRUE))
    expect_equal(ui$getActiveData()$A, 1:10)
    expect_equal(ui$getActiveData()$B, factor(LETTERS[1:10]))
    expect_equal(attr(ui$getActiveData(), "name"), "testdata")
    expect_equal(attr(ui$getActiveData(), "code"), "")

    ## data name is being displayed
    expect_equal(ui$dataNameWidget$datName, "testdata")
    expect_equal(svalue(ui$dataNameWidget$nameLabel), "testdata")


    ## data view disabled; list view enabled
    expect_false(enabled(ui$dataToolbarWidget$dataBtn))
    expect_true(enabled(ui$dataToolbarWidget$listBtn))
    expect_equal(ui$dataViewWidget$current, "data")

    ## clicking list view chanes the data view
    expect_silent(ui$dataToolbarWidget$listBtn$invoke_change_handler())
    expect_equal(ui$dataViewWidget$current, "variables")

    expect_equal(
        ui$dataViewWidget$varView$children[[1]]$get_names(),
        c("Name", "Type", "Info", "Missing")
    )
    expect_equivalent(
        ui$dataViewWidget$varView$children[[1]]$get_items(),
        data.frame(
            Name = c("A", "B"),
            Type = c("numeric", "categorical"),
            Info = c("min 1, max 10", "10 levels"),
            Missing = c(0, 0)
        )
    )

    ## variable options are correct
    expect_equal(ui$ctrlWidget$V1box$get_items()[-1], c("A", "B"))
    expect_equal(ui$ctrlWidget$V2box$get_items()[-1], c("A", "B"))
    expect_equal(ui$ctrlWidget$G1box$get_items()[-1], c("A", "B"))
    expect_equal(ui$ctrlWidget$G2box$get_items()[-1], c("A", "B"))
})

test_that("Example data menus work correctly", {
    skip_if_not_installed("iNZightModules")

    exwin <- iNZImportExampleWin$new(ui)
    expect_equal(svalue(exwin$dsPkg), "Default")
    # expect_equal(exwin$dsPkg$get_items(),
    #     c("Default", "Multiple Response", "Time Series",
    #       "Maps", "Survey", "FutureLearn")
    # )
    expect_equal(svalue(exwin$dsData), character())
    expect_equal(length(exwin$dsData$get_items()), 3L)

    # set a package
    expect_silent(svalue(exwin$dsPkg) <- "Time Series")
    expect_equal(
        exwin$dsData$get_items(),
        c("seaice", "visitorsA2", "visitorsM2", "visitorsQ")
    )
    expect_equal(svalue(exwin$dsTitle), "")

    # choose data
    expect_silent(svalue(exwin$dsData) <- "seaice")
    expect_equal(svalue(exwin$dsTitle), "Sea Ice")

    # choose another package resets fields
    expect_silent(svalue(exwin$dsPkg) <- "Default")
    expect_equal(svalue(exwin$dsData, TRUE), 0)
    expect_equal(svalue(exwin$dsTitle), "")

    expect_silent(svalue(exwin$dsData) <- "census.at.school.500")
    if (grepl(" ", svalue(exwin$dsTitle))) {
        expect_equal(svalue(exwin$dsTitle), "Census at School 500")
    }

    # load it
    expect_silent(exwin$ok_button$invoke_change_handler())
    expect_equal(ui$dataNameWidget$datName, "census.at.school.500_ex")
})

test_that("CSV files load", {
    imp <- iNZImportWin$new(ui)
    imp$fname <- "cas5.csv"
    imp$setfile()
    skip_if(length(imp$prevGp$children) == 1,
        message = "Preview did not load."
    )
    expect_is(imp$prevGp$children[[2]], "GDf")
    expect_equal(imp$prevGp$children[[2]]$get_dim(), c(rows = 5, cols = 10))
    expect_silent(imp$ok_button$invoke_change_handler())
    expect_equal(
        names(ui$getActiveData()),
        c(
            "cellsource", "rightfoot", "travel", "getlunch", "height",
            "gender", "age", "year", "armspan", "cellcost"
        )
    )
    expect_equal(
        dim(ui$getActiveData()),
        c(5, 10)
    )
})

## good to here ----

test_that("SAS (.sas7bdat) files load", {
    imp <- iNZImportWin$new(ui)
    imp$fname <- "test.sas7bdat"
    imp$setfile()
    skip_if(length(imp$prevGp$children) == 1,
        message = "Preview did not load."
    )
    expect_is(imp$prevGp$children[[2]], "GDf")
    expect_equal(imp$prevGp$children[[2]]$get_dim(), c(rows = 5, cols = 7))
    imp$ok_button$invoke_change_handler()
    # expect_silent(imp$okBtn$invoke_change_handler())
    expect_equal(
        names(ui$getActiveData()),
        c("id", "workshop", "gender", "q1", "q2", "q3", "q4")
    )
    expect_equal(
        dim(ui$getActiveData()),
        c(8, 7)
    )
})

test_that("SAS Xport (.xpt) files load", {
    imp <- iNZImportWin$new(ui)
    imp$fname <- "cars.xpt"
    imp$setfile()
    skip_if(length(imp$prevGp$children) == 1,
        message = "Preview did not load."
    )
    expect_is(imp$prevGp$children[[2]], "GDf")
    expect_equal(imp$prevGp$children[[2]]$get_dim(), c(rows = 5, cols = 5))
    imp$ok_button$invoke_change_handler()
    # expect_silent(imp$okBtn$invoke_change_handler())
    expect_equal(
        names(ui$getActiveData()),
        c("MAKE", "PRICE", "MPG", "REP78", "FOREIGN")
    )
    expect_equal(
        dim(ui$getActiveData()),
        c(26, 5)
    )
})

# test_that("Switching variable types works (csv)", {
#     imp <- iNZImportWin$new(ui)
#     imp$fname <- "cas5.csv"
#     imp$setfile()

#     skip_if(length(imp$prevGp$children) == 1,
#         message = "Preview did not load."
#     )
#     # convert YEAR to cat
#     expect_equal(
#         imp$prev$get_names(),
#         c(
#             "cellsource (c)", "rightfoot (n)", "travel (c)",
#             "getlunch (c)", "height (n)", "gender (c)",
#             "age (n)", "year (n)", "armspan (n)", "cellcost (n)"
#         )
#     )
#     imp$fColTypes[8] <- "categorical"
#     imp$generatePreview(NULL)
#     expect_equal(
#         imp$prev$get_names(),
#         c(
#             "cellsource (c)", "rightfoot (n)", "travel (c)",
#             "getlunch (c)", "height (n)", "gender (c)",
#             "age (n)", "year (c)", "armspan (n)", "cellcost (n)"
#         )
#     )

#     imp$ok_button$invoke_change_handler()
#     expect_is(ui$getActiveData()$year, "factor")
# })

#### somewhere up there??? ^^^

# test_that("Date times are supported (csv)", {
#     imp <- iNZImportWin$new(ui)
#     imp$fname <- "dt.csv"
#     imp$setfile()
#     skip_if(length(imp$prevGp$children) == 1,
#         message = "Preview did not load."
#     )

#     expect_equal(
#         imp$prev$get_names(),
#         c("x (d)", "y (t)", "z (dt)")
#     )
#     imp$ok_button$invoke_change_handler()
#     expect_is(ui$getActiveData()$x, "Date")
#     expect_is(ui$getActiveData()$y, "hms")
#     expect_is(ui$getActiveData()$z, "POSIXct")
# })

# test_that("Changing file resets column types", {
#     imp <- iNZImportWin$new(ui)
#     imp$fname <- "dt.csv"
#     imp$fColTypes <- c("numeric", "numeric", "auto")
#     expect_silent(imp$setfile())
#     expect_true(all(imp$fColTypes == "auto"))
# })

# test_that("RData files display list of objects", {
#     save(census.at.school.500, iris, file = "test.rda")
#     on.exit(unlink("test.rda"))
#     imp <- iNZImportWin$new(ui)
#     imp$fname <- "test.rda"
#     expect_silent(imp$setfile())
#     expect_equal(svalue(imp$rdaName), "iris")
#     expect_equal(imp$rdaName$get_items(), c("iris", "census.at.school.500"))
#     expect_silent(svalue(imp$rdaName, index = TRUE) <- 2)
#     imp$ok_button$invoke_change_handler()
# })



# # try(ui$close());
# # ui <- iNZGUI$new()
# # ui$initializeGui()

# test_that("Excel files load and display available sheets", {
#     imp <- iNZImportWin$new(ui)
#     imp$fname <- "sheet.xlsx"
#     expect_silent(imp$setfile())
#     expect_equal(imp$rdaName$get_items(), c("Africa", "Americas", "Asia", "Europe", "Oceania"))
#     expect_equal(svalue(imp$rdaName), "Africa")
#     expect_silent(svalue(imp$rdaName, index = TRUE) <- 3)
#     expect_silent(imp$ok_button$invoke_change_handler())
#     expect_true(all(as.character(ui$getActiveData()$continent) == "Asia"))
# })


# # try(ui$close()); load_all()
# # ui <- iNZGUI$new()
# # ui$initializeGui()

# test_that("User can choose to load a URL", {
#     imp <- iNZImportWin$new(ui)
#     svalue(imp$loadURL) <- TRUE
#     svalue(imp$fileurl) <- "https://raw.githubusercontent.com/iNZightVIT/iNZight/dev/tests/testthat/cas5.csv"
#     expect_equal(imp$fext, "csv")
#     expect_silent(imp$ok_button$invoke_change_handler())
#     expect_equivalent(
#         ui$getActiveData(),
#         iNZightTools::smart_read("cas5.csv")
#     )
# })


# ## Many columns file:
# many_cols <- data.frame(X1 = 1:20)
# for (i in 2:30) {
#     many_cols[[sprintf("X%i", i)]] <- sample(50, 20)
# }
# for (i in 31:40) {
#     many_cols[[sprintf("X%i", i)]] <- sample(LETTERS[1:5], 20, TRUE)
# }
# for (i in 41:50) {
#     many_cols[[sprintf("X%i", i)]] <- sample(LETTERS, 20)
# }

# tf <- tempfile(fileext = ".csv")
# on.exit(unlink(tf), add = TRUE)
# write.csv(many_cols, tf, quote = FALSE, row.names = FALSE)

# imp <- iNZImportWin$new(ui)
# test_that("Data sets with many columns display only var names", {
#     imp$fname <- tf
#     expect_silent(imp$setfile())
#     expect_equal(dim(imp$prev), c(rows = 50L, cols = 3L))
#     expect_false(imp$prev$is_editable(1L))
#     expect_true(imp$prev$is_editable(2L))
#     expect_false(imp$prev$is_editable(3L))
# })

# test_that("Data sets with many columns can change var types", {
#     imp$fColTypes[1] <- "categorical"
#     imp$generatePreview(NULL, reload = TRUE)
#     expect_match(
#         as.character(imp$prev$get_frame()$Values[1]),
#         "^Categories: ",
#         all = FALSE
#     )

#     imp$fColTypes[1] <- "auto"
#     imp$generatePreview(NULL, reload = TRUE)
#     expect_match(
#         as.character(imp$prev$get_frame()$Values[1]),
#         paste(1:5, collapse = " "),
#         all = FALSE
#     )
# })
# imp$cancel_button$invoke_change_handler()

# test_that("JSON files load", {
#     t <- tempfile(fileext = ".json")
#     jsonlite::write_json(iris, t)
#     imp <- iNZImportWin$new(ui)
#     on.exit(try(gWidgets2::dispose(imp$importFileWin), TRUE))

#     imp$fname <- t
#     imp$setfile()
#     expect_equal(imp$fext, "json")
#     expect_equal(dim(imp$prev), c(rows = 5L, cols = 5L))
#     expect_silent(imp$ok_button$invoke_change_handler())
#     expect_equivalent(ui$getActiveData(), iris)
#     # expect_match(
#     #     # this has path names issues
#     #     ui$rhistory$get(),
#     #     "jsonlite::fromJSON(",
#     #     fixed = TRUE,
#     #     all = FALSE
#     # )
# })
# try(ui$close(), silent = TRUE)

# test_that("All documents can be deleted, returning to landing screen", {
#     # devtools::load_all(); try(ui$close(), TRUE)
#     ui <- iNZGUI$new()
#     on.exit(ui$close())
#     ui$initializeGui()

#     ui$setDocument(iNZDocument$new(data = iris))
#     expect_equal(length(ui$iNZDocuments), 1L)

#     expect_silent(
#         ui$do_delete_dataset()
#     )
#     expect_equal(length(ui$iNZDocuments), 1L)
#     expect_equal(ui$dataViewWidget$current, "landing")
# })
