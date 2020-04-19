context("Data is loaded into the UI")

wd <- getwd()
ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))
setwd(wd)

doc <- NULL
test_that("New document is created correctly when data loaded", {
    testdata <- data.frame(A = 1:10, B = LETTERS[1:10], stringsAsFactors = TRUE)
    attr(testdata, "name") <- "testdata"

    expect_silent(doc <<- iNZDocument$new(data = testdata))
    expect_is(doc, "iNZDocument")
    expect_equal(doc$dataModel$dataSet, testdata)
    expect_equal(doc$dataModel$name, "testdata")
})

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
    expect_false(enabled(ui$viewSwitcherWidget$dataBtn))
    expect_true(enabled(ui$viewSwitcherWidget$listBtn))

    ## clicking list view chanes the data view
    expect_true(visible(ui$dataViewWidget$dataGp$children[[1]]))
    expect_false(visible(ui$dataViewWidget$dataGp$children[[2]]))

    expect_silent(ui$viewSwitcherWidget$listBtn$invoke_change_handler())
    expect_false(visible(ui$dataViewWidget$dataGp$children[[1]]))
    expect_true(visible(ui$dataViewWidget$dataGp$children[[2]]))

    expect_equal(ui$dataViewWidget$dataGp$children[[2]]$children[[1]]$get_names(),
        c("VARIABLES (n = numeric, c = categorical, dt = date/time)"))
    expect_equal(ui$dataViewWidget$dataGp$children[[2]]$children[[1]]$get_items(),
        c("(n) A", "(c) B"))




    ## variable options are correct
    expect_equal(ui$ctrlWidget$V1box$get_items()[-1], c("A", "B"))
    expect_equal(ui$ctrlWidget$V2box$get_items()[-1], c("A", "B"))
    expect_equal(ui$ctrlWidget$G1box$get_items()[-1], c("A", "B"))
    expect_equal(ui$ctrlWidget$G2box$get_items()[-1], c("A", "B"))

})

test_that("Example data menus work correctly", {
    exwin <- iNZImportExampleWin$new(ui)
    mod <- exwin$importFileWin$children[[1]]$children[[1]]$children[[2]]
    expect_equal(svalue(mod), "Default")
    # expect_equal(mod$get_items(),
    #     c("Default", "Multiple Response", "Time Series",
    #       "Maps", "Survey", "FutureLearn")
    # )
    ds <- exwin$importFileWin$children[[1]]$children[[1]]$children[[4]]
    expect_equal(svalue(ds), character())
    expect_equal(length(ds$get_items()), 3)

    # set a package
    expect_silent(svalue(mod) <- "Time Series")
    expect_equal(ds$get_items(),
        c("seaice", "visitorsA2", "visitorsM2", "visitorsQ")
    )
    dn <- exwin$importFileWin$children[[1]]$children[[1]]$children[[6]]
    expect_equal(svalue(dn), "")

    # choose data
    expect_silent(svalue(ds) <- "seaice")
    expect_equal(svalue(dn), "Sea Ice")

    # choose another package resets fields
    expect_silent(svalue(mod) <- "Default")
    expect_equal(svalue(ds, TRUE), 0)
    expect_equal(svalue(dn), "")

    expect_silent(svalue(ds) <- "census.at.school.500")
    # expect_equal(svalue(dn), "Census at School 500")

    # load it
    expect_silent(
        exwin$importFileWin$children[[1]]$children[[2]]$children[[2]]$invoke_change_handler()
    )
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
    expect_silent(imp$okBtn$invoke_change_handler())
    expect_equal(
        names(ui$getActiveData()),
        c("cellsource", "rightfoot", "travel", "getlunch", "height",
            "gender", "age", "year", "armspan", "cellcost")
    )
    expect_equal(
        dim(ui$getActiveData()),
        c(5, 10)
    )
})

test_that("SAS (.sas7bdat) files load", {
    imp <- iNZImportWin$new(ui)
    imp$fname <- "test.sas7bdat"
    imp$setfile()
    skip_if(length(imp$prevGp$children) == 1,
        message = "Preview did not load."
    )
    expect_is(imp$prevGp$children[[2]], "GDf")
    expect_equal(imp$prevGp$children[[2]]$get_dim(), c(rows = 5, cols = 7))
    imp$okBtn$invoke_change_handler()
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
    imp$okBtn$invoke_change_handler()
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

test_that("Switching variable types works (csv)", {
    imp <- iNZImportWin$new(ui)
    imp$fname <- "cas5.csv"
    imp$setfile()

    skip_if(length(imp$prevGp$children) == 1,
        message = "Preview did not load."
    )
    # convert YEAR to cat
    expect_equal(
        imp$prev$get_names(),
        c(
            "cellsource (c)", "rightfoot (n)", "travel (c)",
            "getlunch (c)", "height (n)", "gender (c)",
            "age (n)", "year (n)", "armspan (n)", "cellcost (n)"
        )
    )
    imp$fColTypes[8] <- "categorical"
    imp$generatePreview(NULL)
    expect_equal(
        imp$prev$get_names(),
        c(
            "cellsource (c)", "rightfoot (n)", "travel (c)",
            "getlunch (c)", "height (n)", "gender (c)",
            "age (n)", "year (c)", "armspan (n)", "cellcost (n)"
        )
    )

    imp$okBtn$invoke_change_handler()
    expect_is(ui$getActiveData()$year, "factor")
})

test_that("Date times are supported (csv)", {
    imp <- iNZImportWin$new(ui)
    imp$fname <- "dt.csv"
    imp$setfile()
    skip_if(length(imp$prevGp$children) == 1,
        message = "Preview did not load."
    )

    expect_equal(
        imp$prev$get_names(),
        c("x (d)", "y (t)", "z (dt)")
    )
    imp$okBtn$invoke_change_handler()
    expect_is(ui$getActiveData()$x, "Date")
    expect_is(ui$getActiveData()$y, "hms")
    expect_is(ui$getActiveData()$z, "POSIXct")
})

test_that("Changing file resets column types", {
    imp <- iNZImportWin$new(ui)
    imp$fname <- "dt.csv"
    imp$fColTypes <- c("numeric", "numeric", "auto")
    expect_silent(imp$setfile())
    expect_true(all(imp$fColTypes == "auto"))
})

test_that("RData files display list of objects", {
    save(census.at.school.500, iris, file = "test.rda")
    on.exit(unlink("test.rda"))
    imp <- iNZImportWin$new(ui)
    imp$fname <- "test.rda"
    expect_silent(imp$setfile())
    expect_equal(svalue(imp$rdaName), "iris")
    expect_equal(imp$rdaName$get_items(), c("iris", "census.at.school.500"))
    expect_silent(svalue(imp$rdaName, index = TRUE) <- 2)
    imp$okBtn$invoke_change_handler()
})



# try(ui$close());
# ui <- iNZGUI$new()
# ui$initializeGui()
# on.exit(gWidgets2::dispose(ui$win))

test_that("Excel files load and display available sheets", {
    imp <- iNZImportWin$new(ui)
    imp$fname <- "sheet.xlsx"
    expect_silent(imp$setfile())
    expect_equal(imp$rdaName$get_items(), c("Africa", "Americas", "Asia", "Europe", "Oceania"))
    expect_equal(svalue(imp$rdaName), "Africa")
    expect_silent(svalue(imp$rdaName, index = TRUE) <- 3)
    expect_silent(imp$okBtn$invoke_change_handler())
    expect_true(all(as.character(ui$getActiveData()$continent) == "Asia"))
})


# try(ui$close()); load_all()
# ui <- iNZGUI$new()
# ui$initializeGui()
# on.exit(gWidgets2::dispose(ui$win))

test_that("User can choose to load a URL", {
    imp <- iNZImportWin$new(ui)
    svalue(imp$loadURL) <- TRUE
    svalue(imp$fileurl) <- "https://raw.githubusercontent.com/iNZightVIT/iNZight/dev/tests/testthat/cas5.csv"
    expect_equal(imp$fext, "csv")
    expect_silent(imp$okBtn$invoke_change_handler())
    expect_equivalent(
        ui$getActiveData(),
        iNZightTools::smart_read("cas5.csv")
    )
})
