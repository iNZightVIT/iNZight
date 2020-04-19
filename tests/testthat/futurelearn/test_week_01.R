context("Week 1: Introduction")

wd <- getwd()

test_that("Exercise 1.15: Import data into iNZight", {
    ui <- iNZGUI$new()
    ui$initializeGui()
    on.exit(gWidgets2::dispose(ui$win))
    setwd(wd)

    Sys.sleep(1)

    ## Load course data files stored inside iNZight
    # File > Example data
    expect_silent(exwin <- iNZImportExampleWin$new(ui))

    # > FutureLearn
    mod <- exwin$importFileWin$children[[1]]$children[[1]]$children[[2]]
    expect_silent(svalue(mod) <- "FutureLearn")

    # > nhanes_1000
    ds <- exwin$importFileWin$children[[1]]$children[[1]]$children[[4]]
    expect_silent(svalue(ds) <- "nhanes_1000")

    # > Ok
    expect_silent(
        exwin$importFileWin$children[[1]]$children[[2]]$children[[2]]$invoke_change_handler()
    )

    # Check:
    expect_equal(ui$dataNameWidget$datName, "nhanes_1000_ex")

    ## Drag variables into Var1
    expect_silent(svalue(ui$ctrlWidget$V1box) <- "Gender")
    expect_equal(ui$plotType, "bar")
    expect_silent(svalue(ui$ctrlWidget$V1box) <- "Age")
    expect_equal(ui$plotType, "dot")

    ## Load gapminder_2008:
    expect_silent(exwin <- iNZImportExampleWin$new(ui))
    mod <- exwin$importFileWin$children[[1]]$children[[1]]$children[[2]]
    expect_silent(svalue(mod) <- "FutureLearn")
    ds <- exwin$importFileWin$children[[1]]$children[[1]]$children[[4]]
    expect_silent(svalue(ds) <- "gapminder_2008")
    expect_silent(
        exwin$importFileWin$children[[1]]$children[[2]]$children[[2]]$invoke_change_handler()
    )
    expect_equal(ui$dataNameWidget$datName, "gapminder_2008_ex")

    ## Load "olympics100m.csv"
    # Download file
    skip_if_offline()
    url <- "https://www.stat.auckland.ac.nz/~wild/data/FutureLearn/olympics100m.csv"
    f <- tempfile(fileext = ".csv")
    on.exit(unlink(f))
    download.file(url, f, quiet = TRUE)

    # File > Import Data
    expect_silent(imp <- iNZImportWin$new(ui))

    # > Browse
    imp$fname <- f
    expect_silent(imp$setfile())

    # > Import
    expect_silent(imp$okBtn$invoke_change_handler())

    # Check:
    expect_equal(
        ui$dataNameWidget$datName,
        tools::file_path_sans_ext(basename(f))
    )
    expect_equal(
        colnames(ui$getActiveData()),
        c("YEAR", "NAME", "TIME", "Country", "Gender")
    )
})
