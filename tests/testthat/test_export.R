context("Data is exported from the UI")

# skip_on_cran()

# try(ui$close())
# ui <- iNZGUI$new()
# ui$initializeGui(census.at.school.500)
# on.exit(gWidgets2::dispose(ui$win))

test_that("Export RDS", {
    fp <- tempfile(fileext = ".rds")
    on.exit(unlink(fp))
    ui <- list(OS = "windows", getActiveData = function() census.at.school.500)

    w <- iNZExportWin$new(ui)
    expect_is(w, "iNZExportWin")
    w$file$set_value(fp)
    expect_equal(w$ftype$get_index(), 3L)
    expect_silent(w$ok_button$invoke_change_handler())

    expect_equal(readRDS(fp), census.at.school.500)
})

test_that("Export TXT", {
    fp <- tempfile(fileext = ".txt")
    on.exit(unlink(fp))
    ui <- list(OS = "windows", getActiveData = function() census.at.school.500)

    w <- iNZExportWin$new(ui)
    expect_is(w, "iNZExportWin")
    w$file$set_value(fp)
    expect_equal(w$ftype$get_index(), 2L)
    expect_silent(w$ok_button$invoke_change_handler())

    expect_equal(
        dim(readr::read_delim(fp, show_col_types = FALSE)),
        dim(census.at.school.500)
    )
})

if (FALSE) {
    ## Run manually (cannot be automated at this point in time)
    svalue(ui$ctrlWidget$V1box) <- "height"
    svalue(ui$ctrlWidget$V2box) <- "travel"
    ui$getActiveDoc()$setSettings(
        list(
            colby = census.at.school.500$gender,
            varnames = list(colby = "gender")
        )
    )
    ui$plotWidget$savePlot()
}
