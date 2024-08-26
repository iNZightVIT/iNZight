context("Save plot")

skip_on_ci()
skip_on_covr()
skip_on_cran()

# try(ui$close(), TRUE); load_all()
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(gWidgets2::dispose(ui$win))
Sys.sleep(5)

skip_if_not_installed("knitr")
skip_if_not_installed("gridSVG")
skip_if_not_installed("jsonlite")

test_that("Interactive graphs can be saved 100% locally", {
    ui$ctrlWidget$V1box$set_value("height")
    ui$ctrlWidget$V2box$set_value("armspan")

    td <- tempdir()

    w <- ui$plotWidget$savePlot()
    expect_true(w$children[[1]]$children[[1]]$children[[4]]$set_value(td))
    expect_true(w$children[[1]]$children[[1]]$children[[7]]$set_value("test"))
    on.exit(unlink(file.path(td, "test.html")))
    on.exit(unlink(file.path(td, "assets"), TRUE, TRUE), add = TRUE)

    Sys.sleep(0.1)

    gg <- w$children[[1]]$children[[2]]
    expect_false(visible(gg))
    w$children[[1]]$children[[1]]$children[[2]]$set_index(7L)
    Sys.sleep(0.1)
    expect_true(visible(gg))

    gg$children[[4]]$set_value(TRUE)

    w$children[[1]]$children[[4]]$children[[2]]$invoke_change_handler()
    expect_true(dir.exists(file.path(td, "assets")))
})
