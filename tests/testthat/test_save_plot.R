context("Save plot")

# try(ui$close(), TRUE); load_all()
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(gWidgets2::dispose(ui$win))
Sys.sleep(5)

test_that("Interactive graphs can be saved 100% locally", {
    ui$ctrlWidget$V1box$set_value("height")
    ui$ctrlWidget$V2box$set_value("armspan")

    w <- ui$plotWidget$savePlot()
    expect_true(w$children[[1]]$children[[1]]$children[[4]]$set_value(tempdir()))
    expect_true(w$children[[1]]$children[[1]]$children[[7]]$set_value("test"))
    on.exit(unlink(file.path(tempdir(), "test.html")))
    on.exit(unlink(file.path(tempdir(), "assets"), TRUE, TRUE), add = TRUE)

    gg <- w$children[[1]]$children[[2]]
    expect_false(visible(gg))
    w$children[[1]]$children[[1]]$children[[2]]$set_index(7L)
    expect_true(visible(gg))

    gg$children[[4]]$set_value(TRUE)

    op <- options(viewer = print.default)
    on.exit(options(op), add = TRUE)
    expect_output(
        w$children[[1]]$children[[4]]$children[[2]]$invoke_change_handler(),
        file.path(tempdir(), "test.html")
    )
    expect_true(dir.exists(file.path(tempdir(), "assets")))
})
