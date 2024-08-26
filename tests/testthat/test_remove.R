context("Add to Plot window")

skip_on_cran()

# try(ui$close(), TRUE); load_all()
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(gWidgets2::dispose(ui$win))
Sys.sleep(5)

test_that("Features listed for scatter plots", {
    ui$ctrlWidget$V1box$set_value("height")
    ui$ctrlWidget$V2box$set_value("armspan")
    ui$getActiveDoc()$setSettings(
        list(
            colby = as.name("gender"),
            sizeby = as.name("year"),
            jitter = "xy",
            rugs = "x",
            trend = "linear",
            smooth = 0.7,
            join = TRUE,
            lines.by = FALSE,
            LOE = TRUE
        )
    )

    rmv <- iNZPlotRmveModWin$new(ui)
    on.exit(rmv$modWin$footer$children[[2]]$invoke_change_handler())
    expect_equal(length(rmv$g_cur$children), 8)
    expect_equal(svalue(rmv$rmvBtn), "Remove all")

    rmv$g_cur$children[[8]]$set_value(TRUE)
    expect_equal(svalue(rmv$rmvBtn), "Remove selected")
    expect_silent(rmv$rmvBtn$invoke_change_handler())

    rmv <- iNZPlotRmveModWin$new(ui)
    expect_equal(length(rmv$g_cur$children), 7)

    rmv$g_cur$children[[4]]$set_value(TRUE)
    rmv$g_cur$children[[6]]$set_value(TRUE)
    rmv$g_cur$children[[7]]$set_value(TRUE)
    expect_silent(rmv$rmvBtn$invoke_change_handler())

    rmv <- iNZPlotRmveModWin$new(ui)
    expect_equal(length(rmv$g_cur$children), 4)

    expect_silent(rmv$remove_additions(confirm = TRUE))
    rmv <- iNZPlotRmveModWin$new(ui)
    expect_equal(rmv$g_cur$children, list())
    expect_equal(svalue(rmv$modWin$body$children[[1]]), "No additions to remove")
})
