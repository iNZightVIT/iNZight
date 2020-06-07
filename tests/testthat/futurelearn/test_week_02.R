context("Week 2: Stats boot camp")

wd <- getwd()
library(FutureLearnData)
nhanes_1000_ex <- nhanes_1000
attr(nhanes_1000_ex, "name") <- "NHANES Dataset (First 1000 rows)"

test_that("Exercise 2.5: Categorical variables", {
    ui <- iNZGUI$new()
    ui$initializeGui(nhanes_1000_ex)
    on.exit(try(ui$close(), TRUE))
    setwd(wd)
    Sys.sleep(1)

    # Drag Race3 to Variable1 - produces a bar chart
    expect_silent(ui$ctrlWidget$V1box$set_value("Race3"))
    expect_equal(ui$plotType, "bar")

    # Click Get Summary to get table of counts
    expect_silent(smry <- iNZGetSummary$new(ui))
    on.exit(try(gWidgets2::dispose(smry$win), TRUE), add = TRUE, after = FALSE)
    expect_match(
        svalue(smry$info_text),
        "Summary of the distribution of Race3:",
        all = FALSE
    )
    gWidgets2::dispose(smry$win)

    # Colour bars by Race3
    expect_silent(atp <- iNZPlotMod$new(ui))
    on.exit(try(atp$modWin$footer$children[[2]]$invoke_change_handler(), TRUE), add = TRUE, after = FALSE)
    Sys.sleep(1)

    tbl <- atp$modWin$body$children[[1]]$children[[1]]
    wi <- sapply(tbl$children,
        function(x) {
            !is.null(x) &&
                class(x) == "GComboBoxNoEntry" &&
                "Race3" %in% x$get_items()
        }
    )
    expect_silent(tbl$children[[which(wi)]]$set_value("Race3"))
    expect_equal(
        names(ui$curPlot$gen$col.args$f.cols),
        levels(nhanes_1000_ex$Race3)
    )

    # Change palette
    wi <- sapply(tbl$children,
        function(x) {
            !is.null(x) &&
                class(x) == "GComboBoxNoEntry" &&
                svalue(x) == "contrast (max 8)"
        }
    )
    expect_silent(tbl$children[[which(wi)]]$set_index(3L))

    atp$modWin$footer$children[[2]]$invoke_change_handler()

    # Reorder by frequency
    expect_silent(rwin <- iNZreorderWin$new(ui))
    on.exit(try(gWidgets2::dispose(ui$modWin), TRUE), add = TRUE, after = FALSE)
})
