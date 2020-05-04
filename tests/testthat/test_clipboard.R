context("Import/export clipboard data")

# try(ui$close()); load_all()
ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

demo <- "x\ty\tz
10\t3\ta
20\t45\tb"

test_that("Data can be loaded from the 'clipboard'", {
    cpy <- iNZClipboard$new(ui, "paste")
    cpy$textIn$set_value(demo)
    expect_silent(cpy$parseData())
    expect_equivalent(
        cpy$dataOut$get_items(),
        iNZightTools::read_text(demo, "\t")
    )
    expect_silent(cpy$okBtn$invoke_change_handler())
    expect_equivalent(
        ui$getActiveData(),
        iNZightTools::read_text(demo, "\t")
    )
})
