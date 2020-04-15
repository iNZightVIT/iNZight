context("Add-on Modules")

moduledir <- file.path(getwd(), "modules")
# load_all("../../../iNZightModules")
# load_all()

# try(ui$close())
ui <- iNZGUI$new()
ui$initializeGui(iris, addonDir = moduledir)

test_that("Modules loaded into menu", {
    advmenu <- svalue(ui$menuBarWidget$menubar)$Advanced
    expect_true(all(
        c("DemoModule", "DemoModule2", "DemoModule3") %in% names(advmenu)
    ))
})

test_that("Modules launch and close", {
    advmenu <- svalue(ui$menuBarWidget$menubar)$Advanced
    DM <- advmenu[["DemoModule"]]
    expect_equal(DM$widget$label, "Demo Module")
    expect_output(
        modwin <- DM$handler_id$o$o(NULL),
        "Running new module"
    )
    expect_is(modwin, "Demo Module")
    expect_equivalent(modwin$get_data(), iris)
    expect_output(
        modwin$close(),
        "Closing module"
    )
})

test_that("Add/remove modules window", {
    addrm <- svalue(ui$menuBarWidget$menubar)$Advanced$install
    expect_is(addrm, "GAction")
    ## more tests later
})

ui$close()
