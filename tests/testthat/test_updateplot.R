context("Update Plot method and code writing")

skip_on_cran()

# devtools::load_all(); ui$close();
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(gWidgets2::dispose(ui$win))
Sys.sleep(2)

test_that("Construct call function works", {
    ui$ctrlWidget$V1box$set_value("height")
    settings <- ui$getActiveDoc()$getSettings()
    model <- ui$getActiveDoc()$getModel()
    settings$xlab <- NULL
    expect_equal(
        as.character(construct_call(settings, model, list(x = "num"))),
        "inzplot(~height, data = .dataset)"
    )

    settings$col.pt <- "blue"
    expect_equal(
        as.character(construct_call(settings, model, list(x = "num"))),
        "inzplot(~height, data = .dataset, col.pt = \"blue\")"
    )
})

test_that("Update plot call: basic dot plot", {
    expect_equal(ui$getActiveDoc()$getSettings()$x, quote(height))
})


# load_all()
# list_to_args(z)
