context("The user interface loads")

ui <- NULL

test_that("GUI is loaded and initialized without problems", {
    ## load (and then close) the ui object
    ui <<- iNZGUI$new()
    expect_is(ui, "iNZGUI")

    ## at the end of initialisation, 0 is returned invisibly
    expect_equal(ui$initializeGui(), 0)

    ## the new initialized UI window should have several important objects ...
    expect_equal(
        ui$getActiveData(),
        data.frame(empty = " ", stringsAsFactors = TRUE)
    )
    expect_equal(length(ui$iNZDocuments), 1)
    expect_is(ui$iNZDocuments[[1]], "iNZDocument")
})

test_that("Primary UI widgets are loaded and displaying correctly", {
    ## data name is displayed as not loaded
    expect_equal(ui$dataNameWidget$datName, "No data loaded")

    ## control widget (drag/drop boxes)
    expect_is(ui$ctrlWidget, "iNZControlWidget")
    expect_equal(svalue(ui$ctrlWidget$V1box), "Select/Drag-drop Variable 1")
    expect_equal(svalue(ui$ctrlWidget$V2box), "Select/Drag-drop Variable 2")
    expect_equal(svalue(ui$ctrlWidget$G1box), "Select/Drag-drop Variable 3 (subset)")
    expect_equal(svalue(ui$ctrlWidget$G2box), "Select/Drag-drop Variable 4 (subset)")
})

# ui <<- iNZGUI$new(); ui$initializeGui()
test_that("Data view loads", {
    expect_silent(ui$setDocument(iNZDocument$new(data = iris)))
    expect_equal(ui$dataNameWidget$datName, "data")
    df <- ui$dataViewWidget$dfView$children[[1]]
    expect_is(df, "GDf")
    expect_equal(df$get_dim(), c(rows = 150, cols = 5))
})

test_that("UI closes quietly", {
    expect_silent(ui$close())
})

# load_all(); ui$close(); ui <- iNZGUI$new()

test_that("Variable list can be searched", {
    ui$initializeGui(gapminder)
    ui$dataViewWidget$listView()
    expect_true(visible(ui$dataViewWidget$varView))

    svalue(ui$dataViewWidget$searchBox) <- "pop"
    expect_equal(
        ui$dataViewWidget$varWidget$get_items(),
        names(gapminder)[grepl("pop", names(gapminder), ignore.case = TRUE)]
    )
})
