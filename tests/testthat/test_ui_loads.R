context("The user interface loads")

ui <- NULL
on.exit(gWidgets2::dispose(ui$win))

test_that("GUI is loaded and initialized without problems", {
    cat("\n1 ...\n")
    ## load (and then close) the ui object
    ui <<- iNZGUI$new()
    expect_is(ui, "iNZGUI")

    ## at the end of initialisation, 0 is returned invisibly
    expect_equal(ui$initializeGui(), 0)

    ## the new initialized UI window should have several important objects ...
    expect_equivalent(
        ui$getActiveData(),
        structure(
            data.frame(empty = " ", stringsAsFactors = TRUE),
            name = "(empty)"
        )
    )
    expect_equal(length(ui$iNZDocuments), 1)
    expect_is(ui$iNZDocuments[[1]], "iNZDocument")
})

test_that("Primary UI widgets are loaded and displaying correctly", {
    cat("\n2 ...\n")
    ## "Load Data" button displayed instead of dataset selection
    expect_equal(
        ui$dataNameWidget$widget$children[[2]]$get_value(),
        "Import data ..."
    )
    expect_equal(length(ui$dataNameWidget$widget$children), 2L)

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
    cat("\n3 ...\n")
    expect_silent(ui$setDocument(iNZDocument$new(data = iris)))
    expect_equal(
        ui$dataNameWidget$widget$children[[2]]$get_value(),
        "iris"
    )
    expect_equal(length(ui$dataNameWidget$widget$children), 2L)
    expect_equal(ui$dataNameWidget$datName, "iris")
    df <- ui$dataViewWidget$dfView$children[[1]]
    expect_is(df, "GDf")
    expect_equal(
        df$get_dim(),
        c(rows = ui$dataViewWidget$paginate$nrow, cols = 5)
    )
    expect_false(enabled(ui$dataToolbarWidget$dataBtn))
    expect_true(enabled(ui$dataToolbarWidget$listBtn))
    cat("\n3 - complete ...\n")
})

test_that("UI closes quietly", {
    cat("\n4 ...\n")
    expect_silent(ui$close())
    cat("\n4 - complete ...\n")
})

# load_all(); ui$close(); ui <- iNZGUI$new()

test_that("Variable list can be searched", {
    cat("\n5 ...\n")
    ui <<- iNZGUI$new()
    ui$initializeGui(gapminder)
    on.exit(ui$close())
    expect_true(enabled(ui$dataToolbarWidget$listBtn))
    ui$dataViewWidget$listView()
    expect_true(enabled(ui$dataToolbarWidget$dataBtn))
    expect_false(enabled(ui$dataToolbarWidget$listBtn))
    expect_equal(ui$dataViewWidget$current, "variables")

    svalue(ui$dataViewWidget$searchBox) <- "pop"
    expect_equal(
        ui$dataViewWidget$varWidget$get_items()$Name,
        names(gapminder)[grepl("pop", names(gapminder), ignore.case = TRUE)]
    )
    cat("\n5 - complete ...\n")
})

# Switching doesn't work on checks
# test_that("Pop-out mode works OK", {
#     ui$preferences$popout <- TRUE
#     expect_silent(ui$savePreferences())
#     on.exit({
#         ui$preferences$popout <- FALSE
#         ui$savePreferences()
#     })
#     ui$reload()
#     Sys.sleep(10)

#     expect_true(ui$popOut)
# })

test_that("Data view is enabled after changing data", {
    cat("\n6 ...\n")
    ui <<- iNZGUI$new()
    ui$initializeGui(census.at.school.500)
    ui$dataViewWidget$listView()
    cat("\n6.1 ...\n")
    expect_true(enabled(ui$dataToolbarWidget$dataBtn))
    cat("\n6.2 ...\n")
    expect_false(enabled(ui$dataToolbarWidget$listBtn))
    cat("\n6.3 ...\n")
    ui$new_document(census.at.school.500[1:100, ], "subset")
    cat("\n6.4 ...\n")
    expect_true(enabled(ui$dataToolbarWidget$dataBtn))
    cat("\n6.5 ...\n")
    expect_false(enabled(ui$dataToolbarWidget$listBtn))
    cat("\n fin ...\n")
})
