context("Data is loaded into the UI")

ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

doc <- NULL
test_that("New document is created correctly when data loaded", {
    testdata <- data.frame(A = 1:10, B = LETTERS[1:10])
    attr(testdata, "name") <- "testdata"

    expect_silent(doc <<- iNZDocument$new(data = testdata))
    expect_is(doc, "iNZDocument")
    expect_equal(doc$dataModel$dataSet, testdata)
    expect_equal(doc$dataModel$name, "testdata")
})

test_that("UI correctly displays the data", {
    expect_silent(ui$setDocument(doc, reset = TRUE))
    expect_equal(ui$getActiveData()$A, 1:10)
    expect_equal(ui$getActiveData()$B, factor(LETTERS[1:10]))
    expect_equal(attr(ui$getActiveData(), "name"), "testdata")
    expect_equal(attr(ui$getActiveData(), "code"), "")

    ## data name is being displayed
    expect_equal(ui$dataNameWidget$datName, "testdata")
    expect_equal(svalue(ui$dataNameWidget$nameLabel), "testdata")


    ## data view disabled; list view enabled
    expect_false(enabled(ui$viewSwitcherWidget$dataBtn))
    expect_true(enabled(ui$viewSwitcherWidget$listBtn))

    ## clicking list view chanes the data view
    expect_true(visible(ui$dataViewWidget$dataGp$children[[1]]))
    expect_false(visible(ui$dataViewWidget$dataGp$children[[2]]))
    
    expect_silent(ui$viewSwitcherWidget$listBtn$invoke_change_handler())
    expect_false(visible(ui$dataViewWidget$dataGp$children[[1]]))
    expect_true(visible(ui$dataViewWidget$dataGp$children[[2]]))

    expect_equal(ui$dataViewWidget$dataGp$children[[2]]$children[[1]]$get_names(),
        c("VARIABLES (n = numeric, c = categorical)"))
    expect_equal(ui$dataViewWidget$dataGp$children[[2]]$children[[1]]$get_items(),
        c("(n) A", "(c) B"))
    



    ## variable options are correct
    expect_equal(ui$ctrlWidget$V1box$get_items()[-1], c("A", "B"))
    expect_equal(ui$ctrlWidget$V2box$get_items()[-1], c("A", "B"))
    expect_equal(ui$ctrlWidget$G1box$get_items()[-1], c("A", "B"))
    expect_equal(ui$ctrlWidget$G2box$get_items()[-1], c("A", "B"))

})
