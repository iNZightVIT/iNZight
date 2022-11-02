# devtools::load_all(); try(dispose(w))

test_that("Tag objects works", {
    w <- gwindow()
    on.exit(gWidgets2::dispose(w))

    g <- gvbox(container = w)
    t <- gtag("tag1", container = g)
    expect_is(t, "GTag")
})

test_that("Tags object works", {
    # devtools::load_all(); try(dispose(w), TRUE)
    w <- gwindow()
    on.exit(gWidgets2::dispose(w))

    g <- gvbox(container = w)

    tags <- gmultilabel(placeholder = "Drop items here ...", container = g)
    expect_is(tags, "GMultiLabel")

    tags$add_item("hello")
    expect_equal(length(tags$widgets), 1L)

    tags$drop_item("hello")
    expect_equal(length(tags$widgets), 0L)

    tags2 <- gmultilabel(
        c("some", "tags"),
        container = g,
        removeOnClick = TRUE,
        handler = function(h, ...) print("I changed!")
    )
    # expect_output(
    #     tags2$invoke_change_handler(),
    #     "I changed!"
    # )

    c <- gtable(names(iris), container = g)
    addDropSource(c, handler = function(h, ...) svalue(h$obj))
    addDropTarget(tags2, handler = function(h, ...) {
        h$obj$add_item(h$dropdata)
    })
})

test_that("Change handlers work", {
    w <- gwindow()
    on.exit(gWidgets2::dispose(w))
    g <- gvbox(container = w)
    t <- gmultilabel(placeholder = "Drop items here...", container = g)

    addHandlerChanged(t, function(h, ...) print("change!"))
    expect_output(t$set_items(c("three")), "change!")
})
