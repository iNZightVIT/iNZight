# devtools::load_all(); try(dispose(w))

test_that("Tag objects works", {
    w <- gwindow()
    on.exit(gWidgets2::dispose(w))

    g <- gvbox(container = w)
    t <- gtag("tag1", container = g)
    expect_is(t, "GTag")
})

test_that("Tags object works", {
    w <- gwindow()
    on.exit(gWidgets2::dispose(w))

    g <- gvbox(container = w)
    tags <- gtags(placeholder = "Drop items here ...", container = g)
    expect_is(tags, "GTags")

    tags$add_tag("hello")
    expect_equal(length(tags$children), 2L)

    tags$drop_tag("hello")
    expect_equal(length(tags$children), 1L)
})
