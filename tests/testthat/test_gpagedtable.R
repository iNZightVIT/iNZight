skip("in development")

# devtools::load_all()
try(dispose(w))

test_that("Paged table has correct class", {
    w <- gwindow()
    g <- gvbox(container = w, fill = TRUE, expand = TRUE)
    t <- gpagedtable(census.at.school.500,
        container = g, fill = TRUE, expand = TRUE
    )
    expect_equal(as.character(class(t)), "GPagedTable")
})
