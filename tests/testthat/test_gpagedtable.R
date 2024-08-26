test_that("Paged table has correct class", {
    w <- gwindow()
    on.exit(dispose(w))
    g <- gvbox(container = w, fill = TRUE, expand = TRUE)
    t <- gpagedtable(census.at.school.500,
        container = g, fill = TRUE, expand = TRUE
    )
    expect_equal(as.character(class(t)), "GPagedTable")
})

test_that("Paging table working at boundaries", {
    w <- gwindow()
    on.exit(dispose(w))
    g <- gvbox(container = w, fill = TRUE, expand = TRUE)
    t <- gpagedtable(census.at.school.500,
        container = g, fill = TRUE, expand = TRUE
    )

    expect_equal(t$pager$page, 1L)
    expect_silent(t$incrementPage(1))
    expect_equal(t$pager$page, 2L)
    expect_silent(t$incrementPage(1))
    expect_equal(t$pager$page, 2L)
    expect_silent(t$incrementPage(-1))
    expect_equal(t$pager$page, 1L)
    expect_silent(t$incrementPage(-1))
    expect_equal(t$pager$page, 1L)

    expect_warning(t$incrementPage(2))
})
