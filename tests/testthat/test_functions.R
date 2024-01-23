context("Help functions for iNZight")


test_that("URLs for help pages generated correctly", {
    skip_on_cran()
    expect_silent(help_page("user_guides"))
})

test_that("Not in operator works", {
    expect_false("hello" %notin% c("hello", "world"))
    expect_true("hello" %notin% c("world", "goodbye"))
})
