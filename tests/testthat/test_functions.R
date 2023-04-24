context("Help functions for iNZight")


test_that("URLs for help pages generated correctly", {
    skip_on_cran()
    expect_silent(help_page("user_guides"))
})
