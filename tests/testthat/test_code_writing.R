context("Code writing")

# try(ui$close())
ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

test_that("Data set code is applied", {
    cas <- census.at.school.500
    attr(cas, "code") <- "read_csv('cas.csv')"
    ui$setDocument(iNZDocument$new(data = cas), reset = TRUE)

    expect_match(
        gsub("\\s\\s+", " ", paste(ui$rhistory$get(), collapse = " ")),
        "data <- read_csv('cas.csv')",
        fixed = TRUE
    )
})

test_that("magrittr library call is included", {
    expect_match(
        gsub("\\s\\s+", " ", paste(ui$rhistory$get(), collapse = " ")),
        "library(magrittr)",
        fixed = TRUE
    )
})
