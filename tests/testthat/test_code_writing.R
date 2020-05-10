context("Code writing")

# try(ui$close())
ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))
Sys.sleep(2)

test_that("Data set code is applied", {
    cas <- census.at.school.500
    attr(cas, "code") <- "read_csv('cas.csv')"
    ui$setDocument(iNZDocument$new(data = cas), reset = TRUE)
    Sys.sleep(2)

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

test_that("Plot code is generated correctly", {
    svalue(ui$ctrlWidget$V1box) <- "height"
    expect_equal(
        attr(ui$curPlot, "code"),
        "iNZPlot(height, data = data)"
    )

    svalue(ui$ctrlWidget$V2box) <- "travel"
    expect_equal(
        attr(ui$curPlot, "code"),
        "iNZPlot(height ~ travel, data = data)"
    )

    svalue(ui$ctrlWidget$G1box) <- "gender"
    expect_equal(
        attr(ui$curPlot, "code"),
        "iNZPlot(height ~ travel | gender, data = data)"
    )

    svalue(ui$ctrlWidget$G2box) <- "age"
    expect_equal(
        attr(ui$curPlot, "code"),
        "iNZPlot(height ~ travel | gender, data = data)"
    )

    sld <- ui$ctrlWidget$ctrlGp$children[[1]]$children[[15]]
    sld$set_index(2L)
    expect_equal(
        attr(ui$curPlot, "code"),
        "iNZPlot(height ~ travel | gender + age, g2.level = \"[7 - 11]\", data = data)"
    )


    svalue(ui$ctrlWidget$V2box, TRUE) <- 1L
    expect_equal(
        attr(ui$curPlot, "code"),
        "iNZPlot(height ~ . | gender + age, g2.level = \"[7 - 11]\", data = data)"
    )

    svalue(ui$ctrlWidget$G1box, TRUE) <- 1L
    expect_equal(
        attr(ui$curPlot, "code"),
        "iNZPlot(height ~ . | age, data = data, g1.level = \"[7 - 11]\")"
    )

    sld$set_index(1L)
    expect_equal(
        attr(ui$curPlot, "code"),
        "iNZPlot(height, data = data)"
    )

    svalue(ui$ctrlWidget$G2box, TRUE) <- 1L
    svalue(ui$ctrlWidget$G1box) <- "gender"
    expect_equal(
        attr(ui$curPlot, "code"),
        "iNZPlot(height ~ . | gender, data = data)"
    )
})
