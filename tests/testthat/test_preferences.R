context("iNZight User Preferences")

# skip_on_cran()

# try(ui$close(), TRUE); devtools::load_all()
# ui <- iNZGUI$new()
# ui$initializeGui(census.at.school.500)
# on.exit(try(ui$close(), TRUE))

test_that("Preferences load", {
    p <- iNZPrefsWin$new(ui <- NULL)
    expect_is(p, "iNZPrefsWin")
})

test_that("Font size is respected", {
    ui <- iNZGUI$new()
    ui$initializeGui(census.at.school.500)
    on.exit(ui$close())

    ui$preferences$font.size <- 14
    ui$ctrlWidget$V1box$set_value("height")

    s <- iNZGetSummary$new(ui)
    expect_equal(s$font_size, 14)
    expect_equal(s$info_text$font_attr$size, 14)
})
