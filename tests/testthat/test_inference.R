context("Get Inference window")

# ui$close()
# devtools::load_all("../..")
ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

ui$setDocument(iNZDocument$new(data = census.at.school.500), reset = TRUE)
Sys.sleep(5)

test_that("Get inference window - dot plots", {
    svalue(ui$ctrlWidget$V1box) <- "height"

    iwin <- ui$infBtn$invoke_change_handler()
    expect_is(iwin[[1]], "GWindow")
    chk <- iwin[[1]]$children[[1]]$children[[1]]$children[[4]]
    expect_equal(
        chk$widget$label,
        "One Sample t-test"
    )
    # checking this box specifies hypothesis
    null <- iwin[[1]]$children[[1]]$children[[1]]$children[[6]]
    alt <- iwin[[1]]$children[[1]]$children[[1]]$children[[8]]

    expect_false(enabled(null))
    expect_false(enabled(alt))

    expect_silent(
        svalue(chk) <- TRUE
    )
    expect_true(enabled(null))
    expect_true(enabled(alt))
    expect_is(null, "GEdit")
    expect_is(alt, "GComboBox")
    expect_equal(svalue(null), "0")
    expect_equal(svalue(alt), "two sided")

    svalue(null) <- "100"
    svalue(alt) <- "greater than"

    expect_silent(
        w2 <- iwin[[1]]$children[[1]]$children[[2]]$invoke_change_handler()
    )
    expect_is(w2[[1]], "GWindow")
    expect_is(w2[[1]]$children[[1]], "GText")
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Null Hypothesis: true mean is equal to 100"
    )
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Alternative Hypothesis: true mean is greater than 100"
    )
    dispose(w2[[1]])
})


test_that("Get inference window - two-way dot plots", {
    svalue(ui$ctrlWidget$V2box) <- "gender"

    iwin <- ui$infBtn$invoke_change_handler()
    expect_is(iwin[[1]], "GWindow")
    rdo <- iwin[[1]]$children[[1]]$children[[1]]$children[[4]]
    expect_is(rdo, "GRadio")
    expect_equal(svalue(rdo), "None")
    # checking this box specifies hypothesis
    null <- iwin[[1]]$children[[1]]$children[[1]]$children[[6]]
    alt <- iwin[[1]]$children[[1]]$children[[1]]$children[[8]]

    expect_false(enabled(null))
    expect_false(enabled(alt))

    expect_silent(svalue(rdo, index = TRUE) <- 2)
    expect_equal(svalue(rdo), "Two Sample t-test")

    expect_true(enabled(null))
    expect_true(enabled(alt))
    expect_is(null, "GEdit")
    expect_is(alt, "GComboBox")
    expect_equal(svalue(null), "0")
    expect_equal(svalue(alt), "two sided")

    svalue(null) <- "5"
    svalue(alt) <- "less than"

    expect_silent(
        w2 <- iwin[[1]]$children[[1]]$children[[2]]$invoke_change_handler()
    )
    expect_is(w2[[1]], "GWindow")
    expect_is(w2[[1]]$children[[1]], "GText")
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Null Hypothesis: true difference in means is equal to 5"
    )
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Alternative Hypothesis: true difference in means is less than 5"
    )
    dispose(w2[[1]])
})

test_that("Get inference window - ANOVA dot plots", {
    svalue(ui$ctrlWidget$V2box) <- "getlunch"

    iwin <- ui$infBtn$invoke_change_handler()
    expect_is(iwin[[1]], "GWindow")
    chk <- iwin[[1]]$children[[1]]$children[[1]]$children[[4]]
    expect_equal(
        chk$widget$label,
        "ANOVA"
    )
    expect_silent(svalue(chk) <- TRUE)

    expect_silent(
        w2 <- iwin[[1]]$children[[1]]$children[[2]]$invoke_change_handler()
    )
    expect_is(w2[[1]], "GWindow")
    expect_is(w2[[1]]$children[[1]], "GText")
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Null Hypothesis: true group means are equal"
    )
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Alternative Hypothesis: true group means are not equal"
    )
    dispose(w2[[1]])
})

test_that("Get inference window - one way bar plots", {
    svalue(ui$ctrlWidget$V2box, index = TRUE) <- 1
    svalue(ui$ctrlWidget$V1box) <- "getlunch"

    iwin <- ui$infBtn$invoke_change_handler()
    expect_is(iwin[[1]], "GWindow")
    chk <- iwin[[1]]$children[[1]]$children[[1]]$children[[4]]
    expect_equal(
        chk$widget$label,
        "Chi-square test"
    )
    expect_silent(svalue(chk) <- TRUE)

    expect_silent(
        w2 <- iwin[[1]]$children[[1]]$children[[2]]$invoke_change_handler()
    )
    expect_is(w2[[1]], "GWindow")
    expect_is(w2[[1]]$children[[1]], "GText")
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Null Hypothesis: true proportions in each category are equal"
    )
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Alternative Hypothesis: true proportions in each category are not equal"
    )
    dispose(w2[[1]])
})

test_that("Get inference window - two way bar plots", {
    svalue(ui$ctrlWidget$V2box) <- "gender"

    iwin <- ui$infBtn$invoke_change_handler()
    expect_is(iwin[[1]], "GWindow")
    chk <- iwin[[1]]$children[[1]]$children[[1]]$children[[4]]
    expect_equal(
        chk$widget$label,
        "Chi-square test"
    )
    expect_silent(svalue(chk) <- TRUE)

    expect_silent(
        w2 <- iwin[[1]]$children[[1]]$children[[2]]$invoke_change_handler()
    )
    expect_is(w2[[1]], "GWindow")
    expect_is(w2[[1]]$children[[1]], "GText")
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Null Hypothesis: distribution of getlunch does not depend on gender"
    )
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Alternative Hypothesis: distribution of getlunch changes with gender"
    )
    dispose(w2[[1]])
})


test_that("Get inference window - scatter plots", {
    svalue(ui$ctrlWidget$V1box) <- "height"
    svalue(ui$ctrlWidget$V2box) <- "armspan"

    iwin <- ui$infBtn$invoke_change_handler()
    expect_is(iwin[[1]], "GWindow")
    chk <- iwin[[1]]$children[[1]]$children[[1]]$children[[4]]
    expect_equal(
        chk$widget$label,
        "Linear trend"
    )
    expect_silent(svalue(chk) <- TRUE)

    expect_silent(
        w2 <- iwin[[1]]$children[[1]]$children[[2]]$invoke_change_handler()
    )
    expect_is(w2[[1]], "GWindow")
    expect_is(w2[[1]]$children[[1]], "GText")
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "p-values for the null hypothesis of no association"
    )
    dispose(w2[[1]])
})

## Survey designs
expect_true(enabled(ui$infBtn))

data(api, package = "survey")
ui$setDocument(iNZDocument$new(data = apiclus2), reset = TRUE)
Sys.sleep(5)


test_that("Get inference disabled for surveys", {
    swin <- iNZSurveyDesign$new(ui, warn = FALSE)
    svalue(swin$clus1Var) <- "dnum"
    svalue(swin$clus2Var) <- "snum"
    svalue(swin$fpcVar) <- "fpc1 + fpc2"
    swin$createBtn$invoke_change_handler()
    expect_false(enabled(ui$infBtn))
})

test_that("Get inference reenabled for non-surveys", {
    ui$removeDesign()
    expect_true(enabled(ui$infBtn))
})
