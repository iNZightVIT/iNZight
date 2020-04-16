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
        "Null Hypothesis: true group means are all equal"
    )
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Alternative Hypothesis: true group means are not all equal"
    )
    dispose(w2[[1]])
})

test_that("Get inference window - one way bar plots, two levels", {
    svalue(ui$ctrlWidget$V2box, index = TRUE) <- 1
    svalue(ui$ctrlWidget$V1box) <- "gender"

    iwin <- ui$infBtn$invoke_change_handler()
    expect_is(iwin[[1]], "GWindow")
    chk <- iwin[[1]]$children[[1]]$children[[1]]$children[[4]]
    null <- iwin[[1]]$children[[1]]$children[[1]]$children[[6]]
    alt <- iwin[[1]]$children[[1]]$children[[1]]$children[[8]]
    simp <- iwin[[1]]$children[[1]]$children[[1]]$children[[9]]
    expect_equal(svalue(chk), "None")
    expect_equal(svalue(null), "0.5")
    expect_equal(svalue(alt), "two sided")
    expect_false(svalue(simp))
    expect_false(enabled(null))
    expect_false(enabled(alt))
    expect_false(enabled(simp))

    expect_silent(svalue(chk, index = TRUE) <- 2)
    expect_false(enabled(null))
    expect_false(enabled(alt))
    expect_true(enabled(simp))

    expect_silent(svalue(chk, index = TRUE) <- 3)
    expect_true(enabled(null))
    expect_true(enabled(alt))
    expect_false(enabled(simp))

    # expect_silent(svalue(simp) <- TRUE)

    expect_silent(
        w2 <- iwin[[1]]$children[[1]]$children[[2]]$invoke_change_handler()
    )
    expect_is(w2[[1]], "GWindow")
    expect_is(w2[[1]]$children[[1]], "GText")
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Null Hypothesis: true proportion of gender = female is 0.5"
    )
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Alternative Hypothesis: true proportion of gender = female is not equal to 0.5"
    )
    dispose(w2[[1]])
})

test_that("Get inference window - one way bar plots", {
    svalue(ui$ctrlWidget$V2box, index = TRUE) <- 1
    svalue(ui$ctrlWidget$V1box) <- "getlunch"

    iwin <- ui$infBtn$invoke_change_handler()
    expect_is(iwin[[1]], "GWindow")
    chk <- iwin[[1]]$children[[1]]$children[[1]]$children[[4]]
    null <- iwin[[1]]$children[[1]]$children[[1]]$children[[5]]
    expect_equal(
        chk$widget$label,
        "Chi-square test"
    )
    expect_false(enabled(null) && svalue(null))
    expect_silent(svalue(chk) <- TRUE)
    expect_true(enabled(null))
    expect_silent(svalue(null) <- TRUE)

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
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Simulated p-value"
    )
    dispose(w2[[1]])
})

test_that("Get inference window - two way bar plots", {
    svalue(ui$ctrlWidget$V2box) <- "gender"

    iwin <- ui$infBtn$invoke_change_handler()
    expect_is(iwin[[1]], "GWindow")
    chk <- iwin[[1]]$children[[1]]$children[[1]]$children[[4]]
    sim <- iwin[[1]]$children[[1]]$children[[1]]$children[[5]]
    expect_equal(
        chk$widget$label,
        "Chi-square test"
    )
    expect_false(enabled(sim))
    expect_false(svalue(sim))

    expect_silent(svalue(chk) <- TRUE)
    expect_true(enabled(sim))
    expect_silent(svalue(sim) <- TRUE)

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
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Simulated p-value"
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

cas <- census.at.school.500
library(dplyr)
library(magrittr)
suppressWarnings({
    cas2 <- cas %>%
        select("gender", "getlunch", "travel") %>%
        mutate(
            getlunch = forcats::fct_explicit_na(getlunch)
        ) %>%
        group_by(gender, getlunch, travel) %>%
        tally(name = "frequency") %>%
        ungroup() %>%
        mutate(height = sample(cas$height, nrow(.))) %>%
        as.data.frame()
})

ui$close()
ui <- iNZGUI$new()
ui$initializeGui(cas2)
ui$getActiveDoc()$getModel()$setFrequencies("frequency", ui)

test_that("Get inference works for frequencies", {
    svalue(ui$ctrlWidget$V1box) <- "travel"
    expect_true(enabled(ui$infBtn))
    iwin <- ui$infBtn$invoke_change_handler()
    expect_silent(
        iwin2 <- iwin[[1]]$children[[1]]$children[[2]]$invoke_change_handler()
    )
    dispose(iwin2[[1]])

    svalue(ui$ctrlWidget$V2box) <- "gender"
    expect_true(enabled(ui$infBtn))
    iwin <- ui$infBtn$invoke_change_handler()
    expect_silent(
        iwin2 <- iwin[[1]]$children[[1]]$children[[2]]$invoke_change_handler()
    )
    dispose(iwin2[[1]])
})


## Survey designs

ui$close()
data(api, package = "survey")
ui <- iNZGUI$new()
ui$initializeGui(apiclus2)
Sys.sleep(2)

test_that("Get inference for surveys", {
    expect_true(enabled(ui$infBtn))
    swin <- iNZSurveyDesign$new(ui)
    svalue(swin$clus1Var) <- "dnum"
    svalue(swin$clus2Var) <- "snum"
    svalue(swin$fpcVar) <- "fpc1 + fpc2"
    swin$createBtn$invoke_change_handler()
    expect_true(enabled(ui$infBtn))

    svalue(ui$ctrlWidget$V1box) <- "api00"
    iwin <- ui$infBtn$invoke_change_handler()
    svalue(iwin[[1]]$children[[1]]$children[[1]]$children[[2]]) <- TRUE
    out <- iwin[[1]]$children[[1]]$children[[2]]$invoke_change_handler()
    expect_match(
        svalue(out[[1]]$children[[1]]),
        "survey::svydesign"
    )
    dispose(out[[1]])
})

test_that("Get inference still enabled for non-surveys", {
    ui$removeDesign()
    expect_true(enabled(ui$infBtn))
})
