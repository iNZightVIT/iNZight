context("Get Inference window")

# try(ui$close(), TRUE); devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

ui$setDocument(iNZDocument$new(data = census.at.school.500), reset = TRUE)
Sys.sleep(5)

test_that("Get inference window - dot plots", {
    svalue(ui$ctrlWidget$V1box) <- "height"

    iwin <- iNZGetInference$new(ui)
    on.exit(gWidgets2::dispose(iwin$win))
    expect_is(iwin, "iNZGetInference")

    expect_equal(svalue(iwin$inf_method), "Normal theory")
    expect_equal(iwin$hypothesis_test$get_items(), c("None", "One sample t-test"))
    expect_equal(svalue(iwin$hypothesis_test), "None")
    expect_false(visible(iwin$g_hypctrls))
    expect_false(any(grepl("One Sample t-test", svalue(iwin$info_text))))

    expect_silent(iwin$hypothesis_test$set_index(2L))
    expect_true(visible(iwin$g_hypctrls))
    expect_equal(svalue(iwin$hyp_null), "0")
    expect_equal(svalue(iwin$hyp_alt), "two-sided")
    expect_match(svalue(iwin$info_text), "One Sample t-test")
    expect_match(svalue(iwin$info_text), "true mean is equal to 0")
    expect_match(svalue(iwin$info_text), "true mean is not equal to 0")

    expect_true(iwin$hyp_null$set_value("100"))
    iwin$hyp_alt$set_index(2L)
    expect_match(svalue(iwin$info_text), "true mean is equal to 100")
    expect_match(svalue(iwin$info_text), "true mean is greater than 100")

    expect_silent(iwin$hypothesis_test$set_index(1L))
    expect_false(visible(iwin$g_hypctrls))
    expect_false(any(grepl("One Sample t-test", svalue(iwin$info_text))))
})


test_that("Get inference window - two-way dot plots", {
    svalue(ui$ctrlWidget$V2box) <- "gender"

    iwin <- iNZGetInference$new(ui)
    on.exit(gWidgets2::dispose(iwin$win))
    expect_is(iwin, "iNZGetInference")

    expect_equal(svalue(iwin$inf_method), "Normal theory")
    expect_equal(iwin$hypothesis_test$get_items(), c("None", "Two sample t-test", "ANOVA"))
    expect_equal(svalue(iwin$hypothesis_test), "None")
    expect_false(visible(iwin$g_hypctrls))
    expect_false(any(grepl("Two Sample t-test|ANOVA", svalue(iwin$info_text))))

    expect_silent(iwin$hypothesis_test$set_index(2L))
    expect_true(visible(iwin$g_hypctrls))
    expect_equal(svalue(iwin$hyp_null), "0")
    expect_equal(svalue(iwin$hyp_alt), "two-sided")
    expect_false(svalue(iwin$hyp_equalvar))
    expect_match(svalue(iwin$info_text), "Welch Two Sample t-test")
    expect_match(svalue(iwin$info_text), "true difference in means is equal to 0")
    expect_match(svalue(iwin$info_text), "true difference in means is not equal to 0")

    expect_true(iwin$hyp_null$set_value("5"))
    iwin$hyp_alt$set_index(3L)
    iwin$hyp_equalvar$set_value(TRUE)
    expect_match(svalue(iwin$info_text), "Two Sample t-test assuming equal variance")
    expect_match(svalue(iwin$info_text), "true difference in means is equal to 5")
    expect_match(svalue(iwin$info_text), "true difference in means is less than 5")

    expect_silent(iwin$hypothesis_test$set_index(3L))
    expect_false(visible(iwin$g_hypctrls))
    expect_match(svalue(iwin$info_text), "One-way Analysis of Variance")

    expect_silent(iwin$hypothesis_test$set_index(1L))
    expect_false(visible(iwin$g_hypctrls))
    expect_false(any(grepl("Two Sample t-test|ANOVA", svalue(iwin$info_text))))
})

test_that("Get inference window - ANOVA dot plots", {
    svalue(ui$ctrlWidget$V2box) <- "getlunch"

    iwin <- iNZGetInference$new(ui)
    on.exit(gWidgets2::dispose(iwin$win))
    expect_is(iwin, "iNZGetInference")

    expect_equal(svalue(iwin$inf_method), "Normal theory")
    expect_equal(iwin$hypothesis_test$get_items(), c("None", "ANOVA"))
    expect_equal(svalue(iwin$hypothesis_test), "None")
    expect_false(visible(iwin$g_hypctrls))
    expect_false(any(grepl("ANOVA", svalue(iwin$info_text))))

    expect_silent(iwin$hypothesis_test$set_index(2L))
    expect_match(svalue(iwin$info_text), "One-way Analysis of Variance")
    expect_match(svalue(iwin$info_text), "true group means are all equal")
    expect_match(svalue(iwin$info_text), "true group means are not all equal")

    expect_silent(iwin$hypothesis_test$set_index(1L))
    expect_false(any(grepl("ANOVA", svalue(iwin$info_text))))
})

test_that("Get inference window - one way bar plots, two levels", {
    svalue(ui$ctrlWidget$V2box, index = TRUE) <- 1
    svalue(ui$ctrlWidget$V1box) <- "gender"

    iwin <- iNZGetInference$new(ui)
    on.exit(gWidgets2::dispose(iwin$win))
    expect_is(iwin, "iNZGetInference")

    expect_equal(svalue(iwin$inf_method), "Normal theory")
    expect_equal(iwin$hypothesis_test$get_items(), c("None", "Test proportion", "Chi-square test"))
    expect_equal(svalue(iwin$hypothesis_test), "None")
    expect_false(visible(iwin$g_hypctrls))
    expect_false(any(grepl("Chi-square test|Exact binomial test|One-sample test of a proportion", svalue(iwin$info_text))))

    expect_silent(iwin$hypothesis_test$set_index(2L))
    expect_true(visible(iwin$g_hypctrls))
    expect_equal(svalue(iwin$hyp_null), "0.5")
    expect_equal(svalue(iwin$hyp_alt), "two-sided")
    expect_false(svalue(iwin$hyp_exactp))
    expect_match(svalue(iwin$info_text), "One-sample test of a proportion")
    expect_match(svalue(iwin$info_text), "true proportion of gender = female is 0.5")
    expect_match(svalue(iwin$info_text), "true proportion of gender = female is not equal to 0.5")

    expect_true(iwin$hyp_null$set_value("0.6"))
    iwin$hyp_alt$set_index(2L)
    iwin$hyp_exactp$set_value(TRUE)
    expect_match(svalue(iwin$info_text), "Exact binomial test")
    expect_match(svalue(iwin$info_text), "true proportion of gender = female is 0.6")
    expect_match(svalue(iwin$info_text), "true proportion of gender = female is greater than 0.6")

    expect_silent(iwin$hypothesis_test$set_index(3L))
    expect_true(visible(iwin$g_hypctrls))
    expect_match(svalue(iwin$info_text), "Chi-square test for equal proportions")

    expect_silent(iwin$hypothesis_test$set_index(1L))
    expect_false(visible(iwin$g_hypctrls))
    expect_false(any(grepl("Chi-square test|Exact binomial test|One-sample test of a proportion", svalue(iwin$info_text))))
})

test_that("Get inference window - one way bar plots", {
    svalue(ui$ctrlWidget$V2box, index = TRUE) <- 1
    svalue(ui$ctrlWidget$V1box) <- "getlunch"

    iwin <- iNZGetInference$new(ui)
    on.exit(gWidgets2::dispose(iwin$win))
    expect_is(iwin, "iNZGetInference")

    expect_equal(svalue(iwin$inf_method), "Normal theory")
    expect_equal(iwin$hypothesis_test$get_items(), c("None", "Chi-square test"))
    expect_equal(svalue(iwin$hypothesis_test), "None")
    expect_false(visible(iwin$g_hypctrls))
    expect_false(any(grepl("Chi-square test", svalue(iwin$info_text))))

    expect_silent(iwin$hypothesis_test$set_index(2L))
    expect_true(visible(iwin$g_hypctrls))
    expect_false(svalue(iwin$hyp_simulatep))
    expect_match(svalue(iwin$info_text), "Chi-square test for equal proportions")
    expect_false(any(grepl("Simulated p-value", svalue(iwin$info_text))))

    expect_silent(iwin$hyp_simulatep$set_value(TRUE))
    expect_match(svalue(iwin$info_text), "Simulated p-value")

    expect_silent(iwin$hypothesis_test$set_index(1L))
    expect_false(visible(iwin$g_hypctrls))
    expect_false(any(grepl("Chi-square test", svalue(iwin$info_text))))
})

test_that("Get inference window - two way bar plots", {
    svalue(ui$ctrlWidget$V1box) <- "cellsource"
    svalue(ui$ctrlWidget$V2box) <- "gender"

    iwin <- iNZGetInference$new(ui)
    on.exit(gWidgets2::dispose(iwin$win))
    expect_is(iwin, "iNZGetInference")

    expect_equal(svalue(iwin$inf_method), "Normal theory")
    expect_equal(iwin$hypothesis_test$get_items(), c("None", "Chi-square test"))
    expect_equal(svalue(iwin$hypothesis_test), "None")
    expect_false(visible(iwin$g_hypctrls))
    expect_false(any(grepl("Chi-square test", svalue(iwin$info_text))))

    expect_silent(iwin$hypothesis_test$set_index(2L))
    expect_true(visible(iwin$g_hypctrls))
    expect_false(svalue(iwin$hyp_simulatep))
    expect_match(svalue(iwin$info_text), "Chi-square test for equal distributions")
    expect_false(any(grepl("Simulated p-value", svalue(iwin$info_text))))

    expect_silent(iwin$hyp_simulatep$set_value(TRUE))
    expect_match(svalue(iwin$info_text), "Simulated p-value")

    expect_silent(iwin$hypothesis_test$set_index(1L))
    expect_false(visible(iwin$g_hypctrls))
    expect_false(any(grepl("Chi-square test", svalue(iwin$info_text))))
})


test_that("Get inference window - scatter plots", {
    svalue(ui$ctrlWidget$V1box) <- "height"
    svalue(ui$ctrlWidget$V2box) <- "armspan"

    iwin <- iNZGetInference$new(ui)
    on.exit(gWidgets2::dispose(iwin$win))
    expect_is(iwin, "iNZGetInference")

    expect_equal(svalue(iwin$inf_method), "Normal theory")
    expect_is(iwin$hypothesis_test, "uninitializedField")
    expect_match(svalue(iwin$info_text), "Please specify a trend line")

    expect_silent(iwin$trend_choice$linear$set_value(TRUE))
    expect_match(svalue(iwin$info_text), "Linear Trend Coefficients")

    expect_silent(iwin$trend_choice$linear$set_value(FALSE))
    expect_match(svalue(iwin$info_text), "Please specify a trend line")
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
    svalue(ui$ctrlWidget$V1box) <- "gender"
    expect_true(enabled(ui$infBtn))

    iwin <- iNZGetInference$new(ui)
    on.exit(gWidgets2::dispose(iwin$win))
    expect_is(iwin, "iNZGetInference")

    expect_equal(svalue(iwin$inf_method), "Normal theory")
    expect_equal(iwin$hypothesis_test$get_items(), c("None", "Test proportion", "Chi-square test"))

    gWidgets2::dispose(iwin$win)

    svalue(ui$ctrlWidget$V1box) <- "travel"
    expect_true(enabled(ui$infBtn))

    iwin <- iNZGetInference$new(ui)
    on.exit(gWidgets2::dispose(iwin$win))
    expect_is(iwin, "iNZGetInference")

    expect_equal(svalue(iwin$inf_method), "Normal theory")
    expect_equal(iwin$hypothesis_test$get_items(), c("None", "Chi-square test"))

    gWidgets2::dispose(iwin$win)

    svalue(ui$ctrlWidget$V2box) <- "gender"
    expect_true(enabled(ui$infBtn))

    iwin <- iNZGetInference$new(ui)
    on.exit(gWidgets2::dispose(iwin$win))
    expect_is(iwin, "iNZGetInference")

    expect_equal(svalue(iwin$inf_method), "Normal theory")
    expect_equal(iwin$hypothesis_test$get_items(), c("None", "Chi-square test"))
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

    iwin <- iNZGetInference$new(ui)
    on.exit(gWidgets2::dispose(iwin$win))
    expect_is(iwin, "iNZGetInference")

    expect_equal(svalue(iwin$inf_method), "Normal theory")
    expect_equal(iwin$hypothesis_test$get_items(), c("None", "One sample t-test"))

    # svalue(iwin[[1]]$children[[1]]$children[[1]]$children[[2]]) <- TRUE
    # out <- iwin[[1]]$children[[1]]$children[[2]]$invoke_change_handler()
    # expect_match(
    #     svalue(out[[1]]$children[[1]]),
    #     "survey::svydesign"
    # )
    # dispose(out[[1]])
})

test_that("Get inference still enabled for non-surveys", {
    ui$removeDesign()
    expect_true(enabled(ui$infBtn))
})
