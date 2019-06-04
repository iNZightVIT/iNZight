context("Survey data")
data(api, package = "survey")

ui <- iNZGUI$new()
ui$initializeGui(apiclus2)
on.exit(gWidgets2::dispose(ui$win))

test_that("Survey design window defaults are empty", {
    expect_silent(swin <- iNZSurveyDesign$new(ui, warn = FALSE))

    expect_equal(svalue(swin$stratVar), "")
    expect_equal(svalue(swin$clus1Var), "")
    expect_equal(svalue(swin$clus2Var), "")
    expect_false(svalue(swin$nestChk))
    expect_equal(svalue(swin$wtVar), "")
    expect_equal(svalue(swin$fpcVar), "")

    expect_silent(swin$cancelBtn$invoke_change_handler())
})

# svydesign(id = ~dnum + snum, fpc = ~fpc1 + fpc2, data = apiclus2)
test_that("Survey design can be specified using window", {
    expect_silent(swin <- iNZSurveyDesign$new(ui, warn = FALSE))
    expect_silent(svalue(swin$clus1Var) <- "dnum")
    expect_silent(svalue(swin$clus2Var) <- "snum")
    expect_silent(svalue(swin$fpcVar) <- "fpc1 + fpc2")

    expect_equal(svalue(swin$clus1Var), "dnum")
    expect_equal(svalue(swin$clus2Var), "snum")
    expect_equal(svalue(swin$fpcVar), "fpc1 + fpc2")

    expect_silent(swin$createBtn$invoke_change_handler())
    expect_equal(
        ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign(),
        list(
            strata = NULL,
            clus1 = "dnum",
            clus2 = "snum",
            wt = NULL,
            fpc = "fpc1 + fpc2",
            nest = FALSE,
            repweights = NULL,
            freq = NULL
        )
    )
})

test_that("Survey design window remembers the design", {
    expect_silent(swin <- iNZSurveyDesign$new(ui, warn = FALSE))

    expect_equal(svalue(swin$stratVar), "")
    expect_equal(svalue(swin$clus1Var), "dnum")
    expect_equal(svalue(swin$clus2Var), "snum")
    expect_false(svalue(swin$nestChk))
    expect_equal(svalue(swin$wtVar), "")
    expect_equal(svalue(swin$fpcVar), "fpc1 + fpc2")

    expect_silent(swin$cancelBtn$invoke_change_handler())
})

test_that("Removing design works", {
    expect_silent(ui$removeDesign())
    expect_null(ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign())
})

ui$close()

cas <- census.at.school.500
library(dplyr)
library(magrittr)
suppressWarnings({
    cas2 <- cas %>%
        select("gender", "getlunch", "travel") %>%
        group_by(gender, getlunch, travel) %>%
        tally(name = "frequency") %>%
        as.data.frame()
})

ui <- iNZGUI$new()
ui$initializeGui(cas2)
on.exit(gWidgets2::dispose(ui$win))

test_that("Frequency column specification is passed to settings", {
    expect_silent(swin <- iNZSurveyDesign$new(ui, freq = TRUE, warn = FALSE))

    expect_equal(svalue(swin$freqVar), character(0))
    expect_silent(svalue(swin$freqVar) <- "frequency")
    expect_equal(svalue(swin$freqVar), "frequency")

    expect_silent(swin$createBtn$invoke_change_handler())
    expect_equal(
        ui$iNZDocuments[[ui$activeDoc]]$getSettings()$freq,
        cas2$frequency
    )
})

test_that("Frequencies retained after filtering", {
    ## I suspsect it will be broken because of the way things work ...
})

ui$close()


data(scd, package = "survey")
repweights <- 2 *
    data.frame(
        weights.1 = c(1,0,1,0,1,0),
        weights.2 = c(1,0,0,1,0,1),
        weights.3 = c(0,1,1,0,0,1),
        weights.4 = c(0,1,0,1,1,0)
    )
scd$ESAcat <- as.factor(scd$ESA)
scd$ambulancecat <- as.factor(scd$ambulance)
scd <- cbind(scd, repweights)

# load_all()
ui <- iNZGUI$new()
ui$initializeGui(scd)
test_that("Replicate weights can be specified", {
    expect_silent(swin <- iNZSurveyDesign$new(ui, warn = FALSE))

    # check rep weights box
    expect_false(svalue(swin$useRep))
    expect_false(enabled(swin$repVars))
    expect_silent(svalue(swin$useRep) <- TRUE)
    expect_true(enabled(swin$repVars))

    # select variables
    svalue(swin$repVars) <- paste("weights", 1:4, sep = ".")

    expect_warning(
        swin$createBtn$invoke_change_handler(),
        "No weights or probabilities"
    )
    expect_equal(
        ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign(),
        list(
            strata = NULL,
            clus1 = NULL,
            clus2 = NULL,
            wt = NULL,
            fpc = NULL,
            nest = FALSE,
            repweights = paste("weights", 1:4, sep = "."),
            freq = NULL
        )
    )
})

ui$close()
