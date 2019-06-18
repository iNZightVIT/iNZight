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
            poststrat = NULL,
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
        mutate(height = sample(cas$height, nrow(.))) %>%
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

test_that("Non-categorical variables removed after specifying frequencies", {
    expect_true(
        all(
            sapply(ui$getActiveData(), is_cat)[names(ui$getActiveData()) != "frequency"]
        )
    )
})

test_that("Frequencies retained after filtering", {
    ## I suspsect it will be broken because of the way things work ...
})

ui$close()

# devtools::load_all()
chis <- iNZightTools::smart_read("chis.csv")
# chis <- iNZightTools::smart_read("tests/testthat/chis.csv")
dchis <- svrepdesign(data = chis[,c(1:10, 92:96)],
    repweights = chis[, 12:91],
    weights = chis[, 11],
    type = "BRR"
)

# devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui(chis)
test_that("Replicate weights can be specified", {
    expect_silent(swin <- iNZSurveyDesign$new(ui, warn = FALSE))

    # check rep weights box
    expect_false(svalue(swin$useRep))
    expect_false(visible(swin$repG))
    expect_silent(svalue(swin$useRep) <- TRUE)
    expect_true(visible(swin$repG))

    # select variables
    svalue(swin$wtVar) <- "rakedw0"
    svalue(swin$repVars) <- paste("rakedw", 1:80, sep = "")

    expect_silent(swin$createBtn$invoke_change_handler())
    expect_equal(
        ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign(),
        list(
            strata = NULL,
            clus1 = NULL,
            clus2 = NULL,
            wt = "rakedw0",
            fpc = NULL,
            nest = FALSE,
            repweights = paste("rakedw", 1:80, sep = ""),
            poststrat = NULL,
            freq = NULL
        )
    )
})

test_that("Replicate weight object is valid", {
    expect_silent(
        des <- ui$iNZDocuments[[ui$activeDoc]]$getModel()$createSurveyObject()
    )
    expect_equal(des)
})

ui$close()


# devtools::load_all()
data(api, package = "survey")
# replicate this:
dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
pop.types <- data.frame(stype = c("E", "H", "M"), Freq = c(4421, 755, 1018))
dclus1p <- postStratify(dclus1, ~stype, pop.types)

ui <- iNZGUI$new()
ui$initializeGui(apiclus1)

test_that("Survey design must be specified", {
    expect_warning(
        swin <- iNZSurveyPostStrat$new(ui, .use_ui = FALSE),
        "Please specify a survey design first"
    )
})

test_that("Post stratification set by importing additional dataset", {
    expect_silent(swin <- iNZSurveyDesign$new(ui))
    expect_silent(svalue(swin$clus1Var) <- "dnum")
    expect_silent(svalue(swin$fpcVar) <- "fpc")
    expect_silent(svalue(swin$wtVar) <- "pw")
    expect_silent(swin$createBtn$invoke_change_handler())

    expect_silent(swin <- iNZSurveyPostStrat$new(ui, .use_ui = FALSE))
    expect_silent(svalue(swin$PSvar) <- "stype")

    expect_equal(
        swin$lvldf,
        data.frame(stype = c("E", "H", "M"), Freq = NA)
    )

    # now the tbl should have length(levels(style)) + 2 rows
    expect_equal(
        dim(swin$PSlvls),
        c(nrow = 4, ncol = 2)
    )

    # read from file
    tmp <- tempfile(fileext = ".csv")
    write.csv(pop.types, file = tmp, quote = FALSE, row.names = FALSE)
    expect_silent(swin$update_levels(read.csv(tmp)))
    expect_equal(
        sapply(swin$PSlvls$children[c(4, 6, 8)], svalue),
        as.character(pop.types$Freq)
    )

    # and trigger the save
    expect_silent(swin$okBtn$invoke_change_handler())
    expect_equal(
        ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign(),
        list(
            strata = NULL,
            clus1 = "dnum",
            clus2 = NULL,
            wt = "pw",
            fpc = "fpc",
            nest = FALSE,
            repweights = NULL,
            poststrat = pop.types,
            freq = NULL
        )
    )
})

test_that("Post stratification is remembered", {
    expect_silent(swin <- iNZSurveyPostStrat$new(ui, .use_ui = FALSE))
    expect_equal(svalue(swin$PSvar), "stype")
    expect_equal(swin$lvldf, pop.types)
    expect_silent(swin$cancelBtn$invoke_change_handler())
})

test_that("Post stratification can be removed", {
    expect_silent(swin <- iNZSurveyPostStrat$new(ui, .use_ui = FALSE))
    expect_silent(svalue(swin$PSvar, index = TRUE) <- 1)
    expect_null(swin$lvldf)
    expect_silent(swin$okBtn$invoke_change_handler())
    expect_equal(
        ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign(),
        list(
            strata = NULL,
            clus1 = "dnum",
            clus2 = NULL,
            wt = "pw",
            fpc = "fpc",
            nest = FALSE,
            repweights = NULL,
            poststrat = NULL,
            freq = NULL
        )
    )
})

test_that("Post stratification set by manually entering values", {
    expect_silent(swin <- iNZSurveyPostStrat$new(ui, .use_ui = FALSE))
    expect_silent(svalue(swin$PSvar) <- "stype")

    expect_equal(
        swin$lvldf,
        data.frame(stype = c("E", "H", "M"), Freq = NA)
    )

    # now the tbl should have length(levels(style)) + 2 rows
    expect_equal(
        dim(swin$PSlvls),
        c(nrow = 4, ncol = 2)
    )

    # manually enter values
    j <- which(sapply(swin$PSlvls$children,
        function(x) identical(x, swin$PSlvls[2, 2])))
    svalue(swin$PSlvls$children[[j]]) <- pop.types$Freq[1]
    j <- which(sapply(swin$PSlvls$children,
        function(x) identical(x, swin$PSlvls[3, 2])))
    svalue(swin$PSlvls$children[[j]]) <- pop.types$Freq[2]
    j <- which(sapply(swin$PSlvls$children,
        function(x) identical(x, swin$PSlvls[4, 2])))
    svalue(swin$PSlvls$children[[j]]) <- pop.types$Freq[3]

    expect_equal(swin$lvldf, pop.types)

    # and trigger the save
    expect_silent(swin$okBtn$invoke_change_handler())
    expect_equal(
        ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign(),
        list(
            strata = NULL,
            clus1 = "dnum",
            clus2 = NULL,
            wt = "pw",
            fpc = "fpc",
            nest = FALSE,
            repweights = NULL,
            poststrat = pop.types,
            freq = NULL
        )
    )
})
