context("Survey data")
data(api, package = "survey")
chis <- iNZightTools::smart_read("chis.csv")
ncsr <- iNZightTools::smart_read("ncsr.csv")

test_dir <- getwd()

# ui$close()
ui <- iNZGUI$new()
ui$initializeGui(apiclus2)
on.exit(try(ui$close(), TRUE))

test_that("Survey design window defaults are empty", {
    expect_silent(swin <- iNZSurveyDesign$new(ui))

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
    expect_silent(swin <- iNZSurveyDesign$new(ui))
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
            poststrat = NULL,
            type = "survey"
        )
    )
})

test_that("Survey design window remembers the design", {
    expect_silent(swin <- iNZSurveyDesign$new(ui))

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
        mutate(
            getlunch = forcats::fct_explicit_na(getlunch)
        ) %>%
        group_by(gender, getlunch, travel) %>%
        tally(name = "frequency") %>%
        ungroup() %>%
        mutate(height = sample(cas$height, nrow(.))) %>%
        as.data.frame()
})

# ui$close()
ui <- iNZGUI$new()
ui$initializeGui(cas2)
on.exit(gWidgets2::dispose(ui$win))

test_that("Frequency column specification is passed to settings", {
    expect_silent(swin <- iNZSurveyDesign$new(ui, type = "frequency"))

    expect_equal(svalue(swin$freqVar), character(0))
    expect_silent(svalue(swin$freqVar) <- "frequency")
    expect_equal(svalue(swin$freqVar), "frequency")

    expect_silent(swin$createBtn$invoke_change_handler())
    expect_equal(
        ui$iNZDocuments[[ui$activeDoc]]$getSettings()$freq,
        "frequency"
    )
})

test_that("Non-categorical variables removed after specifying frequencies", {
    expect_true(
        all(
            sapply(ui$getActiveData(), is_cat)[names(ui$getActiveData()) != "frequency"]
        )
    )
})

test_that("Plotting and summary of frequencies works", {
    expect_silent(svalue(ui$ctrlWidget$V1box) <- "travel")
    expect_equal(ui$plotType, "bar")
    expect_silent(ui$sumBtn$invoke_change_handler())
})

test_that("Frequencies retained after filtering", {
    fwin <- iNZFilterWin$new(ui)
    dispose(ui$modWin)
    fwin$opt1()
    svalue(ui$modWin$children[[1]]$children[[1]]$children[[2]]) <- "gender"
    svalue(ui$modWin$children[[1]]$children[[2]]) <- 1
    expect_silent(
        ui$modWin$children[[1]]$children[[3]]$children[[1]]$invoke_change_handler()
    )
})

ui$close()

# devtools::load_all()
# chis <- iNZightTools::smart_read("tests/testthat/chis.csv")
dchis <- suppressWarnings(svrepdesign(data = chis[,c(1:10, 92:96)],
    repweights = chis[, 12:91],
    weights = chis[, 11],
    type = "other", scale = 1, rscales = 1
))

# devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui(chis)

test_that("Replicate weights can be specified", {
    expect_silent(swin <- iNZSurveyDesign$new(ui, type = "replicate"))

    # select variables
    svalue(swin$wtVar) <- "rakedw0"
    svalue(swin$repVars) <- paste("rakedw", 1:80, sep = "")
    swin$repVars$invoke_change_handler()
    svalue(swin$repType) <- "other"
    svalue(swin$repScale) <- 1

    expect_silent(swin$createBtn$invoke_change_handler())
    expect_equal(
        ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign(),
        list(
            wt = "rakedw0",
            repweights = paste("rakedw", 1:80, sep = ""),
            reptype = "other",
            scale = 1,
            rscales = rep(1, 80),
            poststrat = NULL,
            type = "replicate"
        )
    )
})

test_that("Replicate weight object is valid", {
    expect_silent(
        des <- ui$iNZDocuments[[ui$activeDoc]]$getModel()$createSurveyObject()
    )
    expect_is(des, "svyrep.design")
    expect_equivalent(weights(des), weights(dchis))
})

test_that("Replicate weight window repopulated correctly", {
    expect_silent(swin <- iNZSurveyDesign$new(ui, type = "replicate"))
    expect_equal(svalue(swin$wtVar), "rakedw0")
    expect_equal(svalue(swin$repVars), paste("rakedw", 1:80, sep = ""))
    expect_equal(svalue(swin$repType), "other")
    expect_equal(svalue(swin$repScale), "1")
    expect_equal(
        swin$rscalesTbl$get_items(),
        data.frame(
            rep.weight = paste("rakedw", 1:80, sep = ""),
            rscales = rep(1, 80),
            stringsAsFactors = TRUE
        )
    )
    swin$cancelBtn$invoke_change_handler()
})

f1 <- file.path(test_dir, "chis_wts.csv")
f2 <- file.path(test_dir, "chis_wts_header.csv")
test_that("Replicate weights can be specified by file", {
    expect_silent(swin <- iNZSurveyDesign$new(ui, type = "replicate"))
    expect_silent(swin$repRscalesClear$invoke_change_handler())
    expect_equal(
        swin$rscalesTbl$get_items(),
        data.frame(rep.weight = character(), rscales = numeric(),
            stringsAsFactors = TRUE
        )
    )

    expect_silent(swin$set_rscales(f1))
    expect_equal(
        swin$rscalesTbl$get_items(),
        data.frame(
            rep.weight = paste("rakedw", 1:80, sep = ""),
            rscales = read.csv(f1, header = FALSE, stringsAsFactors = TRUE)[[1]],
            stringsAsFactors = TRUE
        )
    )

    expect_silent(swin$repRscalesClear$invoke_change_handler())
    expect_equal(
        swin$rscalesTbl$get_items(),
        data.frame(rep.weight = character(), rscales = numeric(),
            stringsAsFactors = TRUE
        )
    )

    expect_silent(swin$set_rscales(f2))
    expect_equal(
        swin$rscalesTbl$get_items(),
        data.frame(
            rep.weight = paste("rakedw", 1:80, sep = ""),
            rscales = read.csv(f2, stringsAsFactors = TRUE)[[1]],
            stringsAsFactors = TRUE
        )
    )
})

ui$close()

# devtools::load_all()
data(api, package = "survey")
# replicate this:
dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
pop.types <- data.frame(
    stype = c("E", "H", "M"), Freq = c(4421, 755, 1018),
    stringsAsFactors = TRUE
)
vec <- structure(
    c(sum(pop.types$Freq), pop.types$Freq[-1]),
    .Names = c("(Intercept)", paste0("stype", as.character(pop.types$stype[-1])))
)
dclus1p <- calibrate(dclus1, ~stype, vec)

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
    expect_equal(swin$lvldf, list())
    expect_silent(svalue(swin$PSvar) <- "stype")
    expect_silent(swin$PSvar$invoke_change_handler())

    expect_equal(
        swin$lvldf,
        list(stype =
            data.frame(stype = c("E", "H", "M"), Freq = NA, stringsAsFactors = TRUE)
        )
    )

    # now the tbl should have length(levels(style)) + 2 rows
    expect_equal(
        dim(swin$PSlvls),
        c(nrow = 6, ncol = 4)
    )

    # read from file
    tmp <- tempfile(fileext = ".csv")
    write.csv(pop.types, file = tmp, quote = FALSE, row.names = FALSE)
    expect_silent(swin$set_freqs("stype", read.csv(tmp, stringsAsFactors = TRUE)))
    expect_equal(
        sapply(swin$PSlvls$children[c(5, 8, 11)], svalue),
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
            poststrat = list(stype = pop.types),
            type = "survey"
        )
    )
})

test_that("Post stratification is remembered", {
    expect_silent(swin <- iNZSurveyPostStrat$new(ui, .use_ui = FALSE))
    expect_equal(svalue(swin$PSvar), "stype")
    expect_equal(swin$lvldf, list(stype = pop.types))
    expect_silent(swin$cancelBtn$invoke_change_handler())
})

test_that("Post stratification can be removed", {
    expect_silent(swin <- iNZSurveyPostStrat$new(ui, .use_ui = FALSE))
    expect_silent(svalue(swin$PSvar, index = TRUE) <- 0)
    expect_equal(swin$lvldf, list(stype = pop.types))
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
            poststrat = NULL,
            type = "survey"
        )
    )
})

test_that("Frequency tables are saved", {
    expect_equal(
        ui$getActiveDoc()$getModel()$getFreqTables(),
        list(stype = pop.types)
    )
})

# clear the values
ui$getActiveDoc()$getModel()$storeFreqTables(NULL)

test_that("Post stratification set by manually entering values", {
    expect_silent(swin <- iNZSurveyPostStrat$new(ui, .use_ui = FALSE))
    expect_silent(svalue(swin$PSvar, index = TRUE) <- 1)

    expect_equal(
        swin$lvldf,
        list(stype =
            data.frame(stype = c("E", "H", "M"), Freq = NA,
                stringsAsFactors = TRUE
            )
        )
    )

    # now the tbl should have length(levels(style)) + 2 rows
    expect_equal(
        dim(swin$PSlvls),
        c(nrow = 6, ncol = 4)
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

    expect_equal(swin$lvldf, list(stype = pop.types))

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
            poststrat = list(stype = pop.types),
            type = "survey"
        )
    )
})

test_that("Post stratification object is correct", {
    expect_silent(
        des <- ui$iNZDocuments[[ui$activeDoc]]$getModel()$createSurveyObject()
    )
    expect_is(des, "survey.design2")
    expect_equal(des$postStrata, dclus1p$postStrata)
})

test_that("Multiple variables can be specified (raking calibration)", {
    expect_silent(swin <- iNZSurveyPostStrat$new(ui, .use_ui = FALSE))
    expect_silent(svalue(swin$PSvar, index = TRUE) <- 1:2)
    expect_equal(
        swin$lvldf,
        list(
            stype = pop.types,
            sch.wide =
                data.frame(sch.wide = c("No", "Yes"), Freq = NA,
                    stringsAsFactors = TRUE
                )
        )
    )

    swin$lvldf$sch.wide$Freq <- as.numeric(table(apipop$sch.wide))
    expect_silent(swin$display_tbl())
    pop.types2 <- data.frame(
        sch.wide = c("No", "Yes"),
        Freq = as.numeric(table(apipop$sch.wide)),
        stringsAsFactors = TRUE
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
            poststrat = list(stype = pop.types, sch.wide = pop.types2),
            type = "survey"
        )
    )

    dclus1g2 <- calibrate(dclus1, ~stype + sch.wide,
        c(vec, sch.wideYes = 5122))

    expect_silent(
        des <- ui$iNZDocuments[[ui$activeDoc]]$getModel()$createSurveyObject()
    )
    expect_is(des, "survey.design2")
    expect_equal(des$postStrata, dclus1g2$postStrata)
})

test_that("User can remove calibration from a survey", {
    expect_silent(swin <- iNZSurveyPostStrat$new(ui, .use_ui = FALSE))
    expect_silent(swin$rmvBtn$invoke_change_handler())
    expect_equal(
        ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign()$poststrat,
        NULL
    )
})

ui$close()
ui <- iNZGUI$new()
ui$initializeGui(ncsr)

test_that("New variables show up in calibration list", {
    # ncsr <- iNZightTools::smart_read("tests/testthat/ncsr.csv")

    swin <- iNZSurveyDesign$new(ui)
    expect_silent(svalue(swin$stratVar) <- "SESTRAT")
    expect_silent(svalue(swin$clus1Var) <- "SECLUSTR")
    expect_silent(svalue(swin$wtVar) <- "popweight")
    expect_silent(svalue(swin$nestChk) <- TRUE)
    expect_silent(swin$createBtn$invoke_change_handler())

    # add interaction between REGION and race
    comb <- iNZcmbCatWin$new(ui)
    svalue(ui$modWin$children[[1]]$children[[3]]) <- c("REGION", "race")
    ui$modWin$children[[1]]$children[[3]]$invoke_change_handler()
    ui$modWin$children[[1]]$children[[5]]$invoke_change_handler()
    expect_is(ui$getActiveData()$REGION.race, "factor")

    expect_silent(swin <- iNZSurveyPostStrat$new(ui, .use_ui = FALSE))
    expect_true("REGION.race" %in% swin$PSvar$get_items())
    swin$cancelBtn$invoke_change_handler()

})
