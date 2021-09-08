context("Survey data")

skip_on_cran()
skip_if_offline()

data(api, package = "survey")
chis <- iNZightTools::smart_read("https://inzight.nz/testdata/chis.csv")
ncsr <- iNZightTools::smart_read("https://inzight.nz/testdata/ncsr.csv")
apijk <- iNZightTools::smart_read("apiclus2-jk1.csv")

test_dir <- getwd()

# ui$close(); load_all()
ui <- iNZGUI$new()
ui$initializeGui(apiclus2)
on.exit(try(gWidgets2::dispose(ui$win), TRUE))

test_that("Survey design window defaults are empty", {
    expect_silent(swin <- iNZSurveyDesign$new(ui))
    expect_true(visible(swin$designWin))

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
    s <- ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign()$spec
    expect_equal(
        s[!sapply(s, is.null)],
        list(
            ids = "dnum + snum",
            fpc = "fpc1 + fpc2",
            nest = FALSE,
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

test_that("Frequency column specification is passed to settings", {
    expect_silent(swin <- iNZSurveyDesign$new(ui, type = "frequency"))

    expect_equal(svalue(swin$freqVar), character(0))
    expect_silent(svalue(swin$freqVar) <- "frequency")
    expect_equal(svalue(swin$freqVar), "frequency")

    expect_silent(swin$createBtn$invoke_change_handler())
    expect_equal(
        as.character(ui$iNZDocuments[[ui$activeDoc]]$getSettings()$freq),
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
    svalue(fwin$filter_var) <- "gender"
    svalue(fwin$cat_levels, index = TRUE) <- 1
    expect_silent(
        fwin$ok_button$invoke_change_handler()
    )
})

ui$close()

# devtools::load_all()
dchis <- suppressWarnings(svrepdesign(data = chis[,c(1:10, 92:96)],
    repweights = chis[, 12:91],
    weights = chis[, 11],
    type = "other", scale = 1, rscales = 1
))

# try(ui$close()); devtools::load_all()
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
    s <- ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign()$spec
    expect_equal(
        s[!sapply(s, is.null)],
        list(
            ids = 1,
            # probs = NULL,
            # strata = NULL,
            # fpc = NULL,
            nest = logical(0),
            weights = "rakedw0",
            type = "replicate",
            repweights = paste("rakedw", 1:80, sep = ""),
            scale = 1,
            rscales = rep(1, 80),
            reptype = "other"
            # poststrat = NULL
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



# apides <- svrepdesign(weights = ~pw, repweights = "repw[0-9]+",
#     data = apijk, type = "JK1")
# svymean(~api00, des=apides)
# svymean(~enroll, des=apides)

# # iNZightPlot(api00, design = apides)

# x <- try(iNZightPlot(enroll, design = apides))

ui <- iNZGUI$new()
ui$initializeGui(apijk)


test_that("JK1 works", {
    expect_silent(swin <- iNZSurveyDesign$new(ui, type = "replicate"))
    svalue(swin$wtVar) <- "pw"
    svalue(swin$repVars) <-
        paste("repw", formatC(1:40, width = 2, flag = "0"), sep = "")
    svalue(swin$repType) <- "JK1"
    #### producing error about scale (n-1)/n not provided
    # expect_silent(swin$createBtn$invoke_change_handler())
    swin$createBtn$invoke_change_handler()
    s <- ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign()$spec
    expect_equal(
        s[!sapply(s, is.null)],
        list(
            ids = 1,
            nest = logical(0),
            weights = "pw",
            type = "replicate",
            repweights =
                paste("repw", formatC(1:40, width = 2, flag = "0"), sep = ""),
            reptype = "JK1"
        )
    )

    # svalue(ui$ctrlWidget$V1box) <- "enroll"
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

# try(ui$close()); devtools::load_all()
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
    pt <- pop.types$Freq
    names(pt) <- paste(pop.types$stype)
    s <- ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign()$spec
    expect_equal(
        s[!sapply(s, is.null)],
        list(
            ids = "dnum",
            fpc = "fpc",
            nest = FALSE,
            weights = "pw",
            type = "survey",
            calibrate = list(stype = c(E = 4421,H = 755, M = 1018))
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
    s <- ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign()$spec
    expect_equal(
        s[!sapply(s, is.null)],
        list(
            ids = "dnum",
            fpc = "fpc",
            nest = FALSE,
            weights = "pw",
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
    s <- ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign()$spec
    expect_equal(
        s[!sapply(s, is.null)],
        list(
            ids = "dnum",
            fpc = "fpc",
            nest = FALSE,
            weights = "pw",
            type = "survey",
            calibrate = list(stype = c(E = 4421,H = 755, M = 1018))
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
    # pop.types2 <- data.frame(
    #     sch.wide = c("No", "Yes"),
    #     Freq = as.numeric(),
    #     stringsAsFactors = TRUE
    # )

    # and trigger the save
    expect_silent(swin$okBtn$invoke_change_handler())
    s <- ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign()$spec
    expect_equal(
        s[!sapply(s, is.null)],
        list(
            ids = "dnum",
            fpc = "fpc",
            nest = FALSE,
            weights = "pw",
            type = "survey",
            calibrate = list(
                stype = structure(as.numeric(table(apipop$stype)), .Names = levels(apipop$stype)),
                sch.wide = structure(as.numeric(table(apipop$sch.wide)), .Names = levels(apipop$sch.wide))
            )
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
    swin <- iNZSurveyDesign$new(ui)
    expect_silent(svalue(swin$stratVar) <- "SESTRAT")
    expect_silent(svalue(swin$clus1Var) <- "SECLUSTR")
    expect_silent(svalue(swin$wtVar) <- "popweight")
    expect_silent(svalue(swin$nestChk) <- TRUE)
    expect_silent(swin$createBtn$invoke_change_handler())

    # add interaction between REGION and race
    comb <- iNZCombineWin$new(ui)
    svalue(comb$factorNames) <- c("REGION", "race")
    comb$factorNames$invoke_change_handler()
    comb$ok_button$invoke_change_handler()
    expect_is(ui$getActiveData()$REGION_race, "factor")

    expect_silent(swin <- iNZSurveyPostStrat$new(ui, .use_ui = FALSE))
    expect_true("REGION_race" %in% swin$PSvar$get_items())
    swin$cancelBtn$invoke_change_handler()

})

# data(api, package = "survey")

# load_all()
ui$close()
ui <- iNZGUI$new()
ui$initializeGui(apistrat)


# e <- new.env()
# e$data <- apistrat
# e$data.svy <- svydesign(ids=~1, strata = ~snum, weights = ~pw, fpc = ~fpc, data = apistrat)

# eval(parse(text = "inzplot(~api99, data = data)"), envir = e)
# eval(parse(text = "inzplot(~api99, design = data.svy)"), envir = e)

test_that("Survey design read from file", {
    svyfile <- tempfile("apistrat", fileext = ".svydesign")
    writeLines('strata = "stype"\nweights = "pw"\nfpc = "fpc"', svyfile)
    on.exit(unlink(svyfile))

    swin <- iNZSurveyDesign$new(ui)
    expect_silent(swin$read_file(svyfile))
    expect_equivalent(
        ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign()$spec,
        iNZightTools::import_survey(svyfile, apistrat)$spec
    )

    ui$setDocument(iNZDocument$new(data = apiclus2), reset = TRUE)
    Sys.sleep(5)
    writeLines('ids = "dnum + snum"\nfpc = "fpc1 + fpc2"', svyfile)

    swin <- iNZSurveyDesign$new(ui)
    expect_silent(swin$read_file(svyfile))
    expect_equivalent(
        ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign()$spec,
        iNZightTools::import_survey(svyfile, apiclus2)$spec
    )
})


ui$close()

# try(ui$close(), TRUE); devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui()

test_that("Survey data can be imported from svydesign file", {
    imp <- iNZImportWin$new(ui)
    imp$fname <- "ncsr.svydesign"
    imp$setfile()
    Sys.sleep(1)
    skip_if(length(imp$prevGp$children) == 1,
        message = "Preview did not load."
    )
    expect_is(imp$prevGp$children[[2]], "GDf")
    expect_equal(imp$prevGp$children[[2]]$get_dim(), c(rows = 30L, cols = 3L))
    expect_silent(imp$okBtn$invoke_change_handler())
    expect_equivalent(ui$getActiveData(), ncsr)
    expect_is(ui$getActiveDoc()$getModel()$getDesign()$design, "survey.design")
})

ui$close()

# require(survey); data(api)
# devtools::load_all()
ui <- iNZGUI$new()

test_that("Survey calibration imported read from svydesign file", {
    ui$initializeGui(apistrat)
    on.exit(ui$close())
    svyfile <- tempfile("apistrat", fileext = ".svydesign")
    writeLines('strata = "stype"\nweights = "pw"\nfpc = "fpc"\n\n[calibrate.stype]\nE = 4421\nH=755\nM=1018\n\n[calibrate."sch.wide"]\n"No" = 1072\n"Yes" = 5122\n', svyfile)
    on.exit(unlink(svyfile), add = TRUE)

    swin <- iNZSurveyDesign$new(ui)
    expect_silent(swin$read_file(svyfile))
    ui$iNZDocuments[[ui$activeDoc]]$getModel()$getDesign()$spec


})



# devtools::load_all("../iNZightTools")
ncsr_svy <- iNZightTools::import_survey(file.path(test_dir, "ncsr.svydesign"))
# ncsr_svy <- iNZightTools::import_survey('tests/testthat/ncsr.svydesign')

# try(ui$close(), TRUE); devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui(ncsr_svy$data)

test_that("Invalid menu items are disabled", {
    m <- function() ui$menuBarWidget$menubar$get_value()
    expect_true(enabled(m()$Dataset$stack))
    expect_true(enabled(m()$Dataset[["Dataset operation"]]$reshape))
    expect_true(enabled(m()$Dataset[["Merge/Join datasets"]]$appendrows))
    expect_is(m()$Dataset[["Frequency tables"]], "list")

    ui$getActiveDoc()$getModel()$setDesign(ncsr_svy$spec, ui)
    expect_false(enabled(m()$Dataset$stack))
    expect_false(enabled(m()$Dataset[["Dataset operation"]]$reshape))
    expect_false(enabled(m()$Dataset[["Merge/Join datasets"]]$appendrows))
    expect_is(m()$Dataset[["Frequency tables"]], "GAction")
    expect_false(enabled(m()$Dataset[["Frequency tables"]]))
})
