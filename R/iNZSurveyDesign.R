iNZSurveyDesign <- setRefClass(
    "iNZSurveyDesign",
    contains = "iNZWindow",
    fields = list(
        svytype = "character",
        freqVar = "ANY",
        stratVar = "ANY",
        clus1Var = "ANY",
        clus2Var = "ANY",
        nestChk = "ANY",
        wtVar = "ANY",
        popSize = "ANY",
        fpcVar = "ANY",
        fpcVar2 = "ANY",
        combWts = "ANY",
        repVars = "ANY",
        repType = "ANY",
        repScale = "ANY",
        repRscalesBtn = "ANY",
        repRscalesClear = "ANY",
        repRscales = "ANY",
        rscalesTbl = "ANY",
        readFileBtn = "ANY",
        createBtn = "ANY",
        cancelBtn = "ANY"
    ),
    methods = list(
        initialize = function(gui, type = c("survey", "replicate", "frequency")) {
            if (is.null(gui$getActiveData())) {
                gerror("Please import a data set first.",
                    title = "No data set",
                    icon = "error"
                )
                return()
            }
            if (names(gui$getActiveData())[1] == "empty") {
                gmessage("Please import a data set first.",
                    title = "No data set",
                    icon = "error"
                )
                return()
            }

            initFields(svytype = match.arg(type))

            ok <- callSuper(gui,
                title = switch(svytype,
                    "survey" = "Specify Complex Survey Design",
                    "replicate" = "Specify Survey Design with Replicate Weights",
                    "frequency" = "Specify frequency column"
                ),
                width = switch(svytype,
                    "replicate" = "large",
                    "small"
                ),
                height = switch(svytype,
                    "frequency" = "small",
                    "med"
                ),
                help = switch(svytype,
                    "frequency" = "user_guides/data_options/#freqtables",
                    "user_guides/data_options/#svydesign"
                ),
                ok = "Create",
                action = .self$create,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("create")

            pnl <- switch(svytype,
                "survey" = set_design(),
                "replicate" = set_repdesign(),
                "frequency" = set_frequency()
            )
            add_body(pnl)

            readFileBtn <<- gbutton("Read from file",
                handler = function(h, ...) read_file())
            readFileBtn$set_icon("gw-file")
            add_toolbar(readFileBtn)

            ## Populate the lists:
            curDes <- GUI$getActiveDoc()$getModel()$getDesign()
            if (is.null(curDes)) return()

            spec <- curDes$spec
            switch(svytype,
                "survey" = {
                    if (!is.null(spec$strata))
                        svalue(stratVar) <<- spec$strata
                    if (!is.null(spec$ids) && spec$ids != 1) {
                        clus <- trimws(strsplit(spec$ids, "+", fixed = TRUE)[[1]])
                        svalue(clus1Var) <<- clus[1]
                        if (length(clus) == 2L)
                            svalue(clus2Var) <<- clus[2]
                    }
                    if (!is.null(spec$nest))
                        svalue(nestChk) <<- spec$nest
                    if (!is.null(spec$weights))
                        svalue(wtVar) <<- spec$weights
                    # TO DO: special handling here
                    if (!is.null(spec$fpc)) {
                        fpc <- trimws(strsplit(spec$fpc, "+", fixed = TRUE)[[1]])
                        svalue(fpcVar) <<- fpc[1]
                        if (length(fpc) == 2L)
                            svalue(fpcVar2) <<- fpc[2]
                    }
                },
                "replicate" = {
                    if (!is.null(spec$weights))
                        svalue(wtVar) <<- spec$weights
                    if (!is.null(spec$repweights)) {
                        # suppress warning from bug in gWidgets2:
                        suppressWarnings(
                            svalue(repVars) <<- spec$repweights
                        )
                        if (!is.null(spec$rscales)) {
                            repRscales <<- data.frame(
                                rep.weight = spec$repweights,
                                rscales = spec$rscales,
                                stringsAsFactors = TRUE
                            )
                            display_scales()
                        }
                    }
                    if (!is.null(spec$reptype)) {
                        svalue(repType) <<- spec$reptype
                    }
                    if (!is.null(spec$scale)) {
                        svalue(repScale) <<- spec$scale
                    }
                },
                "freq" = {
                    if (!is.null(spec$freq))
                        svalue(freqVar) <<- spec$freq
                }
            )
        },
        svalue_or_null = function(x) {
            if (svalue(x) == "") return(NULL)
            svalue(x)
        },
        calculate_population_size = function() {
            svalue(popSize) <<- ""

            svy <- try(create(preview = TRUE), silent = TRUE)
            if (inherits(svy, "try-error")) {
                return()
            }

            wts <- switch(svytype,
                "survey" = survey:::weights.survey.design(svy),
                "replicate" = survey:::weights.svyrep.design(svy)
            )
            svalue(popSize) <<- format(
                sum(wts),
                nsmall = 0L,
                big.mark = ",",
                scientific = FALSE
            )
        },
        create = function(preview = FALSE) {
            switch(svytype,
                "survey" = {
                    strat <- svalue_or_null(stratVar)
                    clus1 <- svalue_or_null(clus1Var)
                    clus2 <- svalue_or_null(clus2Var)
                    if (!is.null(clus1) && !is.null(clus2)) {
                        clus <- paste(clus1, clus2, sep = " + ")
                    } else if (is.null(clus1) && is.null(clus2)) {
                        clus <- "1"
                    } else {
                        clus <- ifelse(is.null(clus1), clus2, clus1)
                    }
                    wts <- svalue_or_null(wtVar)
                    fpc1 <- svalue_or_null(fpcVar)
                    fpc2 <- svalue_or_null(fpcVar2)
                    if (!is.null(fpc1) && !is.null(fpc2)) {
                        fpc <- paste(fpc1, fpc2, sep = " + ")
                    } else if (is.null(fpc1) && is.null(fpc2)) {
                        fpc <- NULL
                    } else {
                        fpc <- ifelse(is.null(fpc1), fpc2, fpc1)
                    }
                    nest <- as.logical(svalue(nestChk))
                    clear <- is.null(strat) && is.null(clus1) &&
                        is.null(clus2) && is.null(wts) && is.null(fpc)

                    spec <- list(
                        strata = strat,
                        ids = clus,
                        weights = wts,
                        nest = nest,
                        fpc = fpc,
                        type = "survey"
                    )

                    if (preview) {
                        spec <- iNZightTools::make_survey(
                            GUI$getActiveData(),
                            structure(list(spec = spec), class = "inzsvyspec")
                        )
                        return(spec$design)
                    }
                    GUI$getActiveDoc()$getModel()$setDesign(
                        spec,
                        gui = GUI
                    )
                },
                "replicate" = {
                    wts <- svalue_or_null(wtVar)
                    repWts <- svalue(repVars, index = FALSE)
                    reptype <- svalue(repType)
                    if (reptype %in% c("bootstrap", "other")) {
                        scale <- as.numeric(svalue(repScale))
                        rscales <- as.numeric(repRscales$rscales)
                        if (length(rscales) == 0)
                            rscales <- rep(scale, length(repWts))
                        else if(any(is.na(rscales)))
                            rscales <- NULL
                    } else if (reptype %in% c("JK1")) {
                        n <- length(repWts)
                        scale <- (n - 1) / n
                        rscales <- NULL
                    } else {
                        scale <- NULL
                        rscales <- NULL
                    }
                    clear <- is.null(wts) && length(repWts) == 0

                    spec <- list(
                        weights = wts,
                        repweights = repWts,
                        reptype = reptype,
                        scale = scale,
                        rscales = rscales,
                        type = "replicate"
                    )
                    if (preview) {
                        spec <- iNZightTools::make_survey(
                            GUI$getActiveData(),
                            structure(
                                list(spec = spec),
                                class = "inzsvyspec"
                            )
                        )
                        return(spec$design)
                    }
                    GUI$getActiveDoc()$getModel()$setDesign(
                        spec,
                        gui = GUI
                    )
                },
                "frequency" = {
                    freqv <- svalue_or_null(freqVar)
                    GUI$getActiveDoc()$getModel()$setFrequencies(
                        freq = freqv,
                        gui = GUI
                    )
                    close()
                    return()
                }
            )

            setOK <- try(
                GUI$getActiveDoc()$getModel()$createSurveyObject(),
                TRUE
            )

            if (!inherits(setOK, "try-error")) {
                close()

                ## write design call
                call <- paste(deparse(setOK$call), collapse = "\n")

                call <- sprintf("%s <- %s",
                    GUI$getActiveDoc()$getModel()$dataDesignName,
                    gsub("dataSet", GUI$getActiveDoc()$getModel()$name, call)
                )
                GUI$rhistory$add(
                    c("## create survey design object\n", call),
                    tidy = TRUE
                )

                ## update plot
                GUI$updatePlot()
            } else {
                gmessage(
                    paste0(
                        "There is a problem with the specification of the survey design:\n\n",
                        setOK
                    ),
                    icon = "error"
                )
            }
        },
        set_design = function() {
            g <- gvbox()

            tbl <- glayout(cont = g)

            vars <- c("", colnames(GUI$getActiveData()))

            ii <- 2
            lbl <- glabel("Strata variable: ")
            tbl[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
            stratVar <<- gcombobox(vars)
            tbl[ii, 2, expand = TRUE] <- stratVar

            ii <- ii + 1
            lbl <- glabel("1st stage clustering variable: ")
            tbl[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
            clus1Var <<- gcombobox(vars)
            tbl[ii, 2, expand = TRUE] <- clus1Var

            ii <- ii + 1
            lbl <- glabel("2nd stage clustering variable: ")
            tbl[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
            clus2Var <<- gcombobox(vars)
            tbl[ii, 2, expand = TRUE] <- clus2Var
            enabled(clus2Var) <<- FALSE

            addHandlerChanged(clus1Var,
                function(h, ...) {
                    enabled(clus2Var) <<- clus1Var$get_index() > 1L
                }
            )
            addHandlerChanged(clus2Var,
                function(h, ...) {
                    enabled(fpcVar2) <<- clus2Var$get_index() > 1L
                }
            )

            ii <- ii + 1
            nestChk <<- gcheckbox("Use nested sampling")
            tbl[ii, 2, expand = TRUE] <- nestChk

            ii <- ii + 2
            lbl <- glabel("Weighting variable: ")
            tbl[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
            wtVar <<- gcombobox(vars,
                handler = function(h, ...) calculate_population_size()
            )
            tbl[ii, 2, expand = TRUE] <- wtVar

            ii <- ii + 1
            lbl <- glabel("Finite population correction: ")
            tbl[ii, 1, expand = TRUE, fill = FALSE, anchor= c(1, 0)] <- lbl
            fpcVar <<- gcombobox(vars,
                handler = function(h, ...) calculate_population_size())
            tbl[ii, 2, expand = TRUE] <- fpcVar

            ii <- ii + 1
            lbl <- glabel("Finite population correction (2nd stage): ")
            tbl[ii, 1, expand = TRUE, fill = FALSE, anchor= c(1, 0)] <- lbl
            fpcVar2 <<- gcombobox(vars,
                handler = function(h, ...) calculate_population_size())
            tbl[ii, 2, expand = TRUE] <- fpcVar2
            enabled(fpcVar2) <<- FALSE

            ii <- ii + 1
            lbl <- glabel("Estimated population size: ")
            font(lbl) <- list(size = 9)
            tbl[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
            popSize <<- glabel("")
            font(popSize) <<- list(size = 9)
            tbl[ii, 2, expand = TRUE] <- popSize

            g
        },
        set_repdesign = function() {
            g <- ggroup()

            g1 <- gvbox(container = g)

            vars <- c("", colnames(GUI$getActiveData()))

            ## ... weights, combine.weights here ...
            tbl <- glayout(container = g1)
            ii <- 1

            tbl[ii, 1, expand = TRUE, anchor = c(1, 0)] <-
                glabel("Sampling weights : ")
            wtVar <<- gcombobox(vars,
                handler = function(h, ...) calculate_population_size())
            tbl[ii, 2, expand = TRUE] <- wtVar
            ii <- ii + 1

            combWts <<- gcheckbox(
                "Replication weights incorporate sampling weights",
                checked = TRUE,
                handler = function(h, ...) calculate_population_size())
            tbl[ii, 1:2, expand = TRUE, anchor = c(1, 0)] <- combWts
            ii <- ii + 1

            addSpace(g1, 5)

            repVars <<- gtable(vars[-1], multiple = TRUE, container = g1,
                handler = function(h, ...) calculate_population_size())
            repVars$set_names("Select replicate weights")
            size(repVars) <<- c(-1, 320)

            lbl <- glabel(
                paste(sep = "\n",
                    "To select a range, click the first, then hold SHIFT while clicking the last.",
                    "Hold CTRL while clicking to add and remove invidividual variables."
                )
            )
            font(lbl) <- list(size = 8)
            add(g1, lbl)


            tbl2 <- glayout(container = g1)
            lbl <- glabel("Estimated population size: ")
            font(lbl) <- list(size = 9)
            tbl2[1L, 1L, expand = TRUE, fill = TRUE, anchor = c(1, 0)] <- lbl
            popSize <<- glabel("")
            font(popSize) <<- list(size = 9)
            tbl2[1L, 2L, expand = TRUE] <- popSize


            g2 <- gvbox(container = g, expand = TRUE)
            ## type, scale, etc.

            tbl2 <- glayout(container = g2)
            ii <- 1

            lbl <- glabel("Type of replication weights: ")
            repType <<- gcombobox(
                c("BRR", "Fay", "JK1", "JKn", "bootstrap", "other"),
                selected = 6,
                handler = function(h, ...) calculate_population_size()
            )
            tbl2[ii, 1, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl2[ii, 2, expand = TRUE] <- repType
            ii <- ii + 1

            addSpace(g2, 5)
            scalesG <- gvbox(container = g2)

            tbl3 <- glayout(container = scalesG)
            ii <- 1

            scalesLbl <- glabel(
                "Specify at least one of overall scale and individual replicate scales"
            )
            font(scalesLbl) <- list(size = 9, weight = "bold")
            tbl3[ii, 1:2, expand = TRUE, anchor = c(0, 0)] <- scalesLbl
            ii <- ii + 1

            lbl <- glabel("Overall scale: ")
            repScale <<- gedit("",
                handler = function(h, ...) calculate_population_size())
            tbl3[ii, 1, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl3[ii, 2, expand = TRUE] <- repScale
            ii <- ii + 1

            lbl <- glabel("Replicate scales: ")
            repRscalesBtn <<- gbutton("Read from file ...",
                handler = function(h, ...) {
                    f <- gfile(
                        type = "open",
                        filter = c("csv" = "csv")
                    )
                    if (length(f) == 0) return()

                    set_rscales(f)
                }
            )
            repRscalesClear <<- gbutton("Clear",
                handler = function(h, ...) {
                    repRscales <<- data.frame(
                        rep.weight = character(),
                        rscales = numeric(),
                        stringsAsFactors = TRUE
                    )
                    display_scales()
                }
            )
            tbl3[ii, 1, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl3[ii, 2, expand = TRUE] <- repRscalesBtn
            tbl3[ii, 3, expand = TRUE] <- repRscalesClear
            ii <- ii + 1


            lbl <- glabel("File should contain one replicate scale per line.")
            font(lbl) <- list(size = 8)
            tbl3[ii, 2:3, expand = TRUE, anchor = c(-1, 0)] <- lbl
            ii <- ii + 1

            ## initialize repRscales
            repRscales <<-
                data.frame(rep.weight = character(), rscales = numeric(),
                    stringsAsFactors = TRUE
                )
            rscalesTbl <<- gtable(repRscales,
                handler = function(h, ...) calculate_population_size())
            tbl3[ii, 2:3, expand = TRUE] <- rscalesTbl
            size(rscalesTbl) <<- c(-1, 200)

            # addHandlerSelectionChanged(repVars,
            #     function(h, ...) {
            #         repRscales <<- data.frame(
            #             rep.weight = svalue(repVars),
            #             rscales = rep("", length(svalue(h$obj)))
            #         )
            #         display_scales()
            #     }
            # )

            addHandlerChanged(repType,
                function(h, ...) {
                    visible(scalesG) <-
                        !svalue(h$obj) %in% c("BRR", "Fay", "JK1", "JKn")
                }
            )


            ## Return the content container thing
            g
        },
        display_scales = function() {
            rscalesTbl$set_items(repRscales)
            invisible()
        },
        set_rscales = function(file) {
            # if first row is a character, file has a header
            x1 <- readLines(file, n = 1)
            file_has_header <- suppressWarnings(is.na(as.numeric(x1)))
            df <- read.csv(file, header = file_has_header, stringsAsFactors = TRUE)
            if (nrow(df) != length(svalue(repVars))) {
                gmessage("You need to specify one scale per replicate.")
                return()
            }
            names(df)[1] <- "rscales"
            repRscales <<- cbind(
                data.frame(rep.weight = svalue(repVars), stringsAsFactors = TRUE),
                df
            )
            display_scales()
        },
        set_frequency = function() {
            g <- gvbox()

            lbl <- glabel("Choose frequency column", cont = g)
            font(lbl) <- list(weight = "bold", size = 11)

            addSpace(g, 5)

            as.int <- function(x) {
                is.numeric(x) && all(floor(x) == x, na.rm = TRUE)
            }
            ints <- sapply(GUI$getActiveData(), as.int)
            vars <- names(GUI$getActiveData())[ints]

            freqVar <<- gcombobox(vars, selected = 0, container = g)

            addSpace(g, 5)
            lbl <- glabel("WARNING: any non-categorical variables will be removed")
            font(lbl) <- list(weight = "bold", size = 9)
            add(g, lbl, expand = TRUE, anchor = c(-1, 0))

            g
        },
        read_file = function(file) {
            if (missing(file)) {
                file <- gfile("Select survey design file",
                    type = "open",
                    filter = ".svydesign"
                )
                if (length(file) == 0) return()
            }
            svyspec <- iNZightTools::import_survey(file)
            GUI$getActiveDoc()$getModel()$setDesign(svyspec, gui = GUI)

            setOK <- try(
                GUI$getActiveDoc()$getModel()$createSurveyObject(),
                silent = TRUE
            )

            if (!inherits(setOK, "try-error")) {
                close()

                ## write design call
                call <- paste(deparse(setOK$call), collapse = "\n")

                call <- sprintf("%s <- %s",
                    GUI$getActiveDoc()$getModel()$dataDesignName,
                    gsub("dataSet", GUI$getActiveDoc()$getModel()$name, call)
                )
                GUI$rhistory$add(
                    c("## create survey design object", call),
                    tidy = TRUE
                )

                ## update plot
                GUI$updatePlot()
            } else {
                gmessage(
                    paste0(
                        "There is a problem with the survey specification file:\n\n",
                        setOK
                    ),
                    icon = "error"
                )
            }
        }
    )
)

iNZSurveyPostStrat <- setRefClass(
    "iNZSurveyPostStrat",
    contains = "iNZWindow",
    fields = list(
        PSvar = "ANY",
        PSlvls = "ANY",
        lvldf = "list",
        okBtn = "ANY",
        cancelBtn = "ANY",
        rmvBtn = "ANY"
    ),
    methods = list(
        initialize = function(gui, .use_ui = TRUE) {
            initFields(lvldf = list())

            curDes <- gui$getActiveDoc()$getModel()$getDesign()
            if (is.null(curDes)) {
                if (.use_ui) {
                    gmessage("Please specify a survey design first",
                        title = "No design specified",
                        icon = "warning"
                    )
                } else {
                    warning("Please specify a survey design first")
                }
                return(invisible(NULL))
            }
            curDes <- curDes$spec

            ok <- callSuper(gui,
                title = "Survey Calibration / Post-stratification",
                width = "med",
                height = "med",
                help = "user_guides/data_options/#post-stratify",
                ok = "Calibrate",
                action = .self$calibrate,
                show_code = FALSE,
                scroll = FALSE,
                body_direction = "horizontal"
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("calibrate")

            ## left size panel (choose variables)
            g1 <- gvbox()

            ## only those with no missing values ...
            fvars <- sapply(GUI$getActiveData(),
                function(v) length(levels(v)) > 0 && sum(is.na(v)) == 0)
            factorvars <- names(GUI$getActiveData())[fvars]
            PSvar <<- gtable(factorvars,
                multiple = TRUE,
                container = g1,
                expand = TRUE
            )
            PSvar$set_names("Choose variables")
            size(PSvar) <<- c(200, 380)
            addHandlerSelectionChanged(PSvar,
                function(h, ...) update_levels())

            lbl <- glabel("Hold CTRL or SHIFT to select multiple",
                container = g1,
                anchor = c(-1, 0),
                expand = TRUE
            )
            font(lbl) <- list(size = 8)

            add_body(g1)

            body_space(10)

            g2 <- gvbox(use.scrollwindow = "y")
            ## table to populate with levels of variable
            gl <- ggroup(container = g2)
            PSlvls <<- glayout(container = gl)
            addSpace(gl, 20)

            add_body(g2, expand = TRUE)

            addHandlerChanged(PSvar,
                function(h, ...) update_levels())

            rmvBtn <<- gbutton("Remove calibration",
                handler = function(h, ...) {
                    svalue(PSvar, index = TRUE) <<- 0
                    calibrate()
                }
            )
            add_toolbar(rmvBtn)

            ## populate on load
            lvldf <<- GUI$getActiveDoc()$getModel()$getFreqTables()
            if (!is.null(curDes$calibrate)) {
                svalue(PSvar) <<- names(curDes$calibrate)
                display_tbl()
            }

            ## when the window closes, store the lvldf in survey
            addHandlerDestroy(GUI$modWin,
                function(h, ...) {
                    GUI$getActiveDoc()$getModel()$storeFreqTables(lvldf)
                }
            )
        },
        update_levels = function(h, ...) {
            for (v in svalue(PSvar)) {
                if (is.null(lvldf[[v]])) {
                    d <- data.frame(
                        a = levels(GUI$getActiveData()[[v]]),
                        b = NA,
                        stringsAsFactors = TRUE
                    )
                    names(d) <- c(v, "Freq")
                    lvldf[[v]] <<- d
                }
            }

            display_tbl()
        },
        set_freqs = function(variable, df) {
            ## add checks that 2 columns with all necessary levels
            names(df) <- c(variable, "Freq")
            lvldf[[variable]] <<- df
            ## now display the information in the UI
            display_tbl()
        },
        set_freq = function(variable, level, freq) {
            lvldf[[variable]]$Freq[lvldf[[variable]][,1] == level] <<-
                as.numeric(freq)
        },
        display_tbl = function() {
            # remove existing children from PSlvls
            if (length(PSlvls$children))
                sapply(PSlvls$children, PSlvls$remove_child)

            # Only display those chosen on the left
            if (length(svalue(PSvar)) == 0) return()

            # display lvldf variables selected in tables:
            ii <- 1

            set_freq_val <- function(h, ...) {
                ## need a way of figuring out which VARIABLE it is ...
                j <- which(sapply(PSlvls[, 2],
                    function(z) identical(z, h$obj)
                ))
                set_freq(
                    svalue(PSlvls[j, 4]), # inivisble variable name
                    svalue(PSlvls[j, 1]), # variable level
                    svalue(h$obj)         # freq
                )
            }

            for (v in svalue(PSvar)) {
                lbl <- glabel(v)
                font(lbl) <- list(weight = "bold")
                PSlvls[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <<- lbl

                lbl <- glabel("Frequency")
                font(lbl) <- list(weight = "bold")
                PSlvls[ii, 2, expand = TRUE, fill = FALSE, anchor = c(-1, 0)] <<- lbl
                lbl <- glabel(v)
                visible(lbl) <- FALSE
                PSlvls[ii, 4] <<- lbl

                for (i in seq_along(1:nrow(lvldf[[v]]))) {
                    ii <- ii + 1
                    PSlvls[ii, 1,
                        expand = TRUE,
                        fill = TRUE,
                        anchor = c(1, 0)
                    ] <<- glabel(lvldf[[v]][i, 1])
                    PSlvls[ii, 2] <<- gedit(
                        ifelse(is.na(lvldf[[v]][i, 2]), "", lvldf[[v]][i, 2]),
                        width = 20,
                        handler = set_freq_val
                    )
                    addHandlerKeystroke(PSlvls[ii, 2], set_freq_val)

                    lbl <- glabel(v)
                    visible(lbl) <- FALSE
                    PSlvls[ii, 4] <<- lbl
                }

                btn <- gbutton("Read from file ...",
                    handler = function(h, ...) {
                        f <- gfile(
                            type = "open",
                            filter = c("csv" = "csv")
                        )
                        if (length(f) == 0) return()

                        df <- read.csv(f, stringsAsFactors = TRUE)
                        rowj <- which(
                            sapply(PSlvls[, 3],
                                function(z) identical(z, h$obj))
                        )
                        var <- svalue(PSlvls[rowj, 4])
                        if (ncol(df) != 2) {
                            gmessage(
                                paste(
                                    "File needs to have 2 columns: one for variable",
                                    "names, and one for frequencies."
                                )
                            )
                            return()
                        }
                        if (nrow(df) != nrow(lvldf[[var]])) {
                            gmessage("File needs to have one row for each level.")
                            return()
                        }
                        set_freqs(var, df)
                    }
                )
                ## add button to second-to-last-row
                PSlvls[ii-1, 3, anchor = c(1, 0)] <<- btn

                btn <- gbutton("Paste from clipboard ...")

                ii <- ii + 2
                PSlvls[ii, 1:3] <<- gseparator()
                ii <- ii + 2
            }

        },
        calibrate = function() {
            curDes <- GUI$getActiveDoc()$getModel()$getDesign()$spec
            cal_list <- lapply(names(lvldf),
                function(var) {
                    x <- lvldf[[var]]$Freq
                    names(x) <- lvldf[[var]][[var]]
                    x
                }
            )
            names(cal_list) <- names(lvldf)
            GUI$getActiveDoc()$getModel()$setDesign(
                modifyList(curDes,
                    list(
                        calibrate =
                            if (length(svalue(PSvar))) cal_list[svalue(PSvar)]
                            else NULL
                    )
                ),
                gui = GUI
            )
            setOK <- try(
                GUI$getActiveDoc()$getModel()$createSurveyObject(),
                TRUE
            )
            if (inherits(setOK, "try-error")) {
                gmessage(
                    "Something went wrong during post stratification ...",
                    type = "error"
                )
                return()
            } else {
                call <- paste(deparse(setOK$call), collapse = "\n")

                call <- sprintf("%s <- %s",
                    GUI$getActiveDoc()$getModel()$dataDesignName,
                    gsub("design_obj", GUI$getActiveDoc()$getModel()$name, call)
                )
                GUI$rhistory$add(
                    c(
                        ifelse(length(svalue(PSvar)),
                            "## calibrate survey design",
                            "## remove survey calibration"
                        ),
                        call
                    ),
                    tidy = TRUE
                )
            }
            close()
        }
    )
)
