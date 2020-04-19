iNZSurveyDesign <- setRefClass(
    "iNZSurveyDesign",
    fields = list(
        GUI = "ANY",
        designWin = "ANY",
        svytype = "character",
        freqVar = "ANY",
        stratVar = "ANY",
        clus1Var = "ANY",
        clus2Var = "ANY",
        nestChk = "ANY",
        wtVar = "ANY",
        fpcVar = "ANY",
        combWts = "ANY",
        repVars = "ANY",
        repType = "ANY",
        repScale = "ANY",
        repRscalesBtn = "ANY",
        repRscalesClear = "ANY",
        repRscales = "ANY",
        rscalesTbl = "ANY",
        createBtn = "ANY",
        cancelBtn = "ANY"
    ),
    methods = list(
        initialize = function(GUI, type = c("survey", "replicate", "frequency")) {
            initFields(GUI = GUI, svytype = match.arg(type))

            if (is.null(GUI$getActiveData())) {
                gerror("Please import a data set first.",
                    title = "No data set", icon = "error")
                return()
            } else if ((names(GUI$getActiveData())[1] == "empty")) {
                gmessage("Please import a data set first.",
                    title = "No data set", icon = "error")
                return()
            }

            designWin <<- gwindow(
                title = switch(svytype,
                    survey = "Specify Complex Survey Design",
                    replicate = "Specify Survey Design with Replicate Weights",
                    frequency = "Specify frequency column"
                ),
                visible = FALSE,
                width = ifelse(svytype == "replicate", 600, 450),
                height = 150,
                parent = GUI$win
            )

            gmain <- gvbox(container = designWin, expand = TRUE)
            gmain$set_borderwidth(5)

            pnl <- switch(svytype,
                "survey" = set_design(),
                "replicate" = set_repdesign(),
                "frequency" = set_frequency()
            )
            add(gmain, pnl)

            ## Add OK/cancel buttons
            addSpring(gmain)
            btnGrp <- ggroup(cont = gmain)
            addSpace(btnGrp, 10)
            #advancedBtn <- gbutton("Advanced", cont = btnGrp)
            addSpring(btnGrp)
            cancelBtn <<- gbutton("Cancel", cont = btnGrp)
            addSpace(btnGrp, 10)
            createBtn <<- gbutton("OK", cont = btnGrp)
            addSpace(btnGrp, 10)

            addSpace(gmain, 10)

            addHandlerClicked(cancelBtn, handler = function(h, ...) {
                dispose(designWin)
            })

            ## Move this to method ...
            addHandlerClicked(createBtn, handler = function(h, ...) {
                svalue_or_null <- function(x) {
                    if (svalue(x) == "") return(NULL)
                    svalue(x)
                }

                switch(svytype,
                    "survey" = {
                        strat <- svalue_or_null(stratVar)
                        clus1 <- svalue_or_null(clus1Var)
                        clus2 <- svalue_or_null(clus2Var)
                        wts <- svalue_or_null(wtVar)
                        fpc <- svalue_or_null(fpcVar)
                        nest <- as.logical(svalue(nestChk))
                        clear <- is.null(strat) && is.null(clus1) &&
                            is.null(clus2) && is.null(wts) && is.null(fpc)
                        GUI$getActiveDoc()$getModel()$setDesign(
                            strata = strat,
                            clus1 = clus1,
                            clus2 = clus2,
                            wt = wts,
                            nest = nest,
                            fpc = fpc,
                            type = "survey",
                            gui = GUI
                        )
                    },
                    "replicate" = {
                        wts <- svalue_or_null(wtVar)
                        repWts <- svalue(repVars, index = FALSE)
                        reptype <- svalue(repType)
                        scale <- as.numeric(svalue(repScale))
                        rscales <- as.numeric(repRscales$rscales)
                        if (length(rscales) == 0)
                            rscales <- rep(scale, length(repWts))
                        else if(any(is.na(rscales)))
                            rscales <- NULL
                        clear <- is.null(wts) && length(repWts) == 0
                        GUI$getActiveDoc()$getModel()$setDesign(
                            wt = wts,
                            repweights = repWts,
                            reptype = reptype,
                            scale = scale,
                            rscales = rscales,
                            type = "replicate",
                            gui = GUI
                        )
                    },
                    "frequency" = {
                        freqv <- svalue_or_null(freqVar)
                        GUI$getActiveDoc()$getModel()$setFrequencies(
                            freq = freqv,
                            gui = GUI
                        )
                        dispose(designWin)
                        return()
                    }
                )

                setOK <- try(
                    GUI$getActiveDoc()$getModel()$createSurveyObject(),
                    TRUE
                )

                if (!inherits(setOK, "try-error")) {
                    # enabled(GUI$infBtn) <<- clear
                    dispose(designWin)

                    ## write design call
                    call <- paste(deparse(setOK$call), collapse = "\n")

                    call <- sprintf("%s <- %s",
                        GUI$getActiveDoc()$getModel()$dataDesignName,
                        gsub("dataSet", GUI$getActiveDoc()$getModel()$name, call))
                    GUI$rhistory$add(c("## create survey design object", call),
                        tidy = TRUE)

                    ## update plot
                    GUI$updatePlot()
                } else {
                    gmessage(paste0(
                        "There is a problem with the specification of the survey design:\n\n",
                        setOK),
                        icon = "error")
                }
            })


            ## Populate the lists:
            curDes <- GUI$getActiveDoc()$getModel()$getDesign()
            if (!is.null(curDes)) {
                switch(svytype,
                    "survey" = {
                        if (!is.null(curDes$strata))
                            svalue(stratVar) <<- curDes$strata
                        if (!is.null(curDes$clus1))
                            svalue(clus1Var) <<- curDes$clus1
                        if (!is.null(curDes$clus2))
                            svalue(clus2Var) <<- curDes$clus2
                        if (!is.null(curDes$nest))
                            svalue(nestChk) <<- curDes$nest
                        if (!is.null(curDes$wt))
                            svalue(wtVar) <<- curDes$wt
                        if (!is.null(curDes$fpc))
                            svalue(fpcVar) <<- curDes$fpc
                    },
                    "replicate" = {
                        if (!is.null(curDes$wt))
                            svalue(wtVar) <<- curDes$wt
                        if (!is.null(curDes$repweights)) {
                            svalue(repVars) <<- curDes$repweights
                            if (!is.null(curDes$rscales)) {
                                repRscales <<- data.frame(
                                    rep.weight = curDes$repweights,
                                    rscales = curDes$rscales,
                                    stringsAsFactors = TRUE
                                )
                                display_scales()
                            }
                        }
                        if (!is.null(curDes$reptype)) {
                            svalue(repType) <<- curDes$reptype
                        }
                        if (!is.null(curDes$scale)) {
                            svalue(repScale) <<- curDes$scale
                        }
                    },
                    "freq" = {
                        if (!is.null(curDes$freq))
                            svalue(freqVar) <<- curDes$freq
                    }
                )
            }

           visible(designWin) <<- TRUE
        },
        set_design = function() {
            g <- gvbox()

            addSpace(g, 5)
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

            ii <- ii + 1
            nestChk <<- gcheckbox("Use nested sampling")
            tbl[ii, 2, expand = TRUE] <- nestChk

            ii <- ii + 2
            lbl <- glabel("Weighting variable: ")
            tbl[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
            wtVar <<- gcombobox(vars)
            tbl[ii, 2, expand = TRUE] <- wtVar

            ii <- ii + 1
            lbl <- glabel("Finite population correction: ")
            tbl[ii, 1, expand = TRUE, fill = FALSE, anchor= c(1, 0)] <- lbl
            fpcVar <<- gcombobox(vars, editable = TRUE)
            tbl[ii, 2, expand = TRUE] <- fpcVar

            g
        },
        set_repdesign = function() {
            size(designWin) <<- c(800, -1)
            g <- ggroup()

            g1 <- gvbox(container = g)

            vars <- c("", colnames(GUI$getActiveData()))

            ## ... weights, combine.weights here ...
            tbl <- glayout(container = g1)
            ii <- 1

            tbl[ii, 1, expand = TRUE, anchor = c(1, 0)] <-
                glabel("Sampling weights : ")
            wtVar <<- gcombobox(vars)
            tbl[ii, 2, expand = TRUE] <- wtVar
            ii <- ii + 1

            combWts <<- gcheckbox(
                "Replication weights incorporate sampling weights",
                checked = TRUE)
            tbl[ii, 1:2, expand = TRUE, anchor = c(1, 0)] <- combWts
            ii <- ii + 1

            addSpace(g1, 5)

            repVars <<- gtable(vars[-1], multiple = TRUE, container = g1)
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


            g2 <- gvbox(container = g, expand = TRUE)
            ## type, scale, etc.

            tbl2 <- glayout(container = g2)
            ii <- 1

            lbl <- glabel("Type of replication weights: ")
            repType <<- gcombobox(
                c("BRR", "Fay", "JK1", "JKn", "bootstrap", "other"),
                selected = 6
            )
            tbl2[ii, 1, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl2[ii, 2, expand = TRUE] <- repType
            ii <- ii + 1

            addSpace(g2, 5)
            scalesG <- gvbox(container = g2)

            tbl3 <- glayout(container = scalesG)
            ii <- 1

            scalesLbl <- glabel("Specify at least one of overall scale and individual replicate scales")
            font(scalesLbl) <- list(size = 9, weight = "bold")
            tbl3[ii, 1:2, expand = TRUE, anchor = c(0, 0)] <- scalesLbl
            ii <- ii + 1

            lbl <- glabel("Overall scale: ")
            repScale <<- gedit("")
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
            rscalesTbl <<- gtable(repRscales)
            tbl3[ii, 2:3, expand = TRUE] <- rscalesTbl
            size(rscalesTbl) <<- c(-1, 200)

            addHandlerSelectionChanged(repVars, function(h, ...) {
                # repRscales <<- data.frame(
                #     rep.weight = svalue(repVars),
                #     rscales = rep("", length(svalue(h$obj)))
                # )
                # display_scales()
            })

            addHandlerChanged(repType, function(h, ...) {
                visible(scalesG) <- !svalue(h$obj) %in% c("BRR", "Fay", "JK1", "JKn")
            })


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
        }
    )
)

iNZSurveyPostStrat <- setRefClass(
    "iNZSurveyPostStrat",
    fields = list(
        GUI = "ANY",
        win = "ANY",
        PSvar = "ANY",
        PSlvls = "ANY",
        lvldf = "list",
        okBtn = "ANY",
        cancelBtn = "ANY",
        rmvBtn = "ANY"
    ),
    methods = list(
        initialize = function(gui, .use_ui = TRUE) {
            initFields(GUI = gui, lvldf = list())

            curDes <- GUI$getActiveDoc()$getModel()$getDesign()
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

            win <<- gwindow("Post Stratification",
                parent = GUI$win,
                width = 730,
                height = 500,
                visible = FALSE
            )
            gmain <- gvbox(container = win)
            gmain$set_borderwidth(5)

            title <- glabel("Specify post stratification",
                container = gmain)
            font(title) <- list(size = 11, weight = "bold")

            ## central panel
            g <- ggroup(container = gmain)

            ## left size panel (choose variables)
            g1 <- gvbox(container = g)

            ## only those with no missing values ...
            # lbl <- glabel("Choose variables",
            #     container = g1,
            #     anchor = c(-1, 0),
            #     expand = TRUE
            # )
            factorvars <- names(GUI$getActiveData())[sapply(
                GUI$getActiveData(),
                function(v)
                    length(levels(v)) > 0 && sum(is.na(v)) == 0
            )]
            PSvar <<- gtable(factorvars,
                multiple = TRUE,
                container = g1,
                expand = TRUE
                # fill = TRUE
            )
            PSvar$set_names("Choose variables")
            size(PSvar) <<- c(200, 380)
            addHandlerSelectionChanged(PSvar, function(h, ...) update_levels())

            lbl <- glabel("Hold CTRL or SHIFT to select multiple",
                container = g1,
                anchor = c(-1, 0),
                expand = TRUE
            )
            font(lbl) <- list(size = 8)

            addSpace(g, 10)
            g2 <- gvbox(container = g,
                use.scrollwindow = "y",
                expand = TRUE
            )
            g2$set_borderwidth(5)
            ## table to populate with levels of variable
            gl <- ggroup(container = g2)
            # addSpring(gl)
            PSlvls <<- glayout(container = gl)
            addSpace(gl, 20)

            addHandlerChanged(PSvar, function(h, ...) update_levels())

            # save/cancel buttons
            addSpring(gmain)
            btnGrp <- ggroup(container = gmain)

            rmvBtn <<- gbutton("Remove",
                # icon = "delete",
                handler = function(h, ...) {
                    svalue(PSvar, index = TRUE) <<- 0
                    set_poststrat_design()
                    dispose(win)
                },
                container = btnGrp
            )

            addSpring(btnGrp)
            okBtn <<- gbutton("OK",
                handler = function(h, ...) {
                    # call postStratify
                    set_poststrat_design()
                    dispose(win)
                },
                container = btnGrp
            )
            cancelBtn <<- gbutton("Cancel",
                handler = function(h, ...) {
                    dispose(win)
                },
                container = btnGrp
            )

            ## populate on load
            lvldf <<- GUI$getActiveDoc()$getModel()$getFreqTables()
            if (!is.null(curDes$poststrat)) {
                svalue(PSvar) <<- names(curDes$poststrat)
                display_tbl()
            }

            ## when the window closes, store the lvldf in survey
            addHandlerDestroy(win, function(h, ...) {
                GUI$getActiveDoc()$getModel()$storeFreqTables(lvldf)
            })

            visible(win) <<- TRUE

            invisible(NULL)
        },
        update_levels = function(h, ...) {
            # read svalue(PSvar) -> lvldf
            # if (length(svalue(PSvar)) == 0) return()

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
                    PSlvls[ii, 1, expand = TRUE, fill = TRUE, anchor = c(1, 0)] <<-
                        glabel(lvldf[[v]][i, 1])
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
                        rowj <- which(sapply(PSlvls[, 3],
                            function(z) identical(z, h$obj)
                        ))
                        var <- svalue(PSlvls[rowj, 4])
                        if (ncol(df) != 2) {
                            gmessage("File needs to have 2 columns: one for variable names, and one for frequencies.")
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
                # PSlvls[ii, 3, anchor = c(1, 0)] <<- btn



                ii <- ii + 2
                PSlvls[ii, 1:3] <<- gseparator()
                ii <- ii + 2
            }

        },
        set_poststrat_design = function() {
            curDes <- GUI$getActiveDoc()$getModel()$getDesign()
            GUI$getActiveDoc()$getModel()$setDesign(
                curDes$strat, curDes$clus1, curDes$clus2,
                curDes$wt, curDes$nest, curDes$fpc,
                curDes$repWts,
                poststrat = if (length(svalue(PSvar))) lvldf[svalue(PSvar)] else NULL,
                type = curDes$type,
                gui = GUI
            )
            setOK <- try(
                GUI$getActiveDoc()$getModel()$createSurveyObject(),
                TRUE
            )
            if (inherits(setOK, "try-error")) {
                gmessage("Something went wrong during post stratification ...",
                    type = "error"
                )
            }
        }
    )
)
