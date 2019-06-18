iNZSurveyDesign <- setRefClass(
    "iNZSurveyDesign",
    fields = list(
        GUI = "ANY",
        designWin = "ANY",
        freqVar = "ANY",
        stratVar = "ANY",
        clus1Var = "ANY",
        clus2Var = "ANY",
        nestChk = "ANY",
        wtVar = "ANY",
        fpcVar = "ANY",
        useRep = "ANY",
        repG = "ANY",
        repVars = "ANY",
        createBtn = "ANY",
        cancelBtn = "ANY"
    ),
    methods = list(
        initialize = function(GUI, freq = FALSE, warn = TRUE) {
            initFields(GUI = GUI)

            if (is.null(GUI$getActiveData())) {
                gerror("Please import a data set first.",
                    title = "No data set", icon = "error")
                return()
            } else if ((names(GUI$getActiveData())[1] == "empty")) {
                gmessage("Please import a data set first.",
                    title = "No data set", icon = "error")
                return()
            }

#             if (!freq && warn) {
#                 gmessage(
#                     paste(
# "The Survey functionality is still under development.",
# "Please use with caution and for experimentation only.",
# "\n\nIf you discover any bugs, let us know by emailing",
# "inzight_support@stat.auckland.ac.nz."
#                     ),
#                     title = "Survey Analysis BETA",
#                     parent = GUI$win,
#                     icon = "warning"
#                 )
#             }

            if (freq) {
                designWin <<-
                    gwindow("Specify frequency column",
                        parent = GUI$win,
                        width = 450,
                        height = 150,
                        visible = FALSE
                    )
                gg <- gvbox(container = designWin, expand = TRUE)
                gg$set_borderwidth(5)

                lbl <- glabel("Choose frequency column", cont = gg)
                font(lbl) <- list(weight = "bold", size = 11)

                addSpace(gg, 5)

                as.int <- function(x) {
                    is.numeric(x) && all(floor(x) == x, na.rm = TRUE)
                }
                ints <- sapply(GUI$getActiveData(), as.int)
                vars <- names(GUI$getActiveData())[ints]

                freqVar <<- gcombobox(vars, selected = 0, container = gg)

                addSpace(gg, 5)
                lbl <- glabel("WARNING: any non-categorical variables will be removed")
                font(lbl) <- list(weight = "bold", size = 9)
                add(gg, lbl, expand = TRUE, anchor = c(-1, 0))
            } else {
                designWin <<-
                    gwindow("Survey Design", parent = GUI$win,
                        width = 450, height = 300, visible = FALSE)
                gg <- gvbox(container = designWin, expand = TRUE)
                gg$set_borderwidth(5)

                ttl <- glabel("Specify Survey Design", cont = gg)
                font(ttl) <- list(weight = "bold", size = 11)

                addSpace(gg, 5)
                tbl <- glayout(cont = gg)

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

                ii <- ii + 2
                useRep <<- gcheckbox("Specify replicate weights")
                tbl[ii, 1:2, expand = TRUE] <- useRep

                ii <- ii + 1
                repG <<- ggroup()
                repVars <<- gtable(vars, multiple = TRUE, container = repG)
                size(repVars) <<- c(-1, 120)
                tbl[ii, 1:2, expand = TRUE] <- repG
                visible(repG) <<- FALSE

                addHandlerChanged(useRep, function(h, ...) {
                    visible(repG) <<- svalue(useRep)
                })

            }

            addSpring(gg)

            btnGrp <- ggroup(cont = gg)
            addSpace(btnGrp, 10)
            #advancedBtn <- gbutton("Advanced", cont = btnGrp)
            addSpring(btnGrp)
            cancelBtn <<- gbutton("Cancel", cont = btnGrp)
            addSpace(btnGrp, 10)
            createBtn <<- gbutton("OK", cont = btnGrp)
            addSpace(btnGrp, 10)

            addSpace(gg, 10)

            #addHandlerClicked(advancedBtn, handler = function(h, ...) {
            #})
            addHandlerClicked(cancelBtn, handler = function(h, ...) {
                dispose(designWin)
            })
            addHandlerClicked(createBtn, handler = function(h, ...) {
                if (freq) {
                    freqv <- svalue(freqVar, index = FALSE)
                    if (freqv == "") freqv <- NULL
                    GUI$getActiveDoc()$getModel()$setFrequencies(
                        freq = freqv, gui = GUI
                    )
                    dispose(designWin)
                    return()
                }

                strat <- svalue(stratVar, index = FALSE)
                clus1 <- svalue(clus1Var, index = FALSE)
                clus2 <- svalue(clus2Var, index = FALSE)
                wts <- svalue(wtVar, index = FALSE)
                fpc <- svalue(fpcVar, index = FALSE)
                nest <- as.logical(svalue(nestChk))
                repWts <- ""
                if (svalue(useRep)) repWts <- svalue(repVars, index = FALSE)

                if (strat == "") strat <- NULL
                if (clus1 == "") clus1 <- NULL
                if (clus2 == "") clus2 <- NULL
                if (wts == "") wts <- NULL
                if (fpc == "") fpc <- NULL
                if (length(repWts) == 0 || all(repWts == "")) repWts <- NULL

                GUI$getActiveDoc()$getModel()$setDesign(
                    strat, clus1, clus2, wts, nest, fpc, repWts, gui = GUI
                )
                setOK <- try(
                    GUI$getActiveDoc()$getModel()$createSurveyObject(),
                    TRUE
                )

                if (!inherits(setOK, "try-error")) {
                    if (!freq && is.null(strat) && is.null(clus1) &&
                        is.null(clus2) && is.null(wts) && is.null(fpc) &&
                        is.null(repWts) &&
                        !freq) {
                        ## ENABLE A WHOLE LOT OF STUFF
                        # enabled(GUI$menubar$menu_list[["Dataset"]][[3]]) <<- TRUE
                        # enabled(GUI$menubar$menu_list[["Variables"]][["Numeric Variables"]][[2]]) <<- TRUE
                        # enabled(GUI$menubar$menu_list[["Plot"]][[3]]) <<- TRUE
                        #enabled(GUI$sumBtn) <<- TRUE
                        enabled(GUI$infBtn) <<- TRUE
                    } else {
                        ## DISABLE A WHOLE LOT OF STUFF
                        # enabled(GUI$menubar$menu_list[["Dataset"]][[3]]) <<- FALSE
                        # enabled(GUI$menubar$menu_list[["Variables"]][["Numeric Variables"]][[2]]) <<- FALSE
                        # enabled(GUI$menubar$menu_list[["Plot"]][[3]]) <<- FALSE
                        ##enabled(GUI$sumBtn) <<- FALSE
                        enabled(GUI$infBtn) <<- FALSE
                    }

                    dispose(designWin)

                    ## write design call
                    call <- deparse(setOK$call)

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
                if (freq) {
                    if (!is.null(curDes$freq))
                        svalue(freqVar) <<- curDes$freq
                } else {
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
                    if (!is.null(curDes$repweights)) {
                        svalue(useRep) <<- TRUE
                        svalue(repVars) <<- curDes$repweights
                    }
                }
            }

           visible(designWin) <<- TRUE
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
        cancelBtn = "ANY"
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
            if (!is.null(curDes$poststrat)) {
                svalue(PSvar) <<- names(curDes$poststrat)
                lvldf <<- curDes$poststrat
                display_tbl()
            }

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
                        b = NA
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

                        df <- read.csv(f)
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

                btn <- gbutton("Read from clipboard ...")
                PSlvls[ii, 3, anchor = c(1, 0)] <<- btn

                
                
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
