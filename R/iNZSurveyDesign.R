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

                as.int <- function(x) is.numeric(x) && all(floor(x) == x)
                ints <- sapply(GUI$getActiveData(), as.int)
                vars <- names(GUI$getActiveData())[ints]

                freqVar <<- gcombobox(vars, selected = 0, container = gg)
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
        mthd = "ANY",
        PSvar = "ANY",
        PSlvls = "ANY",
        lvldf = "data.frame",
        okBtn = "ANY"
    ),
    methods = list(
        initialize = function(gui, .use_ui = TRUE) {
            initFields(GUI = gui)

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
                width = 380,
                height = 350,
                visible = FALSE
            )
            g <- gvbox(container = win)
            g$set_borderwidth(5)

            lbl <- glabel("Specify post stratification",
                container = g)
            font(lbl) <- list(size = 11, weight = "bold")

            tbl <- glayout(container = g)
            ii <- 1

            lbl <- glabel("Method :")
            mthd <<- gcombobox(c("Post stratification", "Rake (not available yet)"))
            tbl[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
            tbl[ii, 2, expand = TRUE] <- mthd
            ii <- ii + 1

            ## also only those with no missing values ...
            factorvars <- names(GUI$getActiveData())[sapply(
                GUI$getActiveData(),
                function(v) 
                    length(levels(v)) > 0 && sum(is.na(v)) == 0
            )]
            lbl <- glabel("Choose factor variable :")
            PSvar <<- gcombobox(factorvars, selected = 0)
            tbl[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
            tbl[ii, 2, expand = TRUE] <- PSvar
            ii <- ii + 1

            addSpace(g, 5)

            ## choose from file button
            tbl[ii, 2, expand = TRUE] <- gbutton("Read from file ...",
                handler = function(h, ...) {
                    f <- gfile(
                        type = "open", 
                        filter = c("csv" = "csv")
                    )
                    df <- read.csv(f)
                    if (all(dim(df) != dim(PSlvls) + c(-1, 0))) {
                        gmessage("File needs to have 2 columns and one row for each levels of the chosen factor variable.")
                        return()
                    }
                    names(df) <- c(svalue(PSvar), "Freq")
                    update_levels(df)
                }
            )
            ii <- ii + 1

            lbl <- glabel("File must contain one column of factors levels, and one column of frequencies.")
            font(lbl) <- list(size = 8)
            tbl[ii, 1:2, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
            ii <- ii + 1

            ## table to populate with levels of variable
            gl <- ggroup(container = g)
            addSpring(gl)
            PSlvls <<- glayout(container = gl)
            addSpace(gl, 20)

            addHandlerChanged(PSvar, function(h, ...) update_levels())

            # save/cancel buttons
            addSpring(g)
            btnGrp <- ggroup(container = g)
            addSpring(btnGrp)
            okBtn <<- gbutton("OK",
                handler = function(h, ...) {
                    # call postStratify
                    set_poststrat_design()
                    dispose(win)
                },
                container = btnGrp
            )
            cnclBtn <- gbutton("Cancel", 
                handler = function(h, ...) {
                    dispose(win)
                },
                container = btnGrp
            )


            visible(win) <<- TRUE

            invisible(NULL)
        },
        update_levels = function(df = NULL) {
            # remove existing children from PSlvls
            if (length(PSlvls$children))
                sapply(PSlvls$children, PSlvls$remove_child)
            
            # for each level of PSvar, create a row
            if (is.null(df)) {
                lvldf <<- data.frame(
                    v = levels(GUI$getActiveData()[[svalue(PSvar)]]),
                    Freq = NA
                )
                names(lvldf)[1] <<- svalue(PSvar)
            } else {
                lvldf <<- df
            }

            lbl <- glabel(svalue(PSvar))
            font(lbl) <- list(weight = "bold")
            PSlvls[1, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <<- lbl
            lbl <- glabel("Frequency")
            font(lbl) <- list(weight = "bold")
            PSlvls[1, 2, expand = TRUE, fill = FALSE, anchor = c(-1, 0)] <<- lbl

            for (i in seq_along(1:nrow(lvldf))) {
                PSlvls[i + 1, 1, expand = TRUE, fill = TRUE, anchor = c(1, 0)] <<- 
                    glabel(lvldf[i, 1])
                PSlvls[i + 1, 2] <<- gedit(
                    ifelse(is.na(lvldf[i, 2]), "", lvldf[i, 2]),
                    width = 20
                )
                # change handler for each of these too
            }

            invisible(NULL)
        },
        set_poststrat_design = function() {
            curDes <- GUI$getActiveDoc()$getModel()$getDesign()
            GUI$getActiveDoc()$getModel()$setDesign(
                curDes$strat, curDes$clus1, curDes$clus2, 
                curDes$wts, curDes$nest, curDes$fpc, 
                curDes$repWts, 
                poststrat = lvldf,
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
