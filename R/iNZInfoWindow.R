iNZInfoWindow <- setRefClass(
    "iNZInfoWindow",
    fields = list(
        GUI = "ANY",
        env = "ANY",
        dataname = "ANY", designname = "ANY",
        curSet = "ANY", curMod = "ANY",
        win = "ANY",
        control_position = "character",
        info_text = "ANY", info_font = "list",
        ctrl_panel = "ANY",
        code_panel = "ANY", code_box = "ANY",
        font_size = "numeric"
    ),
    methods = list(
        initialize = function(gui, controls = c("bottom", "top"),
                              name = "Information Window") {
            initFields(
                GUI = gui,
                control_position = controls,
                font_size = gui$preferences$font.size
            )

            # Check that the data exists
            env <<- new.env()
            curSet <<- GUI$getActiveDoc()$getSettings()
            if (is.null(curSet$x)) {
                gmessage("No variable selected.")
                dispose(win)
                return()
            }
            gen_set_list()

            win <<- gwindow(title = name,
                width = 900 * font_size / 10,
                height = 600 * font_size / 10,
                parent = GUI$win,
                visible = FALSE
            )

            code_panel <<- ggroup()
            code_box <<- gtext("info_function(...)",
                expand = TRUE,
                wrap = FALSE,
                font.attr = list(
                    family = "monospace",
                    size = font_size
                ),
                container = code_panel
            )

            info_font <<- list(
                family = "monospace",
                size = font_size
            )
            info_text <<- gtext(
                text = "",
                wrap = FALSE,
                font.attr = info_font
            )

            ctrl_panel <<- ggroup()
            ctrl_panel$set_borderwidth(5)


            # Main container will consist of three components:
            #  1. code panel (can be toggled; controls info)
            #  2. info text
            #  3. control panel (controls code)
            g <- gvbox(spacing = 0, container = win)

            if (controls == "top") add(g, ctrl_panel)
            if (GUI$preferences$dev.features) {
                add(g, code_panel)
                addSpace(g, 5)
            }
            add(g, info_text, expand = TRUE)
            if (controls == "bottom") add(g, ctrl_panel)

        },
        gen_set_list = function() {
            "Generate the initial settings list"
            dataname <<- GUI$dataNameWidget$datName
            designname <<- NULL
            curSet$data <<- as.name(dataname)
            curSet$data_name <<- dataname
            ## Design or data?
            curMod <<- GUI$getActiveDoc()$getModel()
            assign(dataname, GUI$getActiveData(), envir = env)

            if (!is.null(curMod$dataDesign)) {
                curSet$data <<- NULL
                curSet$design <<- as.name(".design")
                env$.design <<- curMod$createSurveyObject()
                # designname <<- curMod$dataDesignName
                # curSet$design <<- as.name(designname)
                # assign(designname, curMod$createSurveyObject(), envir = env)
            }
        }
    )
)


## A summary window
iNZGetSummary <- setRefClass(
    "iNZGetSummary",
    contains = "iNZInfoWindow",
    fields = list(
    ),
    methods = list(
        initialize = function(gui) {
            callSuper(gui, controls = "bottom", name = "Summary")

            smry_call <- gen_call()
            svalue(code_box) <<- smry_call
            font(code_box) <<- info_font

            smry <- eval(smry_call, env)
            svalue(info_text) <<- paste(smry, collapse = "\n")
            font(info_text) <<- info_font

            ## Control panel
            setup_panel()

            visible(win) <<- TRUE
        },
        gen_call = function() {
            "Generate the function call based on user's chosen vars/settings"

            # This will, at some stage, fetch values from the CODE CALL
            # when it is modified by the user ... and update curSet ... =]

            construct_call(curSet, curMod,
                data = as.name(dataname),
                what = "summary"
            )
        },
        setup_panel = function() {
            ds <- GUI$getActiveData()
            xvar <- ds[[curSet$x]]
            yvar <- if (!is.null(curSet$y)) ds[[curSet$y]] else NULL
            if (is.null(curSet$g1) &&
                is.null(curSet$g2) &&
                !is.null(curSet$y) &&
                (is_num(xvar) | is_num(yvar)) &&
                (!is.null(curSet$trend) | curSet$smooth > 0 |
                    !is_num(xvar) | !is_num(yvar))) {

                # addSpace(ctrl_panel, 5)

                btnHandler <- function(h, ...) {
                    varType <- ifelse(
                        grepl("residuals", svalue(h$obj)),
                        "residual",
                        "predict"
                    )

                    ## window asking for variable names:
                    w2 <- gwindow("Store fitted values", width = 350,
                                    parent = win, visible = FALSE)

                    g2 <- gvbox(container = w2)
                    g2$set_borderwidth(15)

                    scatter <- is_num(xvar) && is_num(yvar)

                    lbl <- glabel(
                        sprintf(
                            "Specify names for the new variable%s",
                            ifelse(scatter && length(curSet$trend) > 1, "s", "")),
                        container = g2,
                        anchor = c(-1, -1)
                    )
                    font(lbl) <- list(size = 12, weight = "bold")

                    addSpace(g2, 20)


                    tbl <- glayout(container = g2)
                    ii <- 1

                    ## Predicted values for GROUP MEANS:
                    fittedLbl <- glabel("")
                    fittedName <- gedit(
                        sprintf(
                            "%s.%s",
                            curSet$varnames[[ifelse(is_num(yvar), "y", "x")]],
                            varType),
                        width = 25
                    )

                    if (is_cat(xvar) || is_cat(yvar)) {
                        tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl
                        tbl[ii, 4:6, expand = TRUE] <- fittedName
                        ii <- ii + 1
                    }

                    ## Predicted values for LINEAR trend:
                    fittedLbl.lin <- glabel(
                        ifelse(length(curSet$trend) > 1, "Linear :", "")
                    )
                    fittedName.lin <- gedit(
                        sprintf("%s.%s%s", curSet$varnames$y, varType,
                                ifelse(length(curSet$trend) > 1, ".linear", "")),
                        width = 25
                    )
                    if (scatter && length(curSet$trend) >= 1 && "linear" %in% curSet$trend) {
                        tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl.lin
                        tbl[ii, 4:6, expand = TRUE] <- fittedName.lin
                        ii <- ii + 1
                    }

                    ## Predicted values for QUADRATIC trend:
                    fittedLbl.quad <- glabel(
                        ifelse(length(curSet$trend) > 1, "Quadratic :", "")
                    )
                    fittedName.quad <- gedit(
                        sprintf("%s.%s%s", curSet$varnames$y, varType,
                                ifelse(length(curSet$trend) > 1, ".quadratic", "")),
                        width = 25
                    )
                    if (scatter && length(curSet$trend) >= 1 && "quadratic" %in% curSet$trend) {
                        tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl.quad
                        tbl[ii, 4:6, expand = TRUE] <- fittedName.quad
                        ii <- ii + 1
                    }

                    ## Predicted values for CUBIC trend:
                    fittedLbl.cub <- glabel(
                        ifelse(length(curSet$trend) > 1, "Cubic :", "")
                    )
                    fittedName.cub <- gedit(
                        sprintf("%s.%s%s", curSet$varnames$y, varType,
                                ifelse(length(curSet$trend) > 1, ".cubic", "")),
                        width = 25
                    )
                    if (scatter && length(curSet$trend) >= 1 && "cubic" %in% curSet$trend) {
                        tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl.cub
                        tbl[ii, 4:6, expand = TRUE] <- fittedName.cub
                        ii <- ii + 1
                    }

                    ## Predicted values for SMOOTHER:
                    fittedLbl.smth <- glabel("Smoother :")
                    fittedName.smth <- gedit(
                        sprintf("%s.%s.smooth", curSet$varnames$y, varType),
                        width = 25
                    )
                    if (scatter && curSet$smooth > 0 && is_num(xvar) &&
                        is_num(yvar)) {
                        tbl[ii, 1:3, anchor = c(1, 0), expand = TRUE] <- fittedLbl.smth
                        tbl[ii, 4:6, expand = TRUE] <- fittedName.smth
                        ii <- ii + 1
                    }

                    addSpring(g2)

                    okBtn <- gbutton(
                        "Ok",
                        icon = "save",
                        handler = function(h, ...) {
                            FUN <-
                                if (varType == "predict")
                                    function(object)
                                        predict(
                                            object,
                                            newdata = data.frame(x = xvar, stringsAsFactors = TRUE)
                                        )
                                else
                                    function(object)
                                        residuals(object)

                            pred <- NULL
                            if (is_cat(xvar) || is_cat(yvar)) {
                                ## just the one
                                fit <- lm(if (is_num(yvar)) yvar ~ xvar else xvar ~ yvar, na.action = na.exclude)
                                pred <- data.frame(FUN(fit), stringsAsFactors = TRUE)
                                colnames(pred) <- svalue(fittedName)
                            } else if (length(curSet$trend) >= 1) {
                                ## for each trend line
                                fits <- lapply(curSet$trend, function(ord) {
                                    switch(ord,
                                        "linear"    = lm(yvar ~ xvar, data = ds, na.action = na.exclude),
                                        "quadratic" = lm(yvar ~ xvar + I(x^2), data = ds, na.action = na.exclude),
                                        "cubic"     = lm(yvar ~ xvar + I(x^2) + I(x^3), data = ds, na.action = na.exclude)
                                    )
                                })
                                pred <- sapply(fits, function(f) FUN(f))
                                colnames(pred) <- sapply(curSet$trend, function(ord) {
                                    switch(ord,
                                        "linear" = svalue(fittedName.lin),
                                        "quadratic" = svalue(fittedName.quad),
                                        "cubic" = svalue(fittedName.cub))
                                })
                            }
                            if (!is.null(pred))
                                newdata <- data.frame(GUI$getActiveData(), pred, stringsAsFactors = TRUE)
                            else
                                newdata <- GUI$getActiveData()


                            if (curSet$smooth > 0 && is_num(xvar) && is_num(yvar)) {
                                tmp <- data.frame(x = xvar, y = yvar, stringsAsFactors = TRUE)
                                fit <- with(curSet, loess(y ~ x, span = curSet$smooth, family = "gaussian", degree = 1, na.action = "na.exclude"))
                                pred <- data.frame(FUN(fit), stringsAsFactors = TRUE)
                                colnames(pred) <- svalue(fittedName.smth)
                                newdata <- data.frame(newdata, pred, stringsAsFactors = TRUE)
                            }


                            GUI$getActiveDoc()$getModel()$updateData(newdata)

                            dispose(w2)
                        },
                        container = g2)

                    visible(w2) <- TRUE
                }

                predBtn <- gbutton("Store fitted values",
                    container = ctrl_panel,
                    handler = btnHandler
                )
                residBtn <- gbutton("Store residuals",
                    container = ctrl_panel,
                    handler = btnHandler
                )

                # addSpace(g, 0)
            }
        }
    )
)


## A summary window
iNZGetInference <- setRefClass(
    "iNZGetInference",
    contains = "iNZInfoWindow",
    fields = list(
        inf_method = "ANY",
        hypothesis_test = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            callSuper(gui, controls = "bottom", name = "Inference")

            update_inference()

            ## Control panel
            setup_panel()

            visible(win) <<- TRUE
        },
        gen_call = function() {
            "Generate the function call based on user's chosen vars/settings"

            # This will, at some stage, fetch values from the CODE CALL
            # when it is modified by the user ... and update curSet ... =]

            construct_call(curSet, curMod,
                data = as.name(dataname),
                what = "inference"
            )
        },
        update_inference = function() {
            ## display a message about bootstrapping
            if (curSet$bs.inference) {
                svalue(info_text) <<- "Performing bootstraps ... "
                font(info_text) <<- info_font
                Sys.sleep(0.1)
            }

            smry_call <- gen_call()
            svalue(code_box) <<- smry_call
            font(code_box) <<- info_font

            smry <- try(eval(smry_call, env), silent = TRUE)
            if (inherits(smry, "try-error")) smry <- "Unable to generate inference."
            svalue(info_text) <<- paste(smry, collapse = "\n")
            font(info_text) <<- info_font
        },
        setup_panel = function() {
            ## this depends on the type of analysis going on
            ds <- GUI$getActiveData()
            xvar <- ds[[curSet$x]]
            yvar <- if (!is.null(curSet$y)) ds[[curSet$y]] else NULL

            xnum <- is_num(xvar)
            ynum <- is_num(yvar)
            if (is.null(yvar)) {
                INFTYPE <- ifelse(xnum, "onesample-ttest", "oneway-table")
            } else {
                if (xnum && ynum) {
                    INFTYPE <- "regression"
                } else if (xnum | ynum) {
                    M <-
                        if (xnum) length(levels(yvar))
                        else length(levels(xvar))
                    if (M == 2) INFTYPE <- "twosample-ttest"
                    if (M > 2) INFTYPE <- "anova"
                } else {
                    INFTYPE <- "twoway-table"
                }
            }

            # curMod <- getActiveDoc()$getModel()
            is_survey <- !is.null(curMod$dataDesign)

            if (is_survey) {
                inf_method <<- gradio("Normal theory")
            } else {
                ## Inference method
                g_method <- gvbox(container = ctrl_panel)
                lbl <- glabel("Inference method",
                    container = g_method,
                    anchor = c(-1, 0)
                )
                font(lbl) <- list(weight = "bold")

                inf_method <<- gradio(c("Normal theory", "Bootstrap"),
                    horizontal = FALSE,
                    container = g_method,
                    handler = function(h, ...) {
                        curSet$bs.inference <<- svalue(h$obj, index = TRUE) == 2L
                        update_inference()
                    }
                )
            }

            # hypothesis testing (all except regression, for now)
            do_hyp_test <- INFTYPE %notin% c("regression")
            if (is_survey && do_hyp_test && INFTYPE == "oneway-table") {
                # survey lets us do prop.test, but not chi-square (one-way)
                do_hyp_test <- length(levels(xvar)) == 2
            }

            if (do_hyp_test) {
                addSpace(ctrl_panel, 20)
                g_hypothesis <- gvbox(container = ctrl_panel)
                lbl <- glabel("Hypothesis test",
                    container = g_hypothesis,
                    anchor = c(-1, 0)
                )
                font(lbl) <- list(weight = "bold")

                hyp_tests <- switch(INFTYPE,
                    "onesample-ttest" = "t.test",
                    "twosample-ttest" = c("t.test2", "anova"),
                    "anova" = "anova",
                    "oneway-table" =
                        if (is_survey) "proportion"
                        else if (length(levels(xvar)) == 2L) c("proportion", "chi2")
                        else "chi2",
                    "twoway-table" = "chi2"
                )

                test_names <- c(
                    t.test = "One sample t-test",
                    t.test2 = "Two sample t-test",
                    anova = "ANOVA",
                    proportion = "Test proportion",
                    chi2 = "Chi-square test"
                )

                test_options <- c("None", test_names[hyp_tests])

                hypothesis_test <<- gradio(test_options,
                    horizontal = FALSE,
                    container = g_hypothesis,
                    handler = function(h, ...) {

                    }
                )
            }

        }
    )
)
