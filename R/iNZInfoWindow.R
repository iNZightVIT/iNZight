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
        button_width = "numeric", button_height = "numeric",
        code_font = "list",
        original_code = "character",
        store_btn = "ANY", run_btn = "ANY", reset_btn = "ANY",
        font_size = "numeric",
        # privacy control window
        suppress = "ANY",
        secondarySuppression = "ANY",
        round = "ANY",
        roundVal = "ANY",
        suppressMeans = "ANY",
        suppressMedian = "ANY",
        suppressQuartiles = "ANY",
        suppressRse = "ANY"
    ),
    methods = list(
        initialize = function(gui, controls = c("bottom", "top"),
                              name = "Information Window") {
            initFields(
                GUI = gui,
                control_position = controls,
                font_size = gui$preferences$font.size,
                button_width = 80, button_height = 25,
                code_font = list(family = "monospace", size = 10),
                original_code = ""
            )

            # Check that the data exists
            env <<- new.env()
            curSet <<- GUI$getActiveDoc()$getSettings()
            gen_set_list()

            win <<- gwindow(title = name,
                width = 900 * font_size / 10,
                height = 600 * font_size / 10,
                parent = GUI$win,
                visible = FALSE
            )

            code_panel <<- gvbox()
            code_panel$set_borderwidth(5)
            code_box <<- gtext("info_function(...)",
                expand = TRUE,
                wrap = FALSE,
                font.attr = code_font,
                container = code_panel
            )
            RGtk2::gtkTextViewSetLeftMargin(code_box$widget, 0)
            RGtk2::gtkTextViewSetRightMargin(code_box$widget, 0)

            code_btns <- ggroup(container = code_panel)
            lbl <- glabel("R code is shown above, which can be edited and run.")
            font(lbl) <- list(size = 9, weight = "bold")
            add(code_btns, lbl, anchor = c(-1, 0))

            addSpring(code_btns)
            btn_pnl <- ggroup(container = code_btns)
            store_btn <<- gbutton("Store",
                container = code_btns,
                handler = function(h, ...) store_code()
            )
            run_btn <<- gbutton("Run",
                container = code_btns,
                handler = function(h, ...) run_code()
            )
            reset_btn <<- gbutton("Reset",
                container = code_btns,
                handler = function(h, ...) reset_code()
            )
            store_btn$set_icon("rlogo")
            run_btn$set_icon("go")
            reset_btn$set_icon("reset")
            size(store_btn) <<- c(button_width, button_height)
            size(run_btn) <<- c(button_width, button_height)
            size(reset_btn) <<- c(button_width, button_height)
            font(store_btn) <<- list(size = 9)
            font(run_btn) <<- list(size = 9)
            font(reset_btn) <<- list(size = 9)

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
            if (GUI$preferences$dev.features && GUI$preferences$show.code) {
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
                designname <<- curMod$dataDesignName
                curSet$data <<- NULL
                curSet$design <<- as.name(designname)
                assign(designname, curMod$createSurveyObject(), envir = env)
                env$.design <<- curMod$createSurveyObject()
            }
        },
        set_input = function(code) {
            original_code <<- code
            svalue(code_box) <<- ""
            insert(code_box, code, where = "beginning", font.attr = code_font)
            enabled(store_btn) <<- enabled(run_btn) <<- enabled(reset_btn) <<-
                svalue(code_box) != ""
        },
        set_output = function(out) {
            svalue(info_text) <<- ""
            gWidgets2::insert(
                info_text,
                paste(out, collapse = "\n"),
                where = "beginning",
                font.attr = info_font
            )
            # font(info_text) <<- info_font
        },
        store_code = function() {
            GUI$rhistory$add(svalue(code_box))
        },
        run_code = function(handle = function(...) NULL) {
            # set code environment
            assign(
                GUI$dataNameWidget$datName,
                GUI$getActiveData(),
                GUI$code_env
            )

            if (!is.null(GUI$getActiveDoc()$getModel()$dataDesign)) {
                assign(
                    GUI$getActiveDoc()$getModel()$dataDesignName,
                    GUI$getActiveDoc()$getModel()$createSurveyObject(),
                    GUI$code_env
                )
            }

            tryCatch(
                {
                    if (grepl("skimr", svalue(code_box))) {
                        olocale <- Sys.getlocale("LC_CTYPE")
                        owidth <- getOption("width")
                        if (GUI$OS == "windows") {
                            Sys.setlocale("LC_CTYPE", "Chinese")
                        }
                        options(width = 200)
                        on.exit({
                            Sys.setlocale("LC_CTYPE", olocale)
                            options(width = owidth)
                        })
                    }
                    output <-
                        if (grepl("skimr", svalue(code_box))) {
                            capture.output(
                                eval(
                                    parse(text = svalue(code_box)),
                                    envir = GUI$code_env
                                )
                            )
                        } else {
                            eval(
                                parse(text = svalue(code_box)),
                                envir = GUI$code_env
                            )
                        }
                },
                error = function(e) {
                    gmessage(
                        sprintf("There was an error in your code:\n\n%s", e$message),
                        title = "Error",
                        icon = "error",
                        parent = win
                    )
                }
            )

            if (!exists("output")) return()


            if (!inherits(output, "inzight.plotsummary") && !grepl("skimr", svalue(code_box))) {
                gmessage(
                    "The code you entered did not produce the appropriate output",
                    title = "Invalid output",
                    icon = "warning",
                    parent = win
                )
                return()
            }

            set_output(output)

            # the handle function will update controls ...
            handle(output)
        },
        reset_code = function() {
            set_input(original_code)
            run_code()
        }
    )
)

## Dataset info
iNZDataSummary <- setRefClass(
    "iNZDataSummary",
    contains = "iNZInfoWindow",
    fields = list(
        page = "integer", pagesize = "integer"
    ),
    methods = list(
        initialize = function(gui) {
            if (is.null(gui$getActiveData()) || all(dim(gui$getActiveData()) == 1L)) return()
            callSuper(gui, controls = "top", name = "Dataset Summary")
            initFields(page = 1L, pagesize = 100L)
            setup_panel()
            visible(win) <<- TRUE
        },
        gen_call = function() {
            "Generate summary call"
            d <- GUI$get_data_object()
            sprintf("%sskimr::skim(%s%s)",
                ifelse(iNZightTools::is_survey(d),
                    sprintf("print(%s, design.summaries = TRUE)\n", designname),
                    ""
                ),
                dataname,
                ifelse(ncol(d) > pagesize,
                    sprintf(", %s:%s", pagesize * (page - 1) + 1, min(ncol(d), pagesize * page)),
                    ""
                )
            )
        },
        update_summary = function(...) {
            # the following is required to ensure the output graphs look OK,
            # and that the rows are all on one line
            olocale <- Sys.getlocale("LC_CTYPE")
            owidth <- getOption("width")
            if (GUI$OS == "windows") {
                Sys.setlocale("LC_CTYPE", "Chinese")
            }
            options(width = 200)
            on.exit({
                Sys.setlocale("LC_CTYPE", olocale)
                options(width = owidth)
            })

            smry_call <- gen_call()
            set_input(smry_call)

            smry_call <- strsplit(smry_call, "\n", fixed = TRUE)[[1]]

            smry <- try(
                lapply(smry_call,
                    function(c) capture.output(eval(parse(text = c), env))
                ),
                silent = TRUE
            )
            if (length(smry) > 1L) {
                smry[-length(smry)] <- lapply(smry[-length(smry)],
                    function(s) {
                        c(s, "", paste(rep("-", 100), collapse = ""), "", "")
                    }
                )
            }
            smry <- do.call(c, smry)
            if (inherits(smry, "try-error")) smry <- "Unable to generate summary."
            set_output(smry)
        },
        setup_panel = function() {
            ds <- GUI$getActiveData()
            N <- ncol(ds)
            if (ncol(ds) <= pagesize) {
                update_summary()
                return()
            }

            g <- gvbox(container = ctrl_panel)
            npage <- ceiling(N / pagesize)
            lbl <- glabel(
                "Summary shown for 100 variables at a time. Use slider to page through.",
                container = g
            )
            sld <- gslider(1L, npage,
                value = page,
                handler = function(h, ...) {
                    page <<- as.integer(svalue(h$obj))
                    update_summary()
                },
                container = g
            )

            update_summary()
        }
    )
)


## A summary window
iNZGetSummary <- setRefClass(
    "iNZGetSummary",
    contains = "iNZInfoWindow",
    fields = list(
        predBtn = "ANY",
        residBtn = "ANY",
        trend = "list",
        trend_menu = "ANY",
        tableDir = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            if (is.null(gui$getActiveDoc()$getSettings()$x)) return()
            callSuper(gui, controls = "bottom", name = "Summary")

            ## Control panel
            setup_panel()

            visible(win) <<- TRUE
        },
        gen_call = function() {
            "Generate the function call based on user's chosen vars/settings"

            # This will, at some stage, fetch values from the CODE CALL
            # when it is modified by the user ... and update curSet ... =]
            vartypes <- list(
                x = NULL,
                y = NULL
            )
            if (!is.null(curSet$x))  {
                vartypes$x <- iNZightTools::vartype(GUI$getActiveData()[[curSet$x]])
                if (!is.null(curSet$y))
                    vartypes$y <- iNZightTools::vartype(GUI$getActiveData()[[curSet$y]])
            }

            construct_call(curSet, curMod, vartypes,
                data = as.name(dataname),
                what = "summary"
            )
        },
        update_summary = function(...) {
            smry_call <- gen_call()
            smry_call_list <- as.list(smry_call[[1]])
            smry_call[[1]] <- as.call(modifyList(smry_call_list, list(...)))

            smry <- try(eval(smry_call, env), silent = TRUE)

            if (inherits(smry, "kableExtra")) {
                print(smry)
                return()
            }

            set_input(mend_call(smry_call, GUI))
            if (inherits(smry, "try-error")) smry <- "Unable to generate summary."
            set_output(smry)
        },
        store_values = function(varType = c("predict", "residual")) {
            varType <- match.arg(varType)

            if (is.null(curSet$y)) return()

            ds <- GUI$getActiveData()
            xvar <- ds[[curSet$x]]
            yvar <- ds[[curSet$y]]
            xnum <- is_num(xvar)
            ynum <- is_num(yvar)
            xname <- as.character(curSet$x)
            yname <- as.character(curSet$y)

            # scatter: y <-> x
            # OR
            # dot plot: num ~ cat

            # cat("xnum:", xnum, "\nynum: ", ynum, "\n")
            if ((xnum && ynum) || xnum) {
                xvar <- ds[[curSet$y]]
                yvar <- ds[[curSet$x]]
                xnum <- is_num(xvar)
                ynum <- is_num(yvar)
                xname <- as.character(curSet$y)
                yname <- as.character(curSet$x)
            }

            ## window asking for variable names:
            w2 <- gwindow("Store fitted values",
                width = 350,
                parent = win,
                visible = FALSE
            )

            g2 <- gvbox(container = w2)
            g2$set_borderwidth(15)

            scatter <- xnum && ynum

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
                sprintf("%s.%s", yname, varType),
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
                sprintf("%s.%s%s", yname, varType,
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
                sprintf("%s.%s%s", yname, varType,
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
                sprintf("%s.%s%s", yname, varType,
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
                sprintf("%s.%s.smooth", yname, varType),
                width = 25
            )
            if (scatter && curSet$smooth > 0 && xnum && ynum) {
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
                                predict(object)
                        else
                            function(object)
                                residuals(object)

                    pred <- NULL
                    if (!xnum || !ynum) {
                        ## just the one
                        fit <- lm(yvar ~ xvar, na.action = na.exclude)
                        pred <- data.frame(FUN(fit), stringsAsFactors = TRUE)
                        colnames(pred) <- svalue(fittedName)
                    } else if (length(curSet$trend) >= 1) {
                        ## for each trend line
                        fits <- lapply(curSet$trend,
                            function(ord) {
                                switch(ord,
                                    "linear"    = lm(yvar ~ xvar, na.action = na.exclude),
                                    "quadratic" = lm(yvar ~ xvar + I(xvar^2), na.action = na.exclude),
                                    "cubic"     = lm(yvar ~ xvar + I(xvar^2) + I(xvar^3), na.action = na.exclude)
                                )
                            }
                        )
                        pred <- sapply(fits, function(f) FUN(f))
                        colnames(pred) <- sapply(curSet$trend,
                             function(ord) {
                                switch(ord,
                                    "linear" = svalue(fittedName.lin),
                                    "quadratic" = svalue(fittedName.quad),
                                    "cubic" = svalue(fittedName.cub))
                            }
                        )
                    }
                    if (!is.null(pred))
                        newdata <- data.frame(
                            GUI$getActiveData(),
                            pred,
                            stringsAsFactors = TRUE
                        )
                    else
                        newdata <- GUI$getActiveData()


                    if (curSet$smooth > 0 && xnum && ynum) {
                        fit <- loess(yvar ~ xvar,
                            span = curSet$smooth,
                            family = "gaussian",
                            degree = 1,
                            na.action = "na.exclude"
                        )
                        pred <- data.frame(FUN(fit), stringsAsFactors = TRUE)
                        colnames(pred) <- svalue(fittedName.smth)
                        newdata <- data.frame(newdata, pred, stringsAsFactors = TRUE)
                    }

                    GUI$getActiveDoc()$getModel()$updateData(newdata)

                    dispose(w2)
                },
                container = g2
            )

            visible(w2) <- TRUE
            invisible(w2)
        },
        trend_handler = function(h, ...) {
            ds <- GUI$getActiveData()
            xvar <- ds[[curSet$x]]
            yvar <- if (!is.null(curSet$y)) ds[[curSet$y]] else NULL
            xnum <- is_num(xvar)
            ynum <- is_num(yvar)

            trend[[tolower(h$obj$widget$label)]] <<- svalue(h$obj)
            curSet$trend <<- names(trend)[unlist(trend)]
            if ((is.null(curSet$trend) || length(curSet$trend) == 0) &&
                curSet$smooth == 0) {
                enabled(predBtn) <<- FALSE
                enabled(residBtn) <<- FALSE
            } else {
                enabled(predBtn) <<- TRUE
                enabled(residBtn) <<- TRUE
            }

            # update the plot, too...
            GUI$getActiveDoc()$setSettings(list(trend = curSet$trend))

            update_summary()
        },
        setup_panel = function() {
            if (grepl("^gg_multi", GUI$plotType)) {
                # button to view as HTML
                html_btn <- gbutton("View HTML table",
                    handler = function(h, ...) {
                        update_summary(html = TRUE)
                    },
                    container = ctrl_panel
                )
            }

            ds <- GUI$getActiveData()
            xvar <- if (!is.null(curSet$x)) ds[[curSet$x]] else NULL
            if (is.null(xvar)) {
                update_summary()
                return()
            }
            yvar <- if (!is.null(curSet$y)) ds[[curSet$y]] else NULL

            xnum <- is_num(xvar)
            ynum <- is_num(yvar)

            if (GUI$plotType == "bar") {
                lbl <- glabel("Table direction", container = ctrl_panel)
                tableDir <<- gradio(c("Horizontal", "Vertical"),
                    container = ctrl_panel,
                    selected =
                        if (is.null(curSet$table.direction)) 1L
                        else switch(curSet$table.direction,
                            horizontal = 1L, vertical = 2L),
                    horizontal = TRUE,
                    handler = function(h, ...) {
                        curSet$table.direction <<- tolower(svalue(h$obj))
                        GUI$getActiveDoc()$setSettings(
                            list(table.direction = tolower(svalue(h$obj)))
                        )
                        update_summary()
                    }
                )
            }


            # show predicted/residual buttons?
            if (!is.null(yvar) && (xnum || ynum)) {
                predBtn <<- gbutton("Store fitted values",
                    container = ctrl_panel,
                    handler = function(h, ...) store_values("predict")
                )
                residBtn <<- gbutton("Store residuals",
                    container = ctrl_panel,
                    handler = function(h, ...) store_values("residual")
                )

                # are they visible?
                if (xnum && ynum &&
                    (is.null(curSet$trend) || length(curSet$trend) == 0) &&
                    curSet$smooth == 0) {
                    enabled(predBtn) <<- FALSE
                    enabled(residBtn) <<- FALSE
                }
            }

            # abilty to add/remove trend lines
            if (xnum && ynum) {
                trend_btn <- gbutton("Trend lines ...",
                    container = ctrl_panel
                )
                trend <<- list(
                    linear = "linear" %in% curSet$trend,
                    quadratic = "quadratic" %in% curSet$trend,
                    cubic = "cubic" %in% curSet$trend
                )
                trend_menu <<- gmenu(
                    list(
                        linear = gcheckbox("Linear",
                            checked = trend$linear,
                            handler = .self$trend_handler
                        ),
                        quadratic = gcheckbox("Quadratic",
                            checked = trend$quadratic,
                            handler = .self$trend_handler
                        ),
                        cubic = gcheckbox("Cubic",
                            checked = trend$cubic,
                            handler = .self$trend_handler
                        )
                    ),
                    popup = TRUE
                )
                addPopupMenu(trend_btn, trend_menu)
            }

            addSpring(ctrl_panel)

            settings_button <- gbutton(
                "Output settings",
                container = ctrl_panel,
                handler = function(h, ...) editOutputSettings()
            )
            settings_button$set_icon("preferences")
            tooltip(settings_button) <- "Edit settings for summary output"

            privacy_button <- gbutton(
                "Confidentiality Rules",
                container = ctrl_panel,
                handler = function(h, ...) editPrivacyRules()
            )
            icon <- RGtk2::gtkImage(
                file = system.file("images/icon-privacy.png",
                    package = "iNZight"
                )
            )
            privacy_button$widget$setImage(icon)
            privacy_button$widget$image$show()
            tooltip(privacy_button) <- "Set or change privacy and confidentiality output controls"

            update_summary()
        },
        editOutputSettings = function() {
            w <- gwindow(
                parent = win,
                width = 400,
                height = 150,
                title = "Output settings"
            )

            g <- gvbox(container = w)
            g$set_borderwidth(5)

            lbl <- glabel(
                paste(sep = "\n",
                    "NOTE: work is still in progress on implementing these settings",
                    "in the output, so some may not display."
                ),
                conatiner = g,
                expand = TRUE
            )
            font(lbl) <- list(weight = "bold", size = 8L)

            tbl <- glayout(container = g)
            ii <- 1L

            # TODO: table direction goes here

            ## -- rounding
            round_lbl <- glabel("Round values to ...")
            tbl[ii, 1L, anchor = c(-1, 0), expand = TRUE] <- round_lbl
            ii <- ii + 1L

            g_round <- ggroup()
            tbl[ii, 1L, expand = TRUE] <- g_round

            signif_value <- gspinbutton(1, 20,
                value = curSet$signif,
                container = g_round,
                expand = TRUE,
                handler = function(h, ...) {
                    curSet$signif <<- svalue(h$obj)
                    GUI$getActiveDoc()$setSettings(list(signif = svalue(h$obj)))
                    update_summary()
                }
            )
            round_value <- gspinbutton(0, 5,
                value = ifelse(is.na(curSet$round), 2L, curSet$round),
                container = g_round,
                expand = TRUE,
                handler = function(h, ...) {
                    curSet$round <<- svalue(h$obj)
                    GUI$getActiveDoc()$setSettings(list(round = svalue(h$obj)))
                    update_summary()
                }
            )
            visible(round_value) <- is.na(curSet$round)
            visible(signif_value) <- !visible(round_value)

            round_toggle <- gradio(
                c("significant digits", "decimal places"),
                horizontal = TRUE,
                selected = ifelse(is.na(curSet$round), 1L, 2L),
                handler = function(h, ...) {
                    # TODO: change visibility of round/signif value spinners

                    curSet$signif <<- svalue(signif_value)
                    if (h$obj$get_index() == 1L) {
                        curSet$round <<- NA
                    } else {
                        curSet$round <<- svalue(round_value)
                    }
                    visible(round_value) <- is.na(curSet$round)
                    visible(signif_value) <- !visible(round_value)
                    GUI$getActiveDoc()$setSettings(
                        list(
                            signif = curSet$signif,
                            round = curSet$round
                        )
                    )
                    update_summary()
                }
            )
            tbl[ii, 2:3, expand = TRUE] <- round_toggle
            ii <- ii + 1L

            ## -- rounding percentages
            roundpc_lbl <- glabel("Round percentages to ")
            tbl[ii, 1L, anchor = c(-1, 0), expand = TRUE] <- roundpc_lbl

            roundpc_value <- gspinbutton(0, 4,
                value = curSet$round_percent,
                handler = function(h, ...) {
                    curSet$round_percent <<- svalue(h$obj)
                    GUI$getActiveDoc()$setSettings(list(round_percent = svalue(h$obj)))
                    update_summary()
                }
            )
            tbl[ii, 2L, expand = TRUE] <- roundpc_value
            tbl[ii, 3L, anchor = c(-1, 0), expand = TRUE] <- glabel(" decimal places")
            ii <- ii + 1L

            addSpring(g)
            button_g <- ggroup(container = g)
            addSpring(button_g)
            close_button <- gbutton("Close",
                container = button_g,
                handler = function(h, ...) gWidgets2::dispose(w)
            )
        },
        editPrivacyRules = function() {
            # TODO: This should only display relevant options for the current output type

            w <- gwindow(
                parent = win,
                width = 400,
                height = 150,
                title = "Privacy and Confidentialisation Options"
            )

            pc <- curSet$privacy_controls
            if (is.null(pc)) pc <- list()

            g <- gvbox(container = w)
            g$set_borderwidth(5)

            tbl <- glayout(container = g)
            ii <- 1L

            ## --- suppression of small counts
            suppress <<- gspinbutton(0, 10000, 1,
                value = if (is.null(pc$suppression)) 10L else pc$suppression,
                handler = function(h, ...) {
                    setPrivacyControls()
                }
            )
            enabled(suppress) <<- !is.null(pc$suppression)
            suppressChk <- gcheckbox("Suppress counts smaller than",
                checked = !is.null(pc$suppression),
                handler = function(h, ...) {
                    enabled(suppress) <<- svalue(h$obj)
                    visible(secondarySuppression) <<- svalue(h$obj)
                    setPrivacyControls()
                }
            )

            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- suppressChk
            tbl[ii, 2:3, fill = TRUE] <- suppress
            ii <- ii + 1L

            secondarySuppression <<- gcheckbox("Secondary suppression (row/column totals)",
                handler = function(h, ...) setPrivacyControls()
            )
            tbl[ii, 1:3, anchor = c(1, 0)] <- secondarySuppression
            ii <- ii + 1L
            visible(secondarySuppression) <<- !is.null(pc$rounding)

            ## --- rounding
            round <<- gradio(
                c("RR3", "GRR", "fixed"),
                horizontal = TRUE,
                value = if (is.null(pc$rounding)) "RR3" else pc$rounding,
                handler = function(h, ...) {
                    visible(roundVal) <<- visible(roundValLbl) <-
                        svalue(h$obj) == "fixed"
                    setPrivacyControls()
                }
            )
            enabled(round) <<- !is.null(pc$rounding)
            roundChk <- gcheckbox("Round counts using",
                checked = !is.null(pc$rounding),
                handler = function(h, ...) {
                    enabled(round) <<- svalue(h$obj)
                    visible(roundVal) <<- visible(roundValLbl) <-
                        svalue(h$obj) && svalue(round) == "fixed"
                    setPrivacyControls()
                }
            )

            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- roundChk
            tbl[ii, 2:3, fill = TRUE] <- round
            ii <- ii + 1L

            roundValLbl <- glabel("Round to base ")
            roundVal <<- gspinbutton(0, 100000, by = 1, value = 100,
                handler = function(h, ...) {
                    setPrivacyControls()
                }
            )
            tbl[ii, 1L, anchor = c(1, 0)] <- roundValLbl
            tbl[ii, 2:3, fill = TRUE] <- roundVal
            ii <- ii + 1L
            visible(roundVal) <<- visible(roundValLbl) <-
                !is.null(pc$rounding) && pc$rounding == "fixed"

            ## --- suppression of means based on small counts
            suppressMeans <<- gspinbutton(0, 10000, 1,
                value = if (is.null(pc$suppression_magnitude)) 10L else pc$suppression_magnitude,
                handler = function(h, ...) {
                    setPrivacyControls()
                }
            )
            enabled(suppressMeans) <<- !is.null(pc$suppression_magnitude)
            suppressMeansChk <- gcheckbox("Suppress means and totals based on counts smaller than",
                checked = !is.null(pc$suppression_magnitude),
                handler = function(h, ...) {
                    enabled(suppressMeans) <<- svalue(h$obj)
                    setPrivacyControls()
                }
            )

            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- suppressMeansChk
            tbl[ii, 2:3, fill = TRUE] <- suppressMeans
            ii <- ii + 1L

            ## --- suppression of quantiles
            suppressMedian <<- gspinbutton(0, 10000,
                value = if (is.null(pc$suppression_quantiles)) 10L else {
                    pc$suppression_quantiles$n[pc$suppression_quantiles$p == 0.5]
                },
                handler = function(h, ...) setPrivacyControls()
            )
            suppressQuartiles <<- gspinbutton(0, 10000,
                value = if (is.null(pc$suppression_quantiles)) 20L else {
                    pc$suppression_quantiles$n[pc$suppression_quantiles$p == 0.25]
                },
                handler = function(h, ...) setPrivacyControls()
            )
            enabled(suppressQuartiles) <<- enabled(suppressMedian) <<-
                !is.null(pc$supression_quantiles)
            suppressQuantilesChk <- gcheckbox("Suppress quantiles based on counts smaller than threshold",
                checked = !is.null(pc$supression_quantiles),
                handler = function(h, ...) {
                    enabled(suppressQuartiles) <<- enabled(suppressMedian) <<-
                        svalue(h$obj)
                    setPrivacyControls()
                }
            )

            tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- suppressQuantilesChk
            ii <- ii + 1L

            tbl[ii, 2L, anchor = c(1, 0), expand = TRUE] <- glabel("Median :")
            tbl[ii, 3L, expand = TRUE, fill = TRUE] <- suppressMedian
            ii <- ii + 1L

            tbl[ii, 2L, anchor = c(1, 0), expand = TRUE] <- glabel("25% and 75% quartiles :")
            tbl[ii, 3L, expand = TRUE, fill = TRUE] <- suppressQuartiles
            ii <- ii + 1L


            suppressRseChk <- gcheckbox("Suppress/identify values with RSE greater than ...",
                checked = !is.null(pc$check_rse),
                handler = function(h, ...) {
                    enabled(suppressRse) <<- svalue(h$obj)
                    setPrivacyControls()
                }
            )
            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- suppressRseChk
            ii <- ii + 1L


            rseEg <- glabel(
                paste(sep = "\n",
                    "Example: entering '50=*,80=**,100=S' will mark values with RSE >= 50 with *,",
                    "RSE >= 80 with **, and suppress values with RSE >= 100."
                )
            )
            font(rseEg) <- list(size = 8L)
            tbl[ii, 1L, expand = TRUE] <- rseEg

            rseText <- ""
            if (!is.null(pc$check_rse)) {
                rseText <- paste0(pc$check_rse$cut, "=", pc$check_rse$output, collapse = ", ")
            }
            suppressRse <<- gedit(rseText,
                handler = function(h, ...) setPrivacyControls()
            )
            enabled(suppressRse) <<- !is.null(pc$check_rse)
            tbl[ii, 2:3, expand = TRUE, fill = TRUE] <- suppressRse
            ii <- ii + 1L

            addSpring(g)
            button_g <- ggroup(container = g)
            addSpring(button_g)
            close_button <- gbutton("Close",
                handler = function(h, ...) gWidgets2::dispose(w)
            )
            apply_button <- gbutton("Apply",
                handler = function(h, ...) setPrivacyControls(TRUE)
            )

            add(button_g, apply_button)
            add(button_g, close_button)
        },
        setPrivacyControls = function(update = FALSE) {
            if (!update) return()

            pc <- curSet$privacy_controls
            pc$suppression <- if (enabled(suppress)) svalue(suppress) else NULL
            pc$secondary_suppression <-
                if (enabled(secondarySuppression)) svalue(secondarySuppression) else NULL
            pc$rounding <- if (enabled(round)) {
                if (svalue(round) == "fixed") svalue(roundVal) else svalue(round)
            } else NULL

            pc$suppression_magnitude <- if (enabled(suppressMeans)) svalue(suppressMeans) else NULL
            pc$suppression_quantiles <- if (enabled(suppressMedian)) {
                list(
                    p = c(0.25, 0.5, 0.75),
                    n = c(
                        svalue(suppressQuartiles),
                        svalue(suppressMedian),
                        svalue(suppressQuartiles)
                    )
                )
            } else NULL

            pc$check_rse <- if (enabled(suppressRse)) {
                x <- strsplit(svalue(suppressRse), ",")[[1]]
                x <- sapply(x, strsplit, split = "=")
                x <- as.data.frame(do.call(rbind, x))
                x[[1]] <- as.integer(x[[1]])
                x[[2]] <- ifelse(x[[2]] == "S", "suppress", x[[2]])
                colnames(x) <- c("cut", "output")
                as.list(x)
            } else NULL

            if (length(pc) == 0L || all(sapply(pc, is.null))) pc <- NULL

            curSet$privacy_controls <<- pc
            print(pc)

            GUI$getActiveDoc()$setSettings(
                list(privacy_controls = pc)
            )
            update_summary()
        }
    )
)


## A summary window
iNZGetInference <- setRefClass(
    "iNZGetInference",
    contains = "iNZInfoWindow",
    fields = list(
        inf_method = "ANY",
        hypothesis_test = "ANY",
        hyp_null = "ANY",
        hyp_alt = "ANY",
        hyp_equalvar = "ANY",
        hyp_exactp = "ANY",
        hyp_simulatep = "ANY",
        g_hypctrls = "ANY",
        g_hyptbl = "ANY",
        trend_choice = "list",
        epi_chk = "ANY",
        ci_slider = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            if (is.null(gui$getActiveDoc()$getSettings()$x)) return()
            callSuper(gui, controls = "top", name = "Inference")

            # update_inference()

            ## Control panel
            setup_panel()

            visible(win) <<- TRUE
        },
        gen_call = function() {
            "Generate the function call based on user's chosen vars/settings"

            # This will, at some stage, fetch values from the CODE CALL
            # when it is modified by the user ... and update curSet ... =]
            vartypes <- list(
                x = iNZightTools::vartype(GUI$getActiveData()[[curSet$x]]),
                y = NULL
            )
            if (!is.null(curSet$y))
                vartypes$y <- iNZightTools::vartype(GUI$getActiveData()[[curSet$y]])
            construct_call(curSet, curMod, vartypes,
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
            GUI$getActiveDoc()$setSettings(
                list(
                    bs.inference = curSet$bs.inference,
                    trend = curSet$trend,
                    ci.width = curSet$ci.width
                )
            )
            smry_call <- gen_call()
            set_input(mend_call(smry_call, GUI))

            smry <- try(eval(smry_call, env), silent = TRUE)
            if (inherits(smry, "try-error"))
                smry <- "Unable to generate inference."
            set_output(smry)

            # disable simulate p-value checkbox if expected counts small
            if (!is.null(hyp_simulatep)) {
                exp_match <- any(grepl("since some expected counts <", smry, fixed = TRUE))
                if (enabled(hyp_simulatep) && exp_match) {
                    blockHandlers(hyp_simulatep)
                    hyp_simulatep$set_value(TRUE)
                    enabled(hyp_simulatep) <<- FALSE
                    unblockHandlers(hyp_simulatep)
                }
                if (!enabled(hyp_simulatep) && !exp_match) {
                    blockHandlers(hyp_simulatep)
                    hyp_simulatep$set_value(FALSE)
                    enabled(hyp_simulatep) <<- TRUE
                    unblockHandlers(hyp_simulatep)
                }
            }
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

                if (getOption("inzight.disable.bootstraps", FALSE)) {
                    inf_method <<- glabel("Normal theory", container = g_method, anchor = c(-1, 0))
                } else {
                    inf_method <<- gradio(c("Normal theory", "Bootstrap"),
                        horizontal = FALSE,
                        container = g_method,
                        handler = function(h, ...) {
                            curSet$bs.inference <<- svalue(h$obj, index = TRUE) == 2L
                            update_inference()
                        }
                    )
                }
            }

            # hypothesis testing (all except regression, for now)
            do_hyp_test <- INFTYPE %notin% c("regression")
            if (is_survey && do_hyp_test && INFTYPE == "oneway-table") {
                # survey lets us do prop.test, but not chi-square (one-way)
                do_hyp_test <- length(levels(xvar)) == 2
            }

            hyp_null <<- NULL
            hyp_alt <<- NULL
            hyp_equalvar <<- NULL
            hyp_exactp <<- NULL
            hyp_simulatep <<- NULL
            g_hypctrls <<- NULL
            g_hyptbl <<- NULL

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

                addSpace(ctrl_panel, 20)
                g_hypargs <- ggroup(container = g_hypothesis)

                hypothesis_test <<- gradio(test_options,
                    horizontal = FALSE,
                    container = gvbox(container = g_hypargs),
                    handler = function(h, ...) handle_test()
                )

                ### hypothesis test arguments:
                addSpace(g_hypargs, 20)
                g_hypctrls <<- gvbox(container = g_hypargs)
                visible(g_hypctrls) <<- FALSE

                # null value/alternative [t.test, t.test2, proportion]
                if (any(c("t.test", "t.test2", "proportion") %in% hyp_tests)) {
                    g_hyptbl <<- glayout(container = g_hypctrls)

                    lbl <- glabel("Null value :")
                    g_hyptbl[1, 1, anchor = c(1, 0), expand = TRUE] <<- lbl
                    hyp_null <<- gedit(
                        ifelse("proportion" %in% hyp_tests, 0.5, 0),
                        handler = function(h, ...) {
                            x <- as.numeric(svalue(hyp_null))
                            curSet$hypothesis.value <<- ifelse(is.na(x), 0, x)
                            update_inference()
                        }
                    )
                    # we want user typing to trigger update, not
                    # requiring them to press Enter...
                    null_timer <- NULL
                    addHandlerKeystroke(hyp_null,
                        function(h, ...) {
                            if (!is.null(null_timer) && null_timer$started)
                                null_timer$stop_timer()
                            null_timer <- gtimer(1000,
                                function(...) {
                                    hyp_null$invoke_change_handler()
                                },
                                one.shot = TRUE
                            )
                        }
                    )
                    g_hyptbl[1, 2, expand = TRUE] <<- hyp_null

                    if (!is_survey) {
                        lbl <- glabel("Alternative hypothesis :")
                        g_hyptbl[2, 1, anchor = c(1, 0), expand = TRUE] <<- lbl
                        hyp_alt <<- gcombobox(c("two-sided", "greater than", "less than"),
                            handler = function(h, ...) {
                                curSet$hypothesis.alt <<- switch(
                                    svalue(h$obj, index = TRUE),
                                    "two.sided", "greater", "less"
                                )
                                update_inference()
                            }
                        )
                        g_hyptbl[2, 2, expand = TRUE] <<- hyp_alt
                    }

                    # equal var [t.test2]
                    if ("t.test2" %in% hyp_tests) {
                        hyp_equalvar <<- gcheckbox("Use equal variance",
                            checked = FALSE,
                            container = g_hypctrls,
                            anchor = c(1, 0),
                            expand = TRUE,
                            handler = function(h, ...) {
                                curSet$hypothesis.var.equal <<- svalue(h$obj)
                                update_inference()
                            }
                        )
                    }

                    # exact p-value [proportion]
                    if ("proportion" %in% hyp_tests) {
                        hyp_exactp <<- gcheckbox("Calculate exact p-value",
                            checked = FALSE,
                            container = g_hypctrls,
                            anchor = c(1, 0),
                            expand = TRUE,
                            handler = function(h, ...) {
                                curSet$hypothesis.use.exact <<- svalue(h$obj)
                                update_inference()
                            }
                        )
                    }

                    size(ctrl_panel) <<- c(-1, 140)
                }

                if ("chi2" %in% hyp_tests && !is_survey) {
                    hyp_simulatep <<- gcheckbox("Simulate p-value",
                        checked = FALSE,
                        container = g_hypctrls,
                        anchor = c(1, 0),
                        expand = TRUE,
                        handler = function(h, ...) {
                            curSet$hypothesis.simulated.p.value <<- svalue(h$obj)
                            update_inference()
                        }
                    )
                }

                handle_test()
            }

            if (INFTYPE == "regression") {
                addSpace(ctrl_panel, 20)
                g_trendopt <- gvbox(container = ctrl_panel)
                lbl <- glabel("Trend options",
                    container = g_trendopt,
                    anchor = c(-1, 0)
                )
                font(lbl) <- list(weight = "bold")

                trend_choice <<- list(
                    linear = gcheckbox("Linear",
                        container = g_trendopt,
                        checked = "linear" %in% curSet$trend,
                        handler = function(h, ...) {
                            handle_trend()
                        }
                    ),
                    quadratic = gcheckbox("Quadratic",
                        container = g_trendopt,
                        checked = "quadratic" %in% curSet$trend,
                        handler = function(h, ...) {
                            handle_trend()
                        }
                    ),
                    cubic = gcheckbox("Cubic",
                        container = g_trendopt,
                        checked = "cubic" %in% curSet$trend,
                        handler = function(h, ...) {
                            handle_trend()
                        }
                    )
                )

                handle_trend()
            }

            if (INFTYPE == "twoway-table" && length(levels(xvar)) == 2) {
                # epi out: cat x cat, x ~ y, x is binary
                addSpace(ctrl_panel, 20)
                epi_chk <<- gcheckbox("Show EPI OUTPUT",
                    checked = curSet$epi.out,
                    container = ctrl_panel,
                    handler = function(h, ...) {
                        curSet$epi.out <<- svalue(epi_chk)
                        update_inference()
                    }
                )
            }

            adv_opts <- list(
                ci_level = TRUE
            )

            if (any(unlist(adv_opts))) {
                ## CI width and other controls:
                addSpring(ctrl_panel)
                add(ctrl_panel, gseparator())

                g_advanced <- gvbox(container = ctrl_panel)
                lbl <- glabel("Additional options",
                    container = g_advanced,
                    anchor = c(-1, 0),
                    fill = TRUE
                )
                font(lbl) <- list(weight = "bold")

                adv_tbl <- glayout(container = g_advanced)
                ii <- 1L

                if (adv_opts$ci_level) {
                    ci_slider <<- gspinbutton(
                        10, 99, 1,
                        value = curSet$ci.width * 100,
                        handler = function(h, ...) {
                            curSet$ci.width <<- svalue(ci_slider) / 100
                            update_inference()
                        }
                    )
                    size(ci_slider) <<- c(100, -1)
                    adv_tbl[ii, 1L, anchor = c(1, 0), fill = TRUE] <- "Confidence level (%):"
                    adv_tbl[ii, 2:3, expand = TRUE] <- ci_slider
                    ii <- ii + 1L

                    addSpring(g_advanced)
                    lbl <- glabel(
                        "You may have to press Enter if you type values in manually.",
                        container = g_advanced,
                        anchor = c(-1, 0),
                        fill = TRUE
                    )
                    font(lbl) <- list(size = 8)
                }
            }

            update_inference()
        },
        handle_test = function() {
            # Triggered when the hypothesis test radio is changed
            curSet$hypothesis.value <<- NULL
            curSet$hypothesis.alt <<- NULL
            curSet$var.equal <<- NULL
            curSet$use.exact <<- NULL
            curSet$hypothesis.test <<- NULL
            curSet$hypothesis.simulated.p.value <<- NULL
            curSet$hypothesis <<- if (svalue(hypothesis_test) == "None") "NULL" else NULL

            is_survey <- !is.null(curMod$dataDesign)

            if (!is.null(g_hypctrls)) visible(g_hypctrls) <<- FALSE
            if (!is.null(hyp_exactp)) visible(hyp_exactp) <<- FALSE
            if (!is.null(hyp_simulatep)) visible(hyp_simulatep) <<- FALSE
            if (!is.null(g_hyptbl)) visible(g_hyptbl) <<- FALSE

            switch(svalue(hypothesis_test),
                "One sample t-test" = ,
                "Two sample t-test" = ,
                "Test proportion" = {
                    visible(g_hypctrls) <<- TRUE
                    visible(g_hyptbl) <<- TRUE
                    curSet$hypothesis.value <<- as.numeric(svalue(hyp_null))
                    if (!is_survey) {
                        curSet$hypothesis.alt <<- switch(
                            svalue(hyp_alt, index = TRUE),
                            "two.sided", "greater", "less"
                        )
                    }
                    if (svalue(hypothesis_test) == "Test proportion") {
                        visible(hyp_exactp) <<- TRUE
                        curSet$hypothesis.test <<- "proportion"
                        curSet$hypothesis.use.exact <<- svalue(hyp_exactp)
                    }
                    if (svalue(hypothesis_test) == "Two sample t-test") {
                        # equal variance
                        curSet$hypothesis.var.equal <<- svalue(hyp_equalvar)
                    }
                },
                "ANOVA" = {
                    curSet$hypothesis.test <<- "anova"
                },
                "Chi-square test" = {
                    visible(g_hypctrls) <<- TRUE
                    visible(hyp_simulatep) <<- TRUE
                    curSet$hypothesis.simulated.p.value <<- svalue(hyp_simulatep)
                }
            )

            update_inference()
        },
        handle_trend = function() {
            chosen <- sapply(trend_choice, function(x) svalue(x))
            curSet$trend <<- if (any(chosen)) names(trend_choice)[chosen] else NULL
            update_inference()
        }
    )
)
