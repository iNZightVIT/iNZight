iNZCodePanel <- setRefClass(
    "iNZCodePanel",
    fields = list(
        GUI = "ANY",
        panel = "ANY",
        button_width = "numeric", button_height = "numeric",
        input = "ANY",
        code_font = "list",
        store_btn = "ANY",
        run_btn = "ANY",
        reset_btn = "ANY",
        original_code = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            initFields(
                GUI = gui,
                button_width = 80, button_height = -1,
                code_font = list(family = "monospace", size = 10),
                original_code = ""
            )
            panel <<- gvbox()
            panel$set_borderwidth(5)

            # Input text box
            input <<- gtext("",
                container = panel,
                expand = TRUE,
                font.attr = code_font
            )
            # the default indent is 10, which doesn't look too nice.
            # no direct access to these methods, so must access directly from RGtk2:
            RGtk2::gtkTextViewSetLeftMargin(input$widget, 0)
            RGtk2::gtkTextViewSetRightMargin(input$widget, 0)

            ctrl_pnl <- ggroup(container = panel,
                expand = TRUE,
                fill = TRUE,
                horizontal = !GUI$popOut
            )
            # size(ctrl_pnl) <- c(-1, 25)

            lbltxt <- paste(
                "R code for the current plot is shown above",
                "which can be edited and run.",
                sep = ifelse(GUI$popOut, "\n", ", ")
            )
            lbl <- glabel(lbltxt)
            font(lbl) <- list(size = 9, weight = "bold")
            add(ctrl_pnl, lbl, anchor = c(-1, 0))

            # add buttons for STORE, RUN, and RESET
            addSpring(ctrl_pnl)
            btn_pnl <- ggroup(container = ctrl_pnl)
            store_btn <<- gbutton("Store",
                container = btn_pnl,
                handler = function(h, ...) store_code()
            )
            run_btn <<- gbutton("Run",
                container = btn_pnl,
                handler = function(h, ...) run_code()
            )
            reset_btn <<- gbutton("Reset",
                container = btn_pnl,
                handler = function(h, ...) reset_code()
            )
            store_btn$set_icon("")
            run_btn$set_icon("")
            reset_btn$set_icon("")

            size(store_btn) <<- c(button_width, button_height)
            size(run_btn) <<- c(button_width, button_height)
            size(reset_btn) <<- c(button_width, button_height)
            font(store_btn) <<- list(size = 9)
            font(run_btn) <<- list(size = 9)
            font(reset_btn) <<- list(size = 9)

            addHandlerChanged(input, .self$input_handler)
            addHandlerKeystroke(input, .self$input_handler)
            enabled(store_btn) <<- enabled(run_btn) <<- enabled(reset_btn) <<-
                svalue(input) != ""

            size(input) <<- c(-1, 60)
        },
        set_input = function(code) {
            original_code <<- code
            svalue(input) <<- ""
            insert(input, code, where = "beginning", font.attr = code_font)
            enabled(store_btn) <<- enabled(run_btn) <<- enabled(reset_btn) <<-
                svalue(input) != ""
        },
        store_code = function() {
            GUI$rhistory$add(attr(GUI$curPlot, "code"))
        },
        run_code = function() {
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
                    rawpl <- eval(
                        parse(text = svalue(input)),
                        envir = GUI$code_env
                    )
                },
                error = function(e) {
                    msg <- sprintf("There was an error in your plot code:\n\n%s", e$message)
                    if (interactive()) {
                        gmessage(
                            msg,
                            title = "Error",
                            icon = "error",
                            parent = GUI$win
                        )
                    } else {
                        stop(msg)
                    }
                }
            )

            if (!exists("rawpl")) return()

            if (inherits(rawpl, "ggplot")) {
                print(rawpl)
            }

            if (!grepl("^inzplot", svalue(input))) {
                GUI$plotType <<- "custom"
                return()
            }

            curpl <- unclass(rawpl)
            if (!is.null(attr(curpl, "dotplot.redraw")))
                if (attr(curpl, "dotplot.redraw"))
                    rawpl <- eval(
                        parse(text = svalue(input)),
                        envir = GUI$code_env
                    )

            # update settings .....
            pcall <- as.list(as.call(parse(text = svalue(input)))[[1]])[-1]

            # first term is x/y
            call_xy <- as.list(pcall[[1]])
            pcall <- pcall[-1]
            vars <- list(
                x = NULL,
                y = NULL,
                g1 = NULL, g1.level = NULL,
                g2 = NULL, g2.level = NULL
            )

            if (length(call_xy) == 2L) { # list("~", "var")
                # just a single variable
                vars$x <- as.character(call_xy[[2L]])
            } else {
                # a more complex formula
                vars$x <- as.character(call_xy[[2]])

                call_yg <- as.list(call_xy[[3]])
                if (length(call_yg) == 1) {
                    # no subsetting
                    vars$y <- as.character(call_yg[[1]])
                } else {
                    if (as.character(call_yg[[2]]) != ".") {
                        vars$y <- as.character(call_yg[[2]])
                    }
                    call_g <- call_yg[[3]]
                    if (length(call_g) == 1) {
                        # just one subset
                        vars$g1 <- as.character(call_g)
                    } else {
                        # two subsets
                        vars$g1 <- as.character(call_g[[2]])
                        vars$g2 <- as.character(call_g[[3]])
                    }
                }
            }

            if (length(vars$x) > 1) {
                vars$x <- vars$x[-1]

                if (!GUI$preferences$multiple_x) {
                    warning("Enable multiple response variables (from Preferences) to continue.")
                    vars$x <- NULL
                }
            }

            # g1.level and g2.level -> depend on g1 and g2
            if (!is.null(pcall$g1.level) && !is.null(vars$g1)) {
                vars$g1.level <- as.character(pcall$g1.level)
            }
            if (!is.null(pcall$g2.level) && !is.null(vars$g2) && !is.null(vars$g1)) {
                vars$g2.level <- as.character(pcall$g2.level)
            }

            # update control widget
            if (!is.null(vars$x)) {
                blockHandlers(GUI$ctrlWidget$V1box)
                GUI$ctrlWidget$V1box$set_value(vars$x)
                unblockHandlers(GUI$ctrlWidget$V1box)
                vars$x <- as.name(vars$x)

                blockHandlers(GUI$ctrlWidget$V2box)
                if (!is.null(vars$y)) {
                    GUI$ctrlWidget$V2box$set_value(vars$y)
                    vars$y <- as.name(vars$y)
                } else {
                    GUI$ctrlWidget$V2box$set_index(1L)
                }
                unblockHandlers(GUI$ctrlWidget$V2box)

                blockHandlers(GUI$ctrlWidget$G1box)
                if (!is.null(vars$g1)) {
                    GUI$ctrlWidget$G1box$set_value(vars$g1)

                    vindex <- 1L
                    if (!is.null(vars$g1.level) && vars$g1.level != "_MULTI") {
                        vindex <- vars$g1.level
                    }
                    GUI$ctrlWidget$createSlider(pos = 6, vars$g1, vindex)
                    vars$g1 <- as.name(vars$g1)
                } else {
                    GUI$ctrlWidget$G1box$set_index(1L)
                    GUI$ctrlWidget$deleteSlider(pos = 6L)
                }
                unblockHandlers(GUI$ctrlWidget$G1box)

                blockHandlers(GUI$ctrlWidget$G2box)
                if (!is.null(vars$g2)) {
                    GUI$ctrlWidget$G2box$set_value(vars$g2)

                    vindex <- 1L
                    if (!is.null(vars$g2.level) && vars$g2.level != "_ALL") {
                        if (vars$g2.level != "_MULTI") {
                            vindex <- vars$g2.level
                        } else {
                            lvls <- levels(GUI$getActiveData()[[vars$g2]])
                            vindex <- length(lvls) + 1L
                        }
                    }
                    GUI$ctrlWidget$createSlider(pos = 8, vars$g2, vindex)
                    vars$g2 <- as.name(vars$g2)
                } else {
                    GUI$ctrlWidget$G2box$set_index(1L)
                    GUI$ctrlWidget$deleteSlider(pos = 8L)
                }
                unblockHandlers(GUI$ctrlWidget$G2box)
            }

            # remove data/design
            call_set <- pcall[names(pcall) %notin% c("data", "design", "g1.level", "g2.level")]
            call_set <- c(vars, call_set)
            if (length(call_set)) {
                # determine settings relevant to this plot, but not set
                # and remove them from settings (set to NULL)
                if (!is.null(attr(rawpl, "plottype")) &&
                    attr(rawpl, "plottype") %in% colnames(iNZightPlots:::plot_types)) {
                    pargs <- iNZightPlots:::plot_types[, attr(rawpl, "plottype")]
                    pargs <- names(pargs[grepl("p", pargs)])
                    dflts <- unclass(iNZightPlots:::inzpar())
                    def_args <- dflts[names(dflts) %in% pargs]
                    call_set <- modifyList(def_args, call_set, keep.null = TRUE)
                }

                GUI$getActiveDoc()$plotSettings$.changed$block()
                GUI$getActiveDoc()$plotSettings$.settingsChanged$block()
                GUI$getActiveDoc()$setSettings(call_set)
                GUI$getActiveDoc()$plotSettings$.changed$unblock()
                GUI$getActiveDoc()$plotSettings$.settingsChanged$unblock()
            }

            ### This can be activated once the UI can be reconfigured based on
            ### what the user types ... if ever!!
            GUI$curPlot <<- unclass(rawpl)

            if (!is.null(attr(GUI$curPlot, "dotplot.redraw")))
                if (attr(GUI$curPlot, "dotplot.redraw"))
                    GUI$curPlot <<- unclass(
                        rawpl <- eval(
                            parse(text = svalue(input)),
                            envir = GUI$code_env
                        )
                    )
            if ( !is.null( attr(GUI$curPlot, "code") ) ) {
                attr(GUI$curPlot, "gg_code") <<- attr(GUI$curPlot, "code")
            }
            attr(GUI$curPlot, "code") <<- svalue(input)
            enabled(GUI$plotToolbar$exportplotBtn) <<- can.interact(rawpl)
            GUI$plotType <<- attr(GUI$curPlot, "plottype")

        },
        reset_code = function() {
            set_input(original_code)
            GUI$updatePlot()
        },
        input_handler = function(h, ...) {
            enabled(store_btn) <<- enabled(run_btn) <<- enabled(reset_btn) <<-
                svalue(input) != ""
        },
        show = function() visible(panel) <<- TRUE,
        hide = function() visible(panel) <<- FALSE
    )
)
