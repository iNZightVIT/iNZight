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
                button_width = 80, button_height = 25,
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

            ctrl_pnl <- ggroup(container = panel, expand = TRUE, fill = TRUE)

            lbl <- glabel("R code for the current plot is shown above, which can be edited and run.")
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
            store_btn$set_icon("rlogo")
            run_btn$set_icon("go")
            reset_btn$set_icon("reset")
            size(store_btn) <<- c(button_width, button_height)
            size(run_btn) <<- c(button_width, button_height)
            size(reset_btn) <<- c(button_width, button_height)
            font(store_btn) <<- list(size = 9)
            font(run_btn) <<- list(size = 9)
            font(reset_btn) <<- list(size = 9)

            enabled(store_btn) <<- enabled(run_btn) <<- enabled(reset_btn) <<-
                svalue(input) != ""

            size(panel) <<- c(-1, 90)
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
                    cat("There was an error ... \n")
                    print(e)
                },
                finally = {
                    if (inherits(rawpl, "ggplot")) print(rawpl)

                    curpl <- unclass(rawpl)
                    if (!is.null(attr(curpl, "dotplot.redraw")))
                        if (attr(curpl, "dotplot.redraw"))
                            rawpl <- eval(
                                parse(text = svalue(input)),
                                envir = GUI$code_env
                            )

                    ### This can be activated once the UI can be reconfigured based on
                    ### what the user types ... if ever!!
                    # if (inherits(p, "inzplotoutput") || inherits(p, "ggplot"))
                    #     GUI$curPlot <<- unclass(rawpl)

                    # if (!is.null(attr(GUI$curPlot, "dotplot.redraw")))
                    #     if (attr(curPlot, "dotplot.redraw"))
                    #         GUI$curPlot <<- unclass(
                    #             rawpl <- eval(
                    #                 parse(text = svalue(input)),
                    #                 envir = GUI$code_env
                    #             )
                    #         )
                    # attr(GUI$curPlot, "code") <<- svalue(input)
                    # enabled(GUI$plotToolbar$exportplotBtn) <<- can.interact(rawpl)
                    # GUI$plotType <<- attr(GUI$curPlot, "plottype")
                }
            )
        },
        reset_code = function() {
            set_input(original_code)
            run_code()
        }
    )
)
