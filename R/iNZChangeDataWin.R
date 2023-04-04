## --------------------------------------------
## Class that handles the filtering of a dataset
## Upon initialization a window with different filter
## options is displayed. Upon choosing one, this
## window is closed and another window with specifics
## for that filter options is opened
## --------------------------------------------

iNZFilterWin <- setRefClass(
    "iNZFilterWin",
    fields = list(
        GUI = "ANY",
        filter_type = "ANY",
        g_value = "ANY", g_row = "ANY", g_random = "ANY",
        filter_var = "ANY",
        cat_levels = "ANY", num_cond = "ANY", num_value = "ANY",
        row_nums = "ANY",
        rand_size = "ANY", rand_num = "ANY", rand_msg = "ANY",
        keytimer = "ANY",
        vartype = "character",
        new_row = "ANY",
        cnclBtn = "ANY", okBtn = "ANY",
        code_panel = "ANY", code_font = "list",
        newdata = "ANY"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui = NULL) {

            if (is.null(gui)) return()
            is_survey <- !is.null(gui$getActiveDoc()$getModel()$getDesign())
            win_title <- sprintf(
                "Filter %s",
                ifelse(is_survey, "survey design", "data")
            )

            ok <- callSuper(gui,
                title = win_title,
                width = 500L,
                height = 500L,
                ok = "Filter",
                action = .self$update_data,
                help = "user_guides/data_options/#filter",
                show_code = TRUE
            )
            if (!ok) return()
            on.exit(.self$show())

            initFields(
                newdata = NULL,
                code_font = list(size = 8, family = "monospace")
            )
            usingMethods("handle_filter", "update_data")

            ## top group
            gtop <- ggroup()
            opts <- c(
                "by value",
                "by row number",
                "randomly"
            )
            if (is_survey) opts <- opts[-3L]
            filter_type <<- gradio(
                opts,
                selected = 1L,
                horizontal = TRUE,
                container = gtop,
                handler = function(h, ...) {
                    visible(g_value) <<- h$obj$get_index() == 1L
                    visible(g_row) <<- h$obj$get_index() == 2L
                    visible(g_random) <<- h$obj$get_index() == 3L
                    # and clear things:
                    clear_result()
                }
            )

            add_body(gtop)
            body_space(10L)

            ### container for content
            gmain <- ggroup(expand = TRUE, fill = TRUE)

            #### --- filter by value
            g_value <<- ggroup(container = gmain, expand = TRUE)
            addSpring(g_value)
            tbl_value <- glayout(container = g_value)

            lbl <- glabel("Variable :")
            filter_var <<- gcombobox(names(GUI$getActiveData(lazy = TRUE)), selected = 0L)
            size(filter_var) <<- c(250, -1)
            tbl_value[1, 1, anchor = c(1, 0)] <- lbl
            tbl_value[1, 2:3] <- filter_var

            addHandlerChanged(filter_var,
                function(h, ...) {
                    varname <- svalue(h$obj)
                    var <- GUI$getActiveData(lazy = TRUE)[[varname]]
                    vartype <<- iNZightTools::vartype(var)

                    # remove all children
                    sapply(rev(tbl_value$child_positions),
                        function(x) if (x$x > 1) tbl_value$remove_child(x$child))

                    switch(vartype,
                        "cat" = {
                            # if categorical, choose levels to keep
                            lbl_levels <- glabel("Levels to keep :")
                            cat_levels <<- gtable(levels(var), multiple = TRUE)
                            addHandlerSelectionChanged(cat_levels, handle_filter)
                            size(cat_levels) <<- c(-1, 200)
                            tbl_value[2, 1, anchor = c(1, 1)] <- lbl_levels
                            tbl_value[2, 2:3] <- cat_levels

                        },
                        "num" = {
                            # if numeric, choose condition and value
                            lbl_condiiton <- glabel("Condition :")
                            num_cond <<- gcombobox(
                                c("<", "<=", "==", ">=", ">", "!="),
                                selected = 0L,
                                handler = handle_filter
                            )
                            vr <- range(var, na.rm = TRUE)
                            dr <- diff(vr) / 100
                            dr <- signif(dr, 1)
                            if (dr >= 1) {
                                nr <- 10^nchar(dr)
                            } else {
                                nr <- 10^-(nchar(dr) - 2L)
                            }
                            if (nr < 1 && all(as.integer(var) == var, na.rm = TRUE)) nr <- 1

                            num_value <<- gspinbutton(vr[1], vr[2], nr, handler = handle_filter)
                            tbl_value[2, 1, anchor = c(1, 0)] <- lbl_condiiton
                            tbl_value[2, 2] <- num_cond
                            tbl_value[2, 3] <- num_value

                        }
                    )


                }
            )

            #### --- filter by row number
            g_row <<- ggroup(container = gmain, expand = TRUE)
            addSpring(g_row)
            tbl_row <- glayout(container = g_row)

            lbl <- glabel("Rows to remove :")
            row_nums <<- gedit()

            keytimer <<- NULL
            addHandlerKeystroke(row_nums,
                function(h, ...) {
                    if (!is.null(keytimer))
                        if (keytimer$started) keytimer$stop_timer()
                    keytimer <<- gtimer(500, handle_filter, one.shot = TRUE)
                }
            )
            size(row_nums) <<- c(250, -1)
            tbl_row[1, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_row[1, 2:3] <- row_nums

            help_text <- paste(sep = "\n",
                "In the box above, enter the numbers of rows to remove.",
                "Separate multiple rows by commas (,), and specify ranges",
                "using a colon (:). For example, "
            )
            lbl <- glabel(help_text)
            font(lbl) <- list(size = 9)
            tbl_row[2, 2:3, anchor = c(-1, 0), expand = TRUE] <- lbl
            lbls <- list(
                "Delete the first 5 rows: " = "1:5",
                "Delete several rows: " = "5, 8, 20",
                "Or a combination: " = "5, 8, 20:30, 87"
            )
            bleft <- gvbox(spacing = 0)
            bright <- gvbox(spacing = 0)
            ii <- 3L
            for (i in seq_along(lbls)) {
                lbl <- glabel(names(lbls)[[i]])
                font(lbl) <- list(size = 9)
                add(bleft, lbl, anchor = c(1, 0), expand = TRUE)
                lbl <- glabel(lbls[[i]])
                font(lbl) <- list(size = 9)
                add(bright, lbl, anchor = c(-1, 0), expand = TRUE)
            }
            tbl_row[3L, 2L] <- bleft
            tbl_row[3L, 3L] <- bright



            #### --- filter randomly
            g_random <<- gvbox(container = gmain, expand = TRUE)
            g_random2 <- ggroup(container = g_random, expand = TRUE)
            addSpring(g_random2)
            tbl_row <- glayout(container = g_random2)
            addSpring(g_random2)

            lbl <- glabel("Sample size :")
            rand_size <<- gedit()
            addHandlerKeystroke(rand_size,
                function(h, ...) {
                    if (!is.null(keytimer))
                        if (keytimer$started) keytimer$stop_timer()
                    keytimer <<- gtimer(500, handle_filter, one.shot = TRUE)
                }
            )
            size(rand_size) <<- c(150, -1)
            tbl_row[1L, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_row[1L, 2L] <- rand_size

            lbl <- glabel("Number of samples :")
            rand_num <<- gspinbutton(
                from = 1,
                to = nrow(GUI$getActiveData(lazy = TRUE)), by = 1L,
                handler = handle_filter)
            size(rand_num) <<- c(150, -1)
            tbl_row[2L, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_row[2L, 2L] <- rand_num

            rand_msg <<- glabel("", container = g_random)
            font(rand_msg) <<- list(size = 9, weight = "bold", color = "orangered")

            add_body(gmain)
            body_spring()

            ### dataset info:
            ginfo <- gvbox()
            cur_row <- glabel(
                sprintf("Current data has %d rows",
                    nrow(GUI$getActiveData(lazy = TRUE))
                ),
                container = body,
                anchor = c(1, 0))
            new_row <<- glabel("", container = body,
            anchor = c(1, 0))
            font(cur_row) <- list(size = 9)
            font(new_row) <<- list(size = 9)

            add_body(ginfo)

            filter_type$invoke_change_handler()


        },
        handle_filter = function(h, ...) {
            newdata <<- NULL

            switch(filter_type$get_index(),
                filter_value(),
                filter_row(),
                filter_random()
            )
            if (is.null(newdata)) {
                clear_result()
                return()
            }

            .dataset <- GUI$get_data_object(lazy = FALSE)
            attr(newdata, "code") <<- gsub(".dataset",
                if (iNZightTools::is_survey(.dataset)) GUI$getActiveDoc()$getModel()$dataDesignName
                else GUI$dataNameWidget$datName,
                attr(newdata, "code")
            )
            # clear panel first...
            set_code(paste(iNZightTools::code(newdata), collapse = "\n"))

            # set row info
            data <- newdata
            if (iNZightTools::is_survey(data)) data <- data$variables
            str <- sprintf("New data has %d rows (%d deleted)",
                nrow(data),
                nrow(GUI$getActiveData(lazy = TRUE)) - nrow(data)
            )
            svalue(new_row) <<- str
            enabled(okBtn) <<- TRUE
        },
        clear_result = function() {
            newdata <<- NULL
            set_code("# R code will show here")
            svalue(new_row) <<- ""
            enabled(okBtn) <<- FALSE
        },
        filter_value = function() {
            if (vartype == "cat" && length(cat_levels$get_index()) == 0) return()
            if (vartype == "num" && svalue(num_cond) == "") return()

            .dataset <- GUI$get_data_object(lazy = FALSE)
            switch(vartype,
                "cat" = {
                    newdata <<- iNZightTools::filter_cat(.dataset,
                        var = svalue(filter_var),
                        levels = svalue(cat_levels)
                    )
                },
                "num" = {
                    newdata <<- iNZightTools::filter_num(.dataset,
                        var = svalue(filter_var),
                        op = svalue(num_cond),
                        num = svalue(num_value)
                    )
                }
            )
        },
        filter_row = function() {
            if (svalue(row_nums) == "") return()
            .dataset <- GUI$get_data_object(lazy = FALSE)
            delrows <- sprintf("c(%s)", svalue(row_nums)) |>
                rlang::parse_expr() |>
                rlang::eval_tidy()
            newdata <<- iNZightTools::remove_rows(.dataset, rows = delrows)
        },
        filter_random = function() {
            if (svalue(rand_size) == "") return()
            samplesize <- as.integer(svalue(rand_size))
            if (is.na(samplesize)) return()
            nsample <- svalue(rand_num)
            if (samplesize * nsample > nrow(GUI$getActiveData(lazy = TRUE))) {
                svalue(rand_msg) <<- paste(
                    sep = "\n",
                    "Cannot sample more rows than in the original dataset.",
                    "Try fewer or smaller samples."
                )
                return()
            } else {
                svalue(rand_msg) <<- ""
            }
            .dataset <- GUI$get_data_object(lazy = FALSE)
            newdata <<- iNZightTools::random_sample(.dataset, n = nsample, sample_size = samplesize)
        },
        update_data = function(h, ...) {
            if (is.null(newdata)) {
                gmessage("No valid filtering applied.")
                return()
            }

            GUI$new_document(newdata, "filtered")
            dispose(GUI$modWin)
        }
    )
)


## --------------------------------------------
## Class that handles the sortby of a dataset
## --------------------------------------------
iNZSortWin <- setRefClass(
    "iNZSortWin",
    fields = list(
        var_names = "character",
        var_tbl = "ANY",
        sort_vars = "logical"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui = NULL) {

            ok <- callSuper(gui,
                title = "Sort data by variables",
                width = "small",
                height = "med",
                ok = "Sort",
                action = .self$sort_data,
                help = "user_guides/data_options/#sort",
                show_code = FALSE,
                scroll = TRUE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("handle_sort", "sort_data")

            add_heading(
                "Data will be sorted by each of the chosen variables",
                "in the order specified. By default, small values",
                "are first; check the 'decreasing' box to sort large",
                "values first instead.",
                size = 9L
            )
            add_heading(
                "New rows will appear as you choose variables. Click 'Sort' once you have enough variables.",
                size = 9L
            )
            body_space(10L)

            var_names <<- names(GUI$getActiveData(lazy = TRUE))
            var_tbl <<- glayout()
            add_body(var_tbl)

            add_var()
        },
        add_var = function() {
            ii <- length(sort_vars) + 1L
            var_tbl[ii, 1L, expand = TRUE] <<-
                gcombobox(
                    c("", var_names[!var_names %in% names(sort_vars)]),
                    selected = 1L,
                    handler = handle_sort
                )
            var_tbl[ii, 2L, expand = TRUE, fill = TRUE] <<-
                gcheckbox("Decreasing", handler = handle_sort)
        },
        handle_sort = function(h, ...) {
            n <- nrow(var_tbl)
            v <- sapply(seq_len(n),
                function(i) {
                    svalue(var_tbl[i, 1L])
                }
            )
            d <- sapply(seq_len(n),
                function(i) {
                    svalue(var_tbl[i, 2L])
                }
            )
            if (v[n] == "") {
                v <- v[-n]
                d <- d[-n]
            }
            sort_vars <<- structure(!d, .Names = v)

            # change options
            for (i in seq_along(sort_vars)) {
                opts <- var_names[!var_names %in% names(sort_vars)[-i]]
                blockHandlers(var_tbl[i, 1L])
                var_tbl[i, 1L]$set_items(c("", opts))
                var_tbl[i, 1L]$set_value(names(sort_vars[i]))
                unblockHandlers(var_tbl[i, 1L])
            }

            # only add new row if all values are filled
            if (length(sort_vars) == n)
                add_var()
        },
        sort_data = function() {
            .dataset <- GUI$get_data_object(lazy = FALSE)
            i <- names(sort_vars) != ""
            newdata <- iNZightTools::sort_vars(
                .dataset,
                names(sort_vars[i]),
                as.logical(sort_vars[i])
            )
            GUI$new_document(newdata, "sorted")
            dispose(GUI$modWin)
        }
    )
)


## --------------------------------------------
## Class that handles aggregate the data set
## --------------------------------------------

iNZAggregateWin <- setRefClass(
    "iNZAggregateWin",
    fields = list(
        GUI = "ANY",
        design = "ANY", is_survey = "logical",
        catvars = "character", numvars = "character",
        available_aggvars = "ANY", aggvars = "ANY",
        aggbtn_add = "ANY", aggbtn_rmv = "ANY",
        btn_up = "ANY", btn_down = "ANY", reordering = "logical",
        smryvars = "ANY",
        gsmry = "ANY", smry_tbl = "ANY",
        df_preview = "ANY",
        close_btn = "ANY", ok_btn = "ANY",
        adv_chk = "ANY"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui) {

            ok <- callSuper(gui,
                title = "Aggregate data",
                width = "med",
                height = "med",
                ok = "Aggregate",
                action = .self$do_aggregation,
                help = "user_guides/data_options/#aggregate",
                show_code = FALSE,
                scroll = FALSE,
                body_direction = "horizontal"
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("do_aggregation")

            initFields(
                reordering = FALSE
            )
            d <- GUI$getActiveData(lazy = TRUE)
            allvars <- names(d)
            vt <- iNZightTools::vartypes(d)
            catvars <<- allvars[vt == "cat"]
            numvars <<- allvars[vt != "cat"] # includes datetimes

            design <<- GUI$getActiveDoc()$getModel()$getDesign()
            is_survey <<- !is.null(design)


            ### +++++++ Variable selection
            g_var <- gvbox(expand = TRUE)

            ### +++ Aggregation variables
            g_aggvars <- gframe("1. Choose aggregation variables",
                container = g_var)
            font(g_aggvars) <- list(weight = "bold")
            g_aggvars$set_borderwidth(5)

            available_aggvars <<- gtable(items = list(Available = catvars),
                multiple = TRUE,
                container = g_aggvars)
            size(available_aggvars) <<- c(-1, 160)

            g_aggbtns <- gvbox(container = g_aggvars)
            addSpring(g_aggbtns)
            aggbtn_add <<- gimagebutton("forward",
                size = "large_toolbar",
                container = g_aggbtns,
                tooltip = "Add selected",
                handler = function(h, ...) {
                    add_aggvars(svalue(available_aggvars, index = TRUE))
                }
            )
            aggbtn_rmv <<- gimagebutton("backward",
                size = "large_toolbar",
                container = g_aggbtns,
                tooltip = "Remove selected",
                handler = function(h, ...) {
                    rmv_aggvars(svalue(aggvars, index = TRUE))
                }
            )
            addSpring(g_aggbtns)

            aggvars <<- gtable(items = list(Selected = character()),
                handler = function(h, ...) update_preview(),
                multiple = TRUE,
                container = g_aggvars)
            size(aggvars) <<- c(-1, 160)

            g_aggbtns2 <- gvbox(container = g_aggvars)
            btn_up <<- gimagebutton("1uparrow",
                size = "large_toolbar",
                container = g_aggbtns2,
                tooltip = "Move selected variable up",
                handler = function(h, ...) {
                    if (reordering) return()
                    reordering <<- TRUE
                    on.exit(reordering <<- FALSE)

                    index <- svalue(aggvars, index = TRUE)
                    if (length(index) != 1) return()
                    if (index == 1) return()
                    selected <- aggvars$get_items()
                    selected_index <- seq_along(selected)
                    selected_index[index] <- index - 1
                    selected_index[index - 1] <- index
                    selected <- selected[selected_index]
                    aggvars$set_items(data.frame(Selected = selected))
                    svalue(aggvars) <<- index - 1
                }
            )
            btn_down <<- gimagebutton("1downarrow",
                size = "large_toolbar",
                container = g_aggbtns2,
                tooltip = "Move selected variable down",
                handler = function(h, ...) {
                    if (reordering) return()
                    reordering <<- TRUE
                    on.exit(reordering <<- FALSE)

                    index <- svalue(aggvars, index = TRUE)
                    if (length(index) != 1) return()
                    selected <- aggvars$get_items()
                    if (index == length(selected)) return()
                    selected_index <- seq_along(selected)
                    selected_index[index] <- index + 1
                    selected_index[index + 1] <- index
                    selected <- selected[selected_index]
                    aggvars$set_items(data.frame(Selected = selected))
                    svalue(aggvars) <<- index + 1
                }
            )



            ### +++ Summary variables
            g_smryvars <- gframe("2. Choose variables to summarise",
                container = g_var)
            font(g_smryvars) <- list(weight = "bold")
            g_smryvars$set_borderwidth(5)

            smryvars <<- gtable(list(Summarize = numvars),
                multiple = TRUE,
                container = g_smryvars)
            addHandlerSelectionChanged(smryvars,
                handler = function(h, ...) update_preview()
            )
            size(smryvars) <<- c(-1, 160)


            lbl <- glabel("CTRL or SHIFT to choose many, or CTRL+A to select all",
                container = g_var)
            font(lbl) <- list(weight = "bold", size = 8)

            add_body(g_var)

            ### +++++++ Summary selection
            gsmry <<- gframe("3. Summaries to calculate")
            gsmry$set_borderwidth(5)
            smry_tbl <<- NULL

            add_body(gsmry)

            ### +++++++ Preview
            gprev <- gframe("Preview", expand = TRUE)
            gprev$set_borderwidth(5)

            df_preview <<- gtable(list(Variables = character()),
                container = gprev
            )
            size(df_preview) <<- c(140, 250)

            add_body(gprev)


            ########################## Window buttons
            adv_chk <<- gcheckbox("Advanced mode",
                handler = function(h, ...) set_advanced())
            # add_toolbar(adv_chk)


            set_advanced()
        },
        add_aggvars = function(index) {
            available <- as.character(available_aggvars$get_items())
            selected <- as.character(aggvars$get_items())

            if (index[1] == -1) {
                selected <- c(selected, available)
                available <- ""
            } else {
                selected <- c(selected, available[index])
                available <- available[-index]
                if (length(available) == 0) available <- ""
            }
            selected <- selected[selected != ""]

            available_aggvars$set_items(data.frame(Available = available))
            aggvars$set_items(data.frame(Selected = selected))
        },
        rmv_aggvars = function(index) {
            available <- as.character(available_aggvars$get_items())
            selected <- as.character(aggvars$get_items())

            if (index[1] == -1) {
                available <- c(available, selected)
                selected <- ""
            } else {
                available <- c(available, selected[index])
                selected <- selected[-index]
                if (length(selected) == 0) selected <- ""
            }
            available <- available[available != ""]

            available_aggvars$set_items(data.frame(Available = available))
            aggvars$set_items(data.frame(Selected = selected))
        },
        add_summary_row = function(text,
                                   name = paste("{var}", tolower(text), sep = "_"),
                                   default = FALSE) {
            i <- 1L
            if (length(smry_tbl$children)) i <- nrow(smry_tbl) + 1L

            chk <- gcheckbox(text, checked = default, handler = function(h, ...) update_preview())
            smry_tbl[i, 1:2, expand = TRUE] <<- chk

            if (svalue(adv_chk) && !is.null(name)) {
                txt <- gedit(name, width = 15)
                smry_tbl[i, 3, expand = TRUE] <<- txt
            }
        },
        update_preview = function() {
            if (length(aggvars$get_items() == 1) && aggvars$get_items()[1] == "") return()
            if (length(svalue(smryvars)) == 0) return()
            summaries <- get_summaries()
            quantiles <- get_quantiles(summaries)
            vars <- svalue(smryvars)
            varnames <- lapply(vars,
                function(var)  {
                    x <- lapply(summaries[, 2],
                        function(smry) {
                            if (!is.null(quantiles)) {
                                glue::glue(smry, .envir = list(var = var, p = quantiles))
                            } else {
                                glue::glue(smry)
                            }
                        }
                    )
                    do.call(c, x)
                }
            )
            varnames <- unique(do.call(c, varnames))
            df_preview$set_items(data.frame(Variables = varnames))
        },
        set_advanced = function() {
            gen_summary_table()
        },
        gen_summary_table = function() {
            if (!is.null(smry_tbl) && nrow(smry_tbl) > 1L) {
                visible(smry_tbl) <<- FALSE
                for (i in length(smry_tbl$children):1)
                    smry_tbl$remove_child(smry_tbl$children[[i]])
                smry_tbl$parent$remove_child(smry_tbl)
            }

            tlbl <- function(text) {
                lbl <- glabel(text)
                font(lbl) <- list(weight = "bold", size = 9)
                lbl
            }

            smry_tbl <<- glayout(container = gsmry)
            advanced <- svalue(adv_chk)

            smry_tbl[1, 1:2, anchor = c(-1, 0), expand = TRUE] <<- tlbl("     Summary")
            if (advanced)
                smry_tbl[1, 3, anchor = c(-1, 0), expand = TRUE] <<- tlbl("Variable name")

            add_summary_row("Count", name = "count", default = TRUE)
            add_summary_row("Mean")
            add_summary_row("Median")
            add_summary_row("Quantile(s)", name = "{var}_q{p}")
            # add quantile extras
            ii <- nrow(smry_tbl) + 1L
            smry_tbl[ii, 2, anchor = c(1, 0), expand = TRUE] <<- glabel("p = ")
            smry_tbl[ii, 3, expand = TRUE] <<- gedit("25, 75", width = 15L)

            add_summary_row("Sum")
            add_summary_row("Standard deviation", name = "{var}_sd")
            add_summary_row("Interquartile range", name = "{var}_iqr")
            if (advanced)
                add_summary_row("Custom", name = NULL)

            add_summary_row("Missing count", name = "{var}_missing", default = TRUE)
        },
        get_summaries = function() {
            adv <- svalue(adv_chk)
            do.call(
                rbind,
                sapply(seq_along(1L:nrow(smry_tbl)),
                    function(i) {
                        w <- smry_tbl[i, 1L]
                        if (!methods::is(w, "GCheckbox") || !svalue(w)) return(NULL)
                        text <- w$get_items()
                        if (text == "Custom") return(NULL)
                        fun <- switch(text,
                            "Standard deviation" = "sd",
                            "Interquartile range" = "IQR",
                            "Quantile(s)" = "quantile",
                            "Missing count" = "missing",
                            tolower(text)
                        )
                        if (adv) {
                            name <- smry_tbl[i, 3L]$get_value()
                        } else {
                            name <- iNZightTools:::agg_default_name(fun)
                        }
                        extra <- NULL
                        if (fun == "quantile") {
                            # fetch quantiles from below
                            extra <- smry_tbl[i + 1L, 3L]$get_value()
                        }

                        c(fun, name, extra)
                    }
                )
            )
        },
        get_quantiles = function(summaries) {
            quantiles <- NULL
            if ("quantile" %in% summaries[,1]) {
                quantiles <- summaries[summaries[,1] == "quantile", 3]
                quantiles <- as.integer(strsplit(quantiles, ",")[[1]])
                quantiles <- quantiles / 100
            }
            quantiles
        },
        do_aggregation = function() {
            adv <- svalue(adv_chk)
            # figure out what summaries the user wants
            summaries <- get_summaries()
            quantiles <- get_quantiles(summaries)

            custom <- NULL
            if (adv) {
                # custom summary functions?

            }

            .dataset <- GUI$get_data_object(lazy = FALSE)
            newdata <- iNZightTools::aggregate_data(
                .dataset,
                group_vars = aggvars$get_items(),
                vars = svalue(smryvars),
                summaries = summaries[,1],
                names = summaries[,2],
                quantiles = quantiles
            )

            GUI$new_document(data = newdata, suffix = "aggregated")
            dispose(GUI$modWin)
        }
    )
)

iNZStackWin <- setRefClass(
    "iNZStackWin",
    fields = list(
        stack_vars = "ANY"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui = NULL) {

            ok <- callSuper(gui,
                title = "Stack variables",
                width = "small",
                height = "med",
                ok = "Stack",
                action = .self$stack_data,
                help = "user_guides/data_options/#stack",
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("stack_data")

            add_heading(
                "Choose variables to stack (or gather) into a single column."
            )
            add_heading("Hold CTRL to choose many.", size = 8L, weight = "bold")

            ## display only numeric variables
            numIndices <- iNZightTools::vartypes(GUI$getActiveData(lazy = TRUE)) != "cat"
            stack_vars <<- gtable(
                names(GUI$getActiveData(lazy = TRUE))[numIndices],
                multiple = TRUE
            )
            names(stack_vars) <<- "Variables"

            add_body(stack_vars, expand = TRUE)

            show()
        },
        stack_data = function() {
            if (length(svalue(stack_vars)) == 0L) {
                gmessage("No variables selected", type = "warning",
                    parent = GUI$modWin)
                return()
            }

            vars <- svalue(stack_vars)

            .dataset <- GUI$getActiveData(lazy = FALSE)
            data <- iNZightTools::reshape_data(.dataset, cols = vars, names_to = "stack_variable", values_to = "stack_value")
            attr(data, "name") <-
                paste(
                    attr(.dataset, "name", exact = TRUE),
                    "stacked",
                    sep = "."
                )
            attr(data, "code") <-
                gsub(".dataset",
                    attr(.dataset, "name", exact = TRUE),
                    attr(data, "code")
                )
            GUI$setDocument(iNZDocument$new(data = data, preferences = GUI$preferences))

            close()
        }
    )
)

iNZReorderVarsWin <- setRefClass(
    "iNZReorderVarsWin",
    fields = list(
        dataVars = "ANY", chosenVars = "ANY",
        btn_add = "ANY", btn_rmv = "ANY",
        btn_up = "ANY", btn_down = "ANY",
        reordering = "logical"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui) {

            initFields(reordering = FALSE)

            ok <- callSuper(gui,
                title = "Reorder and Select Variables",
                width = "med",
                height = "large",
                ok = "Done",
                action = .self$reorder_select_vars,
                # help = "user_guides/data_options/#reorder-and-select-variables",
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("reorder_select_vars")

            add_heading(
                "Select variables from the left to retain",
                "in the data set (hold CTRL to select many).",
                "Use the horizontal arrows to move variables from",
                "'Remove' to 'Keep'.",
                "Use the vertical arrows on the right to move",
                "the chosen variable in 'Keep' up/down."
            )

            # boxes with variables
            g_vars <- ggroup()
            dataVars <<- gtable(data.frame(Remove = names(GUI$getActiveData(lazy = TRUE))),
                multiple = TRUE,
                container = g_vars,
                expand = TRUE,
                fill = TRUE
            )

            g_btns <- gvbox(container = g_vars)
            addSpring(g_btns)
            btn_add <<- gimagebutton("forward",
                size = "large_toolbar",
                container = g_btns,
                tooltip = "Add selected",
                handler = function(h, ...) {
                    add_vars(svalue(dataVars, index = TRUE))
                }
            )
            btn_rmv <<- gimagebutton("backward",
                size = "large_toolbar",
                container = g_btns,
                tooltip = "Remove selected",
                handler = function(h, ...) {
                    rmv_vars(svalue(chosenVars, index = TRUE))
                }
            )
            addSpring(g_btns)

            chosenVars <<- gtable(data.frame(Keep = ""),
                multiple = TRUE,
                container = g_vars,
                expand = TRUE,
                fill = TRUE
            )

            g_btns2 <- gvbox(container = g_vars)
            btn_up <<- gimagebutton("1uparrow",
                size = "large_toolbar",
                container = g_btns2,
                tooltip = "Move selected variable up",
                handler = function(h, ...) {
                    if (reordering) return()
                    reordering <<- TRUE
                    on.exit(reordering <<- FALSE)

                    index <- svalue(chosenVars, index = TRUE)
                    if (length(index) != 1) return()
                    if (index == 1) return()
                    keep <- chosenVars$get_items()
                    keep_index <- seq_along(keep)
                    keep_index[index] <- index - 1
                    keep_index[index - 1] <- index
                    keep <- keep[keep_index]
                    chosenVars$set_items(data.frame(Keep = keep))
                    svalue(chosenVars) <<- index - 1
                }
            )
            btn_down <<- gimagebutton("1downarrow",
                size = "large_toolbar",
                container = g_btns2,
                tooltip = "Move selected variable down",
                handler = function(h, ...) {
                    if (reordering) return()
                    reordering <<- TRUE
                    on.exit(reordering <<- FALSE)

                    index <- svalue(chosenVars, index = TRUE)
                    if (length(index) != 1) return()
                    keep <- chosenVars$get_items()
                    if (index == length(keep)) return()
                    keep_index <- seq_along(keep)
                    keep_index[index] <- index + 1
                    keep_index[index + 1] <- index
                    keep <- keep[keep_index]
                    chosenVars$set_items(data.frame(Keep = keep))
                    svalue(chosenVars) <<- index + 1
                }
            )

            add_body(g_vars, fill = TRUE, expand = TRUE)

            # controls
            g_ctrls <- ggroup()
            add_all_btn <- gbutton("Add all",
                container = g_ctrls,
                handler = function(h, ...) {
                    add_vars(-1)
                }
            )

            addSpring(g_ctrls)
            glabel("Sort (alphabetically): ", container = g_ctrls)
            sort_inc <- gimagebutton(
                "sort-ascending",
                size = "large_toolbar",
                container = g_ctrls,
                handler = function(h, ...) {
                    keep <- chosenVars$get_items()
                    chosenVars$set_items(
                        data.frame(Keep = sort(keep))
                    )
                }
            )
            sort_desc <- gimagebutton(
                "sort-descending",
                size = "large_toolbar",
                container = g_ctrls,
                handler = function(h, ...) {
                    keep <- chosenVars$get_items()
                    chosenVars$set_items(
                        data.frame(Keep = sort(keep, decreasing = TRUE))
                    )
                }
            )
            add_body(g_ctrls)
        },
        add_vars = function(index) {
            remove <- as.character(dataVars$get_items())
            keep <- as.character(chosenVars$get_items())

            if (index[1] == -1) {
                keep <- c(keep, remove)
                remove <- ""
            } else {
                keep <- c(keep, remove[index])
                remove <- remove[-index]
                if (length(remove) == 0) remove <- ""
            }
            keep <- keep[keep != ""]

            dataVars$set_items(data.frame(Remove = remove))
            chosenVars$set_items(data.frame(Keep = keep))
        },
        rmv_vars = function(index) {
            remove <- as.character(dataVars$get_items())
            keep <- as.character(chosenVars$get_items())

            if (index[1] == -1) {
                remove <- c(remove, keep)
                keep <- ""
            } else {
                remove <- c(remove, keep[index])
                keep <- keep[-index]
                if (length(keep) == 0) keep <- ""
            }
            remove <- remove[remove != ""]

            dataVars$set_items(data.frame(Remove = remove))
            chosenVars$set_items(data.frame(Keep = keep))
        },
        reorder_select_vars = function() {
            vars <- as.character(chosenVars$get_items())
            if (length(vars) == 1 && vars == "") {
                gmessage("Add variables to the 'Keep' column on the right first.",
                    title = "No variables selected",
                    icon = "warning",
                    parent = GUI$modWin
                )
                return()
            }

            .dataset <- GUI$get_data_object(lazy = FALSE)
            .d <- if (iNZightTools::is_survey(.dataset)) .dataset$variables else .dataset
            if (identical(vars, colnames(.d))) {
                gmessage("It looks like you have selected all of the variables in the same order.",
                    title = "No change to variables",
                    icon = "warning",
                    parent = GUI$modWin
                )
                return()
            }

            newdata <- iNZightTools::select_vars(.dataset, vars)
            GUI$new_document(data = newdata,
                suffix = ifelse(length(vars) == ncol(.dataset), "reorder", "subset")
            )

            close()
        }
    )
)



## --------------------------------------------
## Class that handles the reshaping of a dataset
## --------------------------------------------

iNZReshapeWin <- setRefClass(
    "iNZReshapeWin",
    fields = list(
        colname = "ANY",
        key = "ANY",
        value = "ANY",
        newview = "ANY",
        col1 = "ANY",
        col2 = "ANY",
        type = "ANY",
        check = "ANY"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui = NULL) {

            ok <- callSuper(gui,
                title = "Reshape dataset",
                width = "med",
                height = "large",
                ok = "Reshape",
                action = .self$do_reshape,
                help = "user_guides/data_options/#reshape",
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("do_reshape")


            format_string <- glabel("Select reshape mode")
            add_body(format_string)

            format <- gcombobox(
                items = c("", "Wide to long", "Long to wide"),
                handler = function(h, ...) {
                    type <<- svalue(format)
                    newview$set_items("")
                    visible(previewbox) <- TRUE
                    enabled(ok_button) <<- TRUE
                    if (type == "Wide to long") {
                        visible(group1) <- TRUE
                        visible(group2) <- FALSE
                        check <<- "wide"
                    } else if (type == "Long to wide") {
                        visible(group2) <- TRUE
                        visible(group1) <- FALSE
                        check <<- "long"
                    } else {
                        visible(group1) <- FALSE
                        visible(group2) <- FALSE
                        visible(previewbox) <- FALSE
                        enabled(ok_button) <<- FALSE
                    }
                }
            )
            add_body(format)

            ## Wide to long
            group1 <- gvbox()

            col_string <- glabel("Select column(s) to gather together", container = group1)

            colname <<- ""
            var1 <- gcombobox(c("", names(GUI$getActiveData(lazy = TRUE))),
                container = group1,
                handler = function(h, ...) {
                    colname <<- svalue(var1)
                    if (colname == "") {
                        newview$set_items("")
                    } else {
                        updatePreview()
                    }
                }
            )

            var2box <- gvbox(container = group1)
            var2 <- gtable(names(GUI$getActiveData(lazy = TRUE)),
                multiple = TRUE,
                expand = TRUE,
                container = var2box)
            addHandlerSelectionChanged(var2,
                function(h, ...) {
                    colname <<- svalue(var2)
                    updatePreview()
                }
            )

            names(var2) <- "Variables"
            visible(var2box) <- FALSE
            size(var2box) <- c(-1, 150)

            checkbox <- gcheckbox(text = "Click to select multiple columns",
                container = group1,
                handler = function(h, ...) {
                    if (svalue(checkbox) == TRUE) {
                        visible(var2box) <- TRUE
                        visible(var1) <- FALSE
                        colname <<- svalue(var2)
                        newview$set_items("")
                    } else {
                        visible(var2box) <- FALSE
                        visible(var1) <- TRUE
                        colname <<- svalue(var1)
                        newview$set_items("")
                    }
                }
            )

            key <<- "key"
            key_string <- glabel(
                "Name the new column containing the old column names",
                container = group1)
            keybox <- gedit("key", container = group1)
            addHandlerKeystroke(keybox,
                function(h, ...) {
                    key <<- ifelse(svalue(keybox) == "", "key", svalue(keybox))
                    updatePreview()
                }
            )

            value <<- "value"
            value_string <- glabel(
                "Name the new column containing the old column values",
                container = group1)
            valuebox <- gedit("value", container = group1)
            addHandlerKeystroke(valuebox,
                function(h,...) {
                    value <<- ifelse(svalue(valuebox) == "", "value", svalue(valuebox))
                    updatePreview()
                }
            )
            visible(group1) <- FALSE
            add_body(group1)

            ## Long to wide
            group2 <- gvbox()

            col1 <<- ""
            label1 <- glabel(
                "Select the column to spread out to multiple columns",
                container = group2)
            col1box <- gcombobox(items = c("", names(GUI$getActiveData(lazy = TRUE))),
                container = group2,
                handler = function(h, ...) {
                    col1 <<- svalue(col1box)
                    if (col1 != "" & col2 != "") {
                        updatePreview()
                    } else {
                        newview$set_items("")
                    }
                }
            )

            col2 <<- ""
            label2 <- glabel(
                "Select the column with the values to be put in these column",
                container = group2)
            col2box <- gcombobox(items = c("", names(GUI$getActiveData(lazy = TRUE))),
                container = group2,
                handler = function(h,...) {
                    col2 <<- svalue(col2box)
                    if (col1 != "" & col2 != "") {
                        updatePreview()
                    } else {
                        newview$set_items("")
                    }
                }
            )
            visible(group2) <- FALSE
            add_body(group2)

            ## Preview window
            previewbox <- gvbox()
            prevTbl <- glayout(homogeneous = FALSE, container = previewbox)

            string1 <- glabel("Original dataset")
            originview <- gtable(
                data.frame(head(GUI$getActiveData(lazy = TRUE)),
                    stringsAsFactors = TRUE)
            )
            prevTbl[1,1, expand = TRUE] <- string1
            prevTbl[2,1, expand = TRUE] <- originview
            size(originview) = c(-1, 250)

            string2 <- glabel("New dataset")
            newview <<- gtable(data.frame("", stringsAsFactors = TRUE))
            prevTbl[1,2, expand = TRUE] <- string2
            prevTbl[2,2, expand = TRUE] <- newview
            size(newview) <<- c(-1, 250)

            add_body(previewbox, expand = TRUE, fill = TRUE)

            visible(previewbox) <- FALSE
            enabled(ok_button) <<- FALSE
        },
        updatePreview = function() {
            d <- reshape()
            newview$set_items(d)
        },
        reshape = function() {
            .dataset <- GUI$getActiveData(lazy = FALSE)
            data_to <- dplyr::case_match(check, "long" ~ "wide", "wide" ~ "long")
            df <- iNZightTools::reshape_data(.dataset, data_to, cols = colname, names_to = key, values_to = value, names_from = col1, values_from = col2)
        },
        do_reshape = function() {
            .dataset <- GUI$getActiveData(lazy = FALSE)
            data <- reshape()
            attr(data, "name") <-
                paste(attr(.dataset, "name", exact = TRUE), "reshaped", sep = ".")
            attr(data, "code") <-
                gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
            GUI$setDocument(iNZDocument$new(data = data, preferences = GUI$preferences))

            close()
        }
    )
)


## --------------------------------------------
## Class that handles the separating of a dataset
## --------------------------------------------
iNZSeparateWin <- setRefClass(
    "iNZSeparateWin",
    fields = list(
        format = "ANY",
        var1 = "ANY", var2 = "ANY",
        col = "ANY",
        sep = "ANY",
        check = "ANY",
        newview = "ANY",
        box = "ANY",
        coltimer = "ANY",
        leftCol = "ANY", rightCol = "ANY",
        namelist = "ANY",
        dtpreview = "ANY",
        separatebtn = "ANY"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui = NULL) {
            ok <- callSuper(gui,
                title = "Separate columns",
                width = "med",
                height = "large",
                ok = "Separate",
                action = .self$do_separate,
                help = "user_guides/data_options/#separate",
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("do_separate")

            initFields(
                sep = "_",
                check = "Column"
            )

            input_tbl <- glayout()
            ii <- 1L

            format.list <- c("Columns", "Rows")
            format_string <- glabel("Separate variable into :")
            input_tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- format_string

            if (iNZightTools::is_survey(GUI$get_data_object(lazy = TRUE))) {
                format <<- glabel(format.list[[1]])
                input_tbl[ii, 2:3, anchor = c(-1, 0), fill = TRUE] <- format
                check <<- "Column"
                col <<- ""
                sep <<- ""
            } else {
                format <<- gradio(items = format.list,
                    horizontal = TRUE,
                    handler = function(h, ...) {
                        col <<- ""
                        sep <<- ""
                        var1$set_value(" ")
                        var2$set_value("")
                        newview$set_items("")
                        check <<- gsub("s", "", svalue(format))
                    }
                )
                size(format) <<- c(350, -1)
                input_tbl[ii, 2:3] <- format
            }
            ii <- ii + 1L

            col_string <- glabel("Select column to separate out :")
            var1 <<- gcombobox(c(" ", names(GUI$getActiveData(lazy = TRUE))),
                handler = function(h, ...) {
                    col <<- svalue(var1)
                    updateView()
                }
            )
            size(var1) <<- c(350, -1)
            input_tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- col_string
            input_tbl[ii, 2:3] <- var1
            ii <- ii + 1L

            sep_string <- glabel("Value separator :")
            var2 <<- gedit(sep)
            addHandlerKeystroke(var2,
                function(h ,...) {
                    sep <<- svalue(var2)
                    updateView()
                }
            )
            size(var2) <<- c(350, -1)
            input_tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- sep_string
            input_tbl[ii, 2:3] <- var2
            ii <- ii + 1L

            lbl <- glabel("Separated column names :")
            leftCol <<- gedit("")
            rightCol <<- gedit("")
            input_tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            input_tbl[ii, 2L, fill = TRUE] <- leftCol
            input_tbl[ii, 3L, fill = TRUE] <- rightCol
            ii <- ii + 1L

            coltimer <<- NULL
            sfun <- function(data) updateView()
            addHandlerKeystroke(leftCol,
                handler = function(h, ...) {
                    if (!is.null(coltimer))
                    if (coltimer$started)
                        coltimer$stop_timer()
                    coltimer <<- gtimer(300, sfun, one.shot = TRUE)
                }
            )
            addHandlerKeystroke(rightCol,
                handler = function(h, ...) {
                    if (!is.null(coltimer))
                    if (coltimer$started)
                        coltimer$stop_timer()
                    coltimer <<- gtimer(300, sfun, one.shot = TRUE)
                }
            )

            add_body(input_tbl)
            body_space(15)

            prevTbl <- glayout(homogeneous = FALSE)

            string1 <- glabel("Original dataset")
            originview <- gtable(data.frame(head(GUI$getActiveData(lazy = TRUE), 10L), stringsAsFactors = TRUE))
            prevTbl[1,1, expand = TRUE] <- string1
            prevTbl[2,1, expand = TRUE] <- originview
            size(originview) = c(-1, 350)

            string2 <- glabel("New dataset")
            newview <<- gtable(data.frame("", stringsAsFactors = TRUE))
            prevTbl[1,2, expand = TRUE] <- string2
            prevTbl[2,2, expand = TRUE] <- newview
            size(newview) <<- c(-1, 350)

            add_body(prevTbl)
        },
        separatedt = function(preview = TRUE) {
            if (sep == "") return()

            data <- if (preview) GUI$get_data_object(nrow = 10L) else GUI$get_data_object(lazy = FALSE)
            left <- iNZightTools::make_names(svalue(leftCol), names(data))
            right <- iNZightTools::make_names(svalue(rightCol), names(data))
            if (check == "Column") {
                if (left == "" || right == "") {
                    splitlist <- c("_", ".", "-")
                    for (split in splitlist) {
                        x <- strsplit(col, split)[[1]]
                        if (length(x) == 2L) {
                            blockHandlers(leftCol)
                            blockHandlers(rightCol)
                            svalue(leftCol) <<- left <- iNZightTools::make_names(x[1], names(data))
                            svalue(rightCol) <<- right <- iNZightTools::make_names(x[2], names(data))
                            unblockHandlers(leftCol)
                            unblockHandlers(rightCol)
                        }
                    }
                }

                tmp <- iNZightTools::separate_var(data, var = col, by = sep, names = c(left, right), into = "cols")

                if (iNZightTools::is_survey(tmp) && preview) tmp <- tmp$variables

            } else if (check == "Row") {
                tmp <- iNZightTools::separate_var(data, var = col, by = sep, into = "rows")
            }
            return(tmp)
        },
        updateView = function(){
            if (col != " " & sep != "") {
                namelist <<- list()
                dtpreview <<- separatedt()
                newview$set_items(dtpreview)
            } else {
                newview$set_items("")
            }
        },
        do_separate = function() {
            .dataset <- GUI$get_data_object(lazy = FALSE)
            newdata <- separatedt(preview = FALSE)
            GUI$new_document(newdata, "separated")
            close()
        }
    )
)



## --------------------------------------------
## Class that handles the uniting of a dataset
## --------------------------------------------
iNZUniteWin <- setRefClass(
    "iNZUniteWin",
    fields = list(
        var1 = "ANY", var2 = "ANY", var3 = "ANY",
        sep = "ANY",
        col = "ANY",
        name = "ANY",
        newview = "ANY",
        unitebtn = "ANY"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui = NULL) {
            ok <- callSuper(gui,
                title = "Unite columns",
                width = "med",
                height = "large",
                ok = "Unite",
                action = .self$do_unite,
                help = "user_guides/data_options/#unite",
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("do_unite")

            g_top <- ggroup()
            g_cols <- gvbox(container = g_top)
            addSpace(g_top, 20)
            g_info <- gvbox(container = g_top)

            col_string <- glabel("Select columns to unite")
            font(col_string) <- list(weight = "bold")
            add(g_cols, col_string, anchor = c(-1, 0))

            var1 <<- gtable(names(GUI$getActiveData(lazy = TRUE)),
                multiple = TRUE,
                expand = TRUE,
                container = g_cols
            )
            addHandlerSelectionChanged(var1,
                function(h, ...) {
                    col <<- svalue(var1)
                    name <<- paste(col, collapse = sep)
                    svalue(var2) <<- name
                    updateView()
                }
            )
            size(var1) <<- c(300, 150)

            lbl <- glabel("New variable name")
            font(lbl) <- list(weight = "bold")
            add(g_info, lbl, anchor = c(-1, 0), fill = TRUE)
            var2 <<- gedit("", container = g_info)
            addHandlerKeystroke(var2,
                function(h, ...) {
                    name <<- ifelse(svalue(var2) == "", "newcol", svalue(var2))
                    updateView()
                }
            )

            sep <<- "_"
            lbl <- glabel("Value separator")
            font(lbl) <- list(weight = "bold")
            add(g_info, lbl, anchor = c(-1, 0), fill = TRUE)
            var3 <<- gedit("_", container = g_info)
            addHandlerKeystroke(var3,
                function(h, ...) {
                    sep <<- svalue(var3)
                    updateView()
                }
            )

            add_body(g_top)

            prevTbl <- glayout(homogeneous = FALSE)

            string1 <- glabel("Original dataset")
            originview = gtable(data.frame(head(GUI$getActiveData(lazy = TRUE), 10L), stringsAsFactors = TRUE))
            prevTbl[1,1, expand = TRUE] <- string1
            prevTbl[2,1, expand = TRUE] <- originview
            size(originview) = c(-1, 350)

            string2 <- glabel("New dataset")
            newview <<- gtable(data.frame("", stringsAsFactors = TRUE))
            prevTbl[1,2, expand = TRUE] <- string2
            prevTbl[2,2, expand = TRUE] <- newview
            size(newview) <<- c(-1, 350)

            add_body(prevTbl)
        },
        updateView = function() {
            data <- GUI$get_data_object(nrow = 10L)
            df <- iNZightTools::combine_vars(data, vars = col, sep, name)
            if (iNZightTools::is_survey(df)) df <- df$variables
            newview$set_items(df)
        },
        do_unite = function() {
            .dataset <- GUI$get_data_object(lazy = FALSE)
            newdata <- iNZightTools::combine_vars(.dataset, vars = col, sep, name)
            GUI$new_document(newdata, "united")
            close()
        }
    )
)


iNZexpandTblWin <- setRefClass(
    "iNZexpandTblWin",
    fields = list(GUI = "ANY"),
    methods = list(
        initialize = function(gui = NULL) {
            initFields(GUI = gui)
            if (is.null(GUI)) return()
            try(dispose(GUI$modWin), silent = TRUE)

            conf <- gconfirm(
                paste(
                    "This will expand the table to individual rows.",
                    "You can revert to the original data using the 'Data set' select box.",
                    "Note: you can get the same effect by specifying a frequency column.",
                    sep = "\n\n"
                ),
                title = "Expand table?",
                icon = "question",
                parent = GUI$win
            )

            if (!conf) return()

            dat <- GUI$getActiveData(lazy = FALSE)
            dat <- tryCatch(
                {
                    as.numeric(rownames(dat))
                    dat
                },
                warning = function(w) {
                    ## cannot convert rownames to numeric - create column
                    dat$Row <- rownames(dat)
                    dat
                }
            )
            numIndices <- sapply(dat, function(x) is_num(x))

            long <- reshape2:::melt.data.frame(dat,
                measure.vars = colnames(dat)[numIndices],
                variable.name = "Column",
                value.name = "Count",
                na.rm = TRUE
            )
            out <- long[rep(rownames(long), long$Count), ]
            rownames(out) <- 1:nrow(out)

            ## for 1-way tables, don't need the "Count" column!
            if (length(unique(out$Column)) == 1)
                out$Column <- NULL
            out$Count <- NULL

            GUI$new_document(out, "expanded")
        }
    )
)


## --------------------------------------------
## Class that handles the joining of the original dataset with a new dataset
## --------------------------------------------
iNZJoinWin <- setRefClass(
    "iNZJoinWin",
    fields = list(
        newdata = "ANY",
        prevTbl = "ANY",
        left_col = "ANY",
        right_col = "ANY",
        data_name = "ANY",
        impview = "ANY",
        join_method = "ANY",
        left_name = "ANY",
        right_name = "ANY",
        joinview = "ANY",
        coltbl = "ANY",
        middle = "ANY",
        joinbtn = "ANY"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui = NULL) {
            ok <- callSuper(gui,
                title = "Join datasets by columns",
                width = "med",
                height = "large",
                ok = "Join",
                action = .self$do_join,
                help = "user_guides/data_options/#join",
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("do_join")

            prevTbl <<- glayout(homogeneous = FALSE)

            string1 <- glabel("Preview of the original dataset")
            originview <- gpagedtable(
                data.frame(head(GUI$getActiveData(lazy = TRUE), 10), stringsAsFactors = TRUE)
            )
            string2 <- glabel("Select join methods")
            jointypes <- list(
                "Inner Join" = "inner_join",
                "Left Join" = "left_join",
                "Full Join" = "full_join",
                "Semi Join" = "semi_join",
                "Anti Join" = "anti_join"
            )
            var1 <- gcombobox(
                items = names(jointypes),
                selected = 2,
                handler = function(h, ...) {
                    join_method <<- jointypes[[svalue(var1)]]
                    updatePreview()
                }
            )

            join_method <<- "left_join"
            enabled(var1) <- !iNZightTools::is_survey(GUI$get_data_object(lazy = TRUE))

            left_name_box <- gvbox()
            name_string <- glabel("Duplicated cols: suffix for Original",
                container = left_name_box,
                anchor = c(-1, 0)
            )
            left_name <<- "Orig"
            left_name_string <- gedit("Orig", container = left_name_box)
            addHandlerKeystroke(left_name_string,
                function(h, ...) {
                    left_name <<- svalue(left_name_string)
                    updatePreview()
                }
            )

            prevTbl[1,1, expand = TRUE] <<- string1
            prevTbl[2,1, expand = TRUE] <<- originview$block
            prevTbl[3,1, expand = TRUE] <<- string2
            prevTbl[4,1, expand = TRUE] <<- var1
            prevTbl[5,1, expand = TRUE] <<- left_name_box
            size(originview$table) <- c(-1, 200)

            string3 <- glabel("Preview of the second dataset")
            impview <<- gpagedtable(data.frame("", stringsAsFactors = TRUE))

            data2frombox <- ggroup()
            data2from <- gradio(c("Existing", "Import new"),
                horizontal = TRUE,
                container = data2frombox
            )
            data_name <<- glabel("test")
            right_name_box <- gvbox()
            name_string <- glabel("Duplicated cols: suffix for New",
                container = right_name_box,
                anchor = c(-1, 0)
            )
            right_name <<- "New"
            right_name_string = gedit("New", container = right_name_box)
            addHandlerKeystroke(right_name_string,
                function(h, ...) {
                    right_name <<- svalue(right_name_string)
                    updatePreview()
                }
            )


            prevTbl[1,2, expand = TRUE] <<- string3
            prevTbl[2,2, expand = TRUE] <<- impview$block
            prevTbl[3,2, expand = TRUE] <<- data2frombox
            prevTbl[4,2, expand = TRUE] <<- data_name
            prevTbl[5,2, expand = TRUE] <<- right_name_box
            size(impview$table) <<- c(-1, 200)

            addHandlerChanged(data2from,
                handler = function(h, ...) {
                    # delete current
                    prevTbl$remove_child(data_name)
                    dispose(data_name)

                    switch(svalue(h$obj, index = TRUE),
                        {
                            # if choose existing, show dropdown of available datasets (MINUS the current)
                            data_set_names <- GUI$dataNameWidget$nameLabel$get_items()
                            data_set_names <- data_set_names[data_set_names != GUI$dataNameWidget$datName]
                            if (length(data_set_names)) {
                                data_name <<- gcombobox(data_set_names,
                                    selected = 0,
                                    handler = function(h, ...) {
                                        if (svalue(h$obj) == "") {
                                            newdata <<- NULL
                                            set_second_data()
                                            return()
                                        }
                                        i <- sapply(GUI$dataNameWidget$nameLabel$get_items(),
                                            function(x) x == svalue(h$obj))
                                        newdata <<- GUI$iNZDocuments[[which(i)[1]]]$getData()
                                        set_second_data()
                                    }
                                )
                            } else {
                                data_name <<- glabel("No datasets available")
                            }
                        },
                        {
                            # else show file chooser
                            data_name <<- gfilebrowse(
                                text = "Specify a file",
                                initial.dir = file.path(".", "data"),
                                handler = function(h, ...) {
                                    newdata <<- iNZightTools::smart_read(svalue(data_name))
                                    set_second_data()
                                }
                            )
                        }
                    )
                    prevTbl[4, 2, expand = TRUE] <<- data_name
                }
            )
            data2from$invoke_change_handler()

            add_body(prevTbl)

            ## Middle box
            middle <<- gvbox()
            coltbl <<- glayout(container = middle)
            coltbl[1, 1:4] <<- glabel("Please specify columns to match on from two datasets")

            add_body(middle)

            ## Bottom box
            bottom <- gvbox()
            preview_string2 = glabel("Preview", container = bottom, anchor = c(-1, 0))
            joinview <<- gpagedtable(data.frame("", stringsAsFactors = TRUE), container = bottom)
            size(joinview$table) <<- c(-1, 150)

            add_body(bottom)

            join_types_button <- gbutton("Join Methods",
                handler = function(h, ...) show_join_help()
            )
            add_toolbar(join_types_button)

        },
        set_second_data = function() {
            impview$set_items(head(newdata, 10))

            left_col <<- ""
            right_col <<- ""

            d1 <- tryCatch(
                joinData(),
                error = function(e) {
                    if (e$message == "`by` required, because the data sources have no common variables") {
                        a <- tibble::tibble()
                        attr(a, "join_cols") <- ""
                    }
                }
            )
            attr <- attr(d1, "join_cols")
            left_col <<- as.character(attr)
            right_col <<- left_col

            create_join_table()
            updatePreview()
        },
        updatePreview = function() {
            "update the preview window"
            d <- joinData()
            if (length(d) == 0) return()
            if (nrow(d) == 0) {
                joinview$set_items("Joined dataset has 0 row")
            } else {
                d[is.na(d)] <- "NA"
                joinview$set_items(head(d, 10))
            }
        },
        joinData = function() {
            if (length(left_col) != 0 & length(left_col) == length(right_col)) {
                ## checking for column types
                list <- list()
                for (i in 1:length(left_col)) {
                    orig_type <- class(GUI$getActiveData(lazy = TRUE)[[left_col[i]]])
                    new_type <- class(newdata[[right_col[i]]])
                    if (orig_type == new_type | orig_type == "character" &
                        new_type == "factor" | orig_type == "factor" &
                        new_type == "character") {
                        list <- append(list, TRUE)
                    } else {
                        list <- append(list, FALSE)
                    }
                }
                if (any("" %in% c(left_col, right_col), !length(left_col), !length(right_col))) {
                    by <- NULL
                } else {
                    by <- setNames(right_col, left_col)
                }
                ## Now left_col contains some column names and the matching columns from two datasets are in the same class so JOIN
                if (all(list == TRUE)) {
                    d <- iNZightTools::join_data(
                        GUI$getActiveData(lazy = FALSE),
                        newdata,
                        by,
                        how = gsub("_join$", "", join_method),
                        suffix_l = sprintf(".%s", left_name),
                        suffix_r = sprintf(".%s", right_name)
                    )
                    return(d)
                } else {
                    joinview$set_items("Selected columns are of different types")
                    return()
                }
            } else {
                joinview$set_items("Please specify columns to match on from two datasets")
                return()
            }
        },
        ## Create join table
        create_join_table = function() {
            if (length(coltbl$children) > 1L) {
                middle$remove_child(coltbl)
                coltbl <<- glayout()
                coltbl[1L, 1:4] <<- glabel("Please specify columns to match on from two datasets")
                middle$add_child(coltbl, fill = TRUE)
            }
            if (length(left_col) == 0L) {
                add_joinby_row(coltbl, 1L)
                joinview$set_items("Please specify columns to match on from two datasets")
                return()
            } else {
                for (i in 1:length(left_col)) {
                    add_joinby_row(coltbl, i)
                    number <- i + 1L
                    coltbl[number, 1L]$set_items(left_col[i])
                    coltbl[number, 1L]$set_value(left_col[i])
                    coltbl[number, 2L]$set_items(right_col[i])
                    coltbl[number, 2L]$set_value(right_col[i])
                }
            }
        },
        # Add joinby row
        add_joinby_row = function(coltbl, number) {
            n <- number + 1L
            coltbl[n, 1L] <<- gcombobox(
                c("", setdiff(names(GUI$getActiveData(lazy = TRUE)), left_col)),
                handler = function(h, ...) {
                    new_col <- svalue(coltbl[n, 1L])
                    left_col[number] <<- new_col
                    updatePreview()
                }
            )
            coltbl[n, 2L] <<- gcombobox(c("", setdiff(names(newdata), right_col)),
                handler = function(h, ...) {
                    new_col <- svalue(coltbl[n, 2L])
                    right_col[number] <<- new_col
                    updatePreview()
                }
            )
            coltbl[n, 3L] <<- gbutton('delete',
                handler = function(h, ...) {
                    remove_joinby_row(coltbl, n, left_col)
                }
            )
            coltbl[n, 4L] <<- gbutton('add',
                handler = function(h, ...) {
                    add_joinby_row(coltbl, length(left_col) + 1L)
                }
            )
        },
        ## Remove joinby row
        remove_joinby_row = function(coltbl, pos, left) {
            pos <- pos - 1L
            if (length(left_col) > 0L) {
                left_col <<- left[-pos]
                right_col <<- right_col[-pos]
            }
            create_join_table()
        },
        do_join = function() {
            .dataset <- GUI$getActiveData(lazy = FALSE)
            data <- joinData()
            # TODO: this should report a warning/error?
            if (length(data) == 0 | nrow(data) == 0)
                data <- GUI$getActiveData(lazy = TRUE)

            GUI$new_document(data, "joined")
            close()
        },
        show_join_help = function() {
            helpwin <- gwindow(title = "Join Methods", parent = GUI$modWin)
            win <- gvbox(container = helpwin)
            win$set_borderwidth(10)

            inner_join <- glabel("Inner Join", container = win, anchor = c(-1, 0))
            font(inner_join) <- list(size = 12, weight = "bold")
            inner_join_help <- glabel(
                add_lines("Keep all the matched rows within both datasets", 80),
                container = win, anchor = c(-1, 0))
            addSpace(win, 5)

            left_join <- glabel("Left Join", container = win, anchor = c(-1, 0))
            font(left_join) <- list(size = 12, weight = "bold")
            left_join_help <- glabel(
                add_lines(
                    paste(
                        "Keep every row in the original dataset and",
                        "match them to the imported dataset"
                    ),
                    50
                ),
                container = win, anchor = c(-1, 0)
            )
            addSpace(win, 5)

            full_join <- glabel("Full Join", container = win, anchor = c(-1, 0))
            font(full_join) <- list(size = 12, weight = "bold")
            full_join_help <- glabel(
                add_lines("Keep all the rows in both datasets", 50),
                container = win, anchor = c(-1, 0)
            )
            addSpace(win, 5)

            semi_join <- glabel("Semi Join", container = win, anchor = c(-1, 0))
            font(semi_join) <- list(size = 12, weight = "bold")
            semi_join_help <- glabel(
                add_lines("Keep matched rows in the original dataset ONLY", 50),
                container = win, anchor = c(-1, 0)
            )
            addSpace(win, 5)

            anti_join <- glabel("Anti Join", container = win, anchor = c(-1, 0))
            font(anti_join) <- list(size = 12, weight = "bold")
            anti_join_help <- glabel(
                add_lines(
                    paste(
                        "Return all rows in the original dataset which",
                        "do not have a match in the imported dataset"
                    ),
                    50
                ),
                container = win, anchor = c(-1, 0)
            )
            addSpace(win, 5)

        }
    )
)


## --------------------------------------------
## Class that handles appending new row to the dataset
## --------------------------------------------
iNZAppendRowsWin <- setRefClass(
    "iNZAppendRowsWin",
    fields = list(
        newdata = "ANY",
        date = "ANY"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui = NULL) {
            ok <- callSuper(gui,
                title = "Append new rows",
                width = "small",
                height = "small",
                ok = "Append",
                action = .self$do_append,
                help = "user_guides/data_options/#append",
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("do_append")

            file_string <- glabel("Import data")
            add_body(file_string, anchor = c(-1,0))

            data_name <- gfilebrowse(text = "Specify a file",
                initial.dir = file.path(".", "data"),
                handler = function(h, ...) {
                    newdata <<- iNZightTools::smart_read(svalue(data_name))
                }
            )
            add_body(data_name)

            date <<- FALSE
            check_box <- gcheckbox(
                "Tick if you want to attach a timestamp to the appended rows",
                handler = function(h, ...) {
                    date <<- svalue(check_box)
                }
            )
            add_body(check_box)
        },
        appendrow = function() {
            data <- GUI$getActiveData(lazy = TRUE)
            oldcols <- names(data)
            newcols <- names(newdata)
            common <- intersect(oldcols, newcols)
            if (length(common) != 0) {
                for (i in 1:length(common)) {
                    colname <- common[i]
                    if (class(data[[colname]]) != class(newdata[[colname]])) {
                        names(data)[which(names(data) == colname)] <-
                            paste0(colname, class(data[[colname]]))
                        names(newdata)[which(names(newdata) == colname)] <<-
                            paste0(colname, class(newdata[[colname]]))
                    }
                }
            }
            iNZightTools::append_rows(as.data.frame(data), new_data = newdata, when_added = date)
        },
        do_append = function() {
            .dataset <- GUI$getActiveData(lazy = FALSE)
            data <- appendrow()
            GUI$new_document(data, "appended")
            close()
        }
    )
)

## Data dictionary class
iNZDataDict <- setRefClass(
    "iNZDataDict",
    contains = "iNZWindow",
    fields = list(
        file_picker = "ANY",
        vars = "character",
        apply_dict = "ANY",
        apply_to_all = "ANY",
        dict = "ANY",
        dict_name = "ANY",
        dict_type = "ANY",
        dict_title = "ANY",
        dict_description = "ANY",
        dict_units = "ANY",
        dict_codes = "ANY",
        dict_values = "ANY",
        dict_separator = "ANY",
        dict_preview = "ANY",
        updating = "logical"
    ),
    methods = list(
        initialize = function(gui = NULL, ...) {
            ok <- callSuper(gui,
                title = "Load Data Dictionary",
                width = "large",
                height = "large",
                ok = "Load",
                action = .self$do_load,
                help = "user_guides/data_options/#data-dictionary",
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("do_load")

            initFields(
                updating = FALSE
            )

            add_heading(
                "Choose a file containing a CSV- or Excel-formatted data dictionary."
            )

            g_file <- ggroup()
            lb <- glabel("Data dictionary file :",
                container = g_file
            )
            font(lb) <- list(weight = "bold")
            file_picker <<- gfilebrowse(container = g_file,
                handler = function(h, ...) {
                    update_vars()
                }
            )
            addSpring(g_file)

            add_body(g_file)

            tbl <- glayout()
            ii <- 1L

            dict_name <<- gcombobox("", handler = function(h, ...) update_preview())
            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- glabel("Variable name :")
            tbl[ii, 2L, expand = TRUE] <- dict_name
            ii <- ii + 1L

            dict_type <<- gcombobox("", handler = function(h, ...) update_preview())
            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- glabel("Variable type :")
            tbl[ii, 2L, expand = TRUE] <- dict_type
            ii <- ii + 1L

            dict_title <<- gcombobox("", handler = function(h, ...) update_preview())
            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- glabel("Friendly name/title :")
            tbl[ii, 2L, expand = TRUE] <- dict_title
            ii <- ii + 1L

            dict_description <<- gcombobox("", handler = function(h, ...) update_preview())
            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- glabel("Description :")
            tbl[ii, 2L, expand = TRUE] <- dict_description
            ii <- ii + 1L

            ii <- 1L

            dict_units <<- gcombobox("", handler = function(h, ...) update_preview())
            tbl[ii, 3L, anchor = c(1, 0), expand = TRUE] <- glabel("Units :")
            tbl[ii, 4L, expand = TRUE] <- dict_units
            ii <- ii + 1L

            dict_codes <<- gcombobox("", handler = function(h, ...) update_preview())
            tbl[ii, 3L, anchor = c(1, 0), expand = TRUE] <- glabel("Factor codes :")
            tbl[ii, 4L, expand = TRUE] <- dict_codes
            ii <- ii + 1L

            dict_values <<- gcombobox("", handler = function(h, ...) update_preview())
            tbl[ii, 3L, anchor = c(1, 0), expand = TRUE] <- glabel("Factor labels :")
            tbl[ii, 4L, expand = TRUE] <- dict_values
            ii <- ii + 1L

            dict_separator <<- gedit("|")
            tbl[ii, 3L, anchor = c(1, 0), expand = TRUE] <- glabel("Code/level separator :")
            tbl[ii, 4L, expand = TRUE] <- dict_separator
            ii <- ii + 1L

            add_body(tbl)

            dict_preview <<- gdf(data.frame(Preview = "Choose variables for boxes above"))
            size(dict_preview) <<- c(-1, 100)

            add_body(dict_preview, expand = TRUE, fill = TRUE)

            apply_dict <<- gcheckbox(
                "Apply dictionary to current data set",
                checked = TRUE,
                handler = function(h, ...) enabled(apply_to_all) <<- svalue(h$obj)
            )
            add_body(apply_dict)

            apply_to_all <<- gcheckbox(
                "Apply to all loaded data sets",
                checked = FALSE
            )
            add_body(apply_to_all)
        },
        update_vars = function() {
            file <- svalue(file_picker)
            if (file == "") return()
            vars <<- colnames(iNZightTools::smart_read(file))

            # this prevents update_preview() from running while setting vars
            updating <<- TRUE
            on.exit(updating <<- FALSE)

            # some guessing can go on here
            dict_name$set_items(vars)
            dict_name$set_index(1L)

            dict_type$set_items(c("", vars))
            if ("type" %in% vars) dict_type$set_value("type")

            dict_title$set_items(c("", vars))
            if ("title" %in% vars) dict_title$set_value("title")

            dict_description$set_items(c("", vars))
            if ("description" %in% vars) dict_description$set_value("description")

            dict_units$set_items(c("", vars))
            if ("units" %in% vars) dict_units$set_value("units")

            dict_codes$set_items(c("", vars))
            if ("codes" %in% vars) dict_codes$set_value("codes")

            dict_values$set_items(c("", vars))
            if ("values" %in% vars) dict_values$set_value("values")

            updating <<- FALSE
            update_preview()

        },
        val_or_null = function(x) {
            x <- svalue(x)
            if (length(x) == 0L || x == "") return(NULL)
            x
        },
        update_preview = function() {
            if (updating) return()
            file <- svalue(file_picker)
            if (file == "") return()

            arglist <- list(
                file = file,
                name = val_or_null(dict_name),
                type = val_or_null(dict_type),
                title = val_or_null(dict_title),
                description = val_or_null(dict_description),
                units = val_or_null(dict_units),
                codes = val_or_null(dict_codes),
                values = val_or_null(dict_values),
                level_separator = svalue(dict_separator)
            )
            arglist <- modifyList(list(), arglist) # drop NULLs

            dict <<- try(
                do.call(iNZightTools::read_dictionary, arglist),
                silent = TRUE
            )

            if (inherits(dict, "try-error")) {
                print("Error ...")
                print(dict)
                dict <<- NULL
                dict_preview$set_frame(
                    data.frame(
                        Error = "Try choosing variables from boxes above."
                    )
                )
                return()
            }

            dd <- iNZightTools::as_tibble(dict, n = 20L)
            for (i in colnames(dd)) {
                if (is.character(dd[[i]])) {
                    dd[[i]] <- stringr::str_trunc(dd[[i]], 30L)
                }
            }
            dict_preview$set_frame(dd)

        },
        do_load = function() {
            # load dict from file and (optionally) append to dataz
            iwin <- gwindow("Loading data dictionary",
                width = 300,
                height = 100,
                parent = window(),
                visible = FALSE
            )
            ig <- gvbox(container = iwin)
            addSpring(ig)
            ig$set_borderwidth(10)
            glabel("Loading data dictionary and applying to data set.",
                container = ig
            )
            glabel("This may take a few moments ...", container = ig)
            addSpring(ig)
            visible(iwin) <- TRUE
            Sys.sleep(0.01)
            on.exit(try(dispose(iwin), silent = TRUE))

            if (svalue(apply_dict) && svalue(apply_to_all)) {
                lapply(GUI$iNZDocuments, function(x) x$setDictionary(dict, apply = TRUE))
            } else {
                GUI$getActiveDoc()$setDictionary(dict, apply = svalue(apply_dict))
            }
            close()
        }
    )
)

iNZDDView <- setRefClass(
    "iNZDDView",
    contains = "iNZWindow",
    fields = list(
        dict_df = "ANY",
        dd_view = "ANY",
        search_box = "ANY",
        match_case_chk = "ANY",
        search_btn = "ANY",
        clear_search_btn = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Data Dictionary",
                width = "large",
                height = "large",
                ok = "Close",
                cancel = NULL,
                action = .self$close,
                help = "user_guides/data_options/#data-dictionary",
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())

            dict_df <<- GUI$getActiveDoc()$getModel()$dict_df
            cn <- colnames(dict_df)
            for (i in cn[!cn %in% c("code", "value")]) {
                if (is.character(dict_df[[i]])) {
                    dict_df[[i]] <<- stringr::str_wrap(dict_df[[i]], 80L)
                }
            }

            ctrls_tbl <- glayout()
            ii <- 1L

            search_box <<- gedit("",
                initial.msg = "Enter search term"
            )
            match_case_chk <<- gcheckbox("Case sensitive", checked = FALSE)
            search_btn <<- gbutton("Search",
                handler = function(h, ...) search()
            )
            clear_search_btn <<- gbutton("Clear",
                handler = function(h, ...) {
                    svalue(search_box) <<- ""
                    search()
                }
            )
            ctrls_tbl[ii, 1L, expand = TRUE] <- search_box
            ctrls_tbl[ii, 2L] <- match_case_chk
            ctrls_tbl[ii, 3L] <- search_btn
            ctrls_tbl[ii, 4L] <- clear_search_btn
            ii <- ii + 1L

            add_body(ctrls_tbl)

            dd_view <<- gdf(dict_df)
            dd_view$remove_popup_menu()
            add_body(dd_view, expand = TRUE, fill = TRUE)
        },
        search = function() {
            fields <- c("name", "title", "description", "value")
            fields <- fields[fields %in% names(dict_df)]
            term <- trimws(svalue(search_box))
            if (term == "") {
                dd_view$set_frame(dict_df)
                return()
            }
            match_case <- svalue(match_case_chk)
            if (!match_case) term <- tolower(term)
            match_mat <- sapply(dict_df[, fields, drop = FALSE],
                function(x) {
                    if (!match_case) x <- tolower(x)
                    grepl(term, x, fixed = TRUE)
                }
            )
            matches <- apply(match_mat, 1L, any)
            dd_view$set_frame(dict_df[matches, ])
        },
        close = function() {
            # suppress Gtk-CRITICAL warnings:
            body$remove_child(dd_view)
            callSuper()
        }
    )
)
