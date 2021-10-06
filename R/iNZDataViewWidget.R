iNZDataViewWidget <- setRefClass(
    "iNZDataViewWidget",
    fields = list(
        GUI = "ANY", ## the iNZight GUI object
        widget = "ANY",
        landingView = "ANY",
        dataGp = "ANY", ## group containing the 2 different view groups
        current = "character",
        dfView = "ANY", ## group that contains data view
        varView = "ANY", ## group that contains variable view
        data = "ANY",
        dfWidget = "ANY",
        paginate = "list",
        colPageGp = "ANY",
        btnColPrev = "ANY",
        btnColNext = "ANY",
        colPageLbl = "ANY",
        btnPrev = "ANY",
        btnNext = "ANY",
        pageLbl = "ANY",
        columns = "character",
        ## max size before dataview gets deactived
        dataThreshold = "numeric",
        varWidget = "ANY",
        searchBox = "ANY",
        searchGp = "ANY",
        block_update = "logical"
    ),
    methods = list(
        initialize = function(gui, dataThreshold) {
            initFields(
                GUI = gui,
                dfView = NULL,
                varView = NULL,
                dfWidget = NULL,
                current = "",
                dataThreshold = dataThreshold,
                paginate = list(col = 1L, row = 1L, ncol = 30L, nrow = 20L),
                block_update = FALSE
            )

            widget <<- gvbox(expand = TRUE)

            add(widget, init_search())

            dataGp <<- gvbox(expand = TRUE, container = widget)
            set_data()

            createLandingView()
            createDfView()
            createVarView()

            show("data")
        },
        show = function(what = c("data", "variables", "landing")) {
            what <- match.arg(what)
            if (current != "" && what == current) return()

            # delete existing:
            try(
                invisible(
                    sapply(dataGp$children,
                        function(x) delete(dataGp, x)
                    )
                ),
                silent = TRUE
            )

            if (nrow(GUI$getActiveData()) == 1L &&
                colnames(GUI$getActiveData())[1] == "empty")
            {
                show_landing()
                return()
            }

            switch(what,
                "landing" = ,
                "data" = show_data(),
                "variables" = show_variables()
            )
        },
        show_landing = function() {
            add(dataGp, landingView, expand = TRUE)
            current <<- "landing"
        },
        show_data = function() {
            add(dataGp, dfView, expand = TRUE)
            current <<- "data"
        },
        show_variables = function() {
            add(dataGp, varView, expand = TRUE)
            current <<- "variables"
        },
        init_search = function () {
            searchGp <<- ggroup()
            addSpace(searchGp, 5)

            lbl <- glabel("Filter/search variables :", container = searchGp)

            searchtimer <- NULL
            searchBox <<- gedit(
                handler = function(h, ...) {
                    matches <- grep(svalue(h$obj), names(GUI$getActiveData()),
                        ignore.case = TRUE)
                    if (length(matches) == 0)
                        matches <- NA_character_
                    else
                        matches <- names(GUI$getActiveData())[matches]

                    paginate$col <<- 1L
                    columns <<- matches
                    set_data()
                },
                container = searchGp,
                expand = TRUE
            )
            addHandlerKeystroke(searchBox,
                function(h, ...) {
                    if (!is.null(searchtimer))
                        if (searchtimer$started)
                            searchtimer$stop_timer()

                    searchtimer <- gtimer(300,
                        searchBox$invoke_change_handler,
                        one.shot = TRUE
                    )
                }
            )
            visible(searchGp) <<- FALSE
            invisible(searchGp)
        },
        toggle_search = function() {
            visible(searchGp) <<- !visible(searchGp)
        },
        set_data = function(update = TRUE) {
            if (length(columns) == 1L && is.na(columns)) {
                data <<- data.frame(
                    "No variables found" = NA
                )
                return()
            }
            data <<- GUI$getActiveData()
            if (length(columns)) data <<- data[, columns, drop = FALSE]
            nr <- nrow(data)
            nc <- ncol(data)
            page <- list(
                rows = paginate$row + seq_len(paginate$nrow) - 1L,
                cols = paginate$col + seq_len(paginate$ncol) - 1L
            )
            page$rows <- page$rows[page$rows <= nr]
            page$cols <- page$cols[page$cols <= nc]
            data <<- data[page$rows, page$cols, drop = FALSE]
            if (update) updateWidget()
        },
        ## recreate both views with active dataset
        updateWidget = function() {
            ## delete the currently displayed views
            try(
                invisible(
                    sapply(dataGp$children,
                        function(x) delete(dataGp, x)
                    )
                ),
                silent = TRUE
            )
            set_data(update = FALSE)

            ## (re)create the views, with any changes to data
            updateDfView()
            updateVarView()
            if (current == "") return()
            showing <- current
            current <<- ""
            show(showing)
        },
        ## only update the variable view
        updateVarView = function() {
            if (block_update) return()
            if (is.null(varView)) {
                createVarView()
                return()
            }

            ## prefix variable type to variable names
            vnames <- if (length(columns)) columns else colnames(GUI$getActiveData())

            vtypes <- sapply(GUI$getActiveData()[vnames],
                function(x)
                    switch(iNZightTools::vartype(x),
                        'num' = 'numeric',
                        'cat' = 'categorical',
                        'dt' = 'datetime'
                    )
            )

            vsmry <- sapply(GUI$getActiveData()[vnames],
                function(x) {
                    if (all(is.na(x))) return("All missing")
                    switch(iNZightTools::vartype(x),
                        'num' = {
                            paste(
                                c("min", "max"),
                                signif(range(x, na.rm = TRUE), 4),
                                collapse = ", "
                            )
                        },
                        'cat' = {
                            paste(length(levels(x)), "levels")
                        },
                        'dt' = {
                            paste(
                                as.character(range(x, na.rm = TRUE)),
                                collapse = " to "
                            )
                        }
                    )
                }
            )

            vmiss <- sapply(GUI$getActiveData()[vnames],
                function(x) sum(is.na(x))
            )

            varsDf <- data.frame(
                Name = vnames,
                Type = vtypes,
                Info = vsmry,
                Missing = vmiss
            )
            varWidget$set_items(varsDf)
        },
        ## only update the data.frame view
        updateDfView = function() {
            if (is.null(dfView)) {
                createDfView()
                return()
            }

            blockHandlers(dfWidget)
            on.exit(unblockHandlers(dfWidget))
            dfWidget$set_frame(data)
            dfWidget$add_dnd_columns()

            Nc <- ncol(GUI$getActiveData())
            colPageLbl$set_value(
                sprintf("Variables %s-%s of %s",
                    paginate$col,
                    min(Nc, paginate$col + paginate$ncol - 1L),
                    Nc
                )
            )
            enabled(btnColPrev) <<- paginate$col > 1L
            enabled(btnColNext) <<- paginate$col + paginate$ncol - 1L < Nc
            visible(colPageGp) <<- Nc > paginate$ncol

            Nr <- nrow(GUI$getActiveData())
            pageLbl$set_value(
                sprintf("Rows %s-%s of %s",
                    paginate$row,
                    min(Nr, paginate$row + paginate$nrow - 1L),
                    Nr
                )
            )
            enabled(btnPrev) <<- paginate$row > 1L
            enabled(btnNext) <<- paginate$row + paginate$nrow - 1L < Nr
        },
        createLandingView = function() {
            addCentered <- function(g, widget) {
                c <- ggroup(container = g)
                addSpring(c)
                add(c, widget)
                addSpring(c)
                invisible(NULL)
            }

            # only needs to run once
            landingView <<- gvbox()
            landingView$set_borderwidth(10)
            lbl <- glabel("To get started, Import a dataset",
                container = landingView,
                anchor = c(-1, 0)
            )
            font(lbl) <- list(size = 12)

            addSpace(landingView, 5)

            lbl <- glabel(
                paste(sep = "\n",
                    "If you have a dataset, click the 'Import data' button",
                    "above, or find it and other options in the 'File' menu.",
                    "",
                    "If you're just getting started, why not load one of",
                    "the 'Example Datasets'?"
                ),
                container = landingView,
                anchor = c(-1, 0)
            )

            exBtn <- gbutton("Load Example Data",
                handler = function(h, ...) iNZImportExampleWin$new(GUI)
            )
            exBtn$set_icon("gw-datasheet")
            addCentered(landingView, exBtn)

            addSpace(landingView, 10)

            lbl <- glabel(
                paste(sep = "\n",
                    "Not sure what to do? Check out the getting started guide!"
                ),
                container = landingView,
                anchor = c(-1, 0)
            )
            guideBtn <- gbutton("Getting Started with iNZight",
                handler = function(h, ...) help_page('user_guides/basics')
            )
            guideBtn$set_icon("gw-help_topic")
            addCentered(landingView, guideBtn)

        },
        ## create the data.frame view (invisible)
        createDfView = function() {
            dfView <<- gvbox(expand = TRUE)

            ## Column pager
            colPageGp <<- ggroup()
            visible(colPageGp) <<- FALSE
            addSpring(colPageGp)

            colPageLbl <<- glabel("", container = colPageGp)
            font(colPageLbl) <<- list(size = 8)

            btnColPrev <<- gbutton("",
                container = colPageGp,
                handler = function(h, ...) {
                    if (paginate$col == 1L) return()
                    paginate$col <<- paginate$col - paginate$ncol
                    updateWidget()
                }
            )
            btnColPrev$set_icon("go-back")

            btnColNext <<- gbutton("",
                container = colPageGp,
                handler = function(h, ...) {
                    if (paginate$col + paginate$ncol - 1L >= ncol(GUI$getActiveData())) return()
                    paginate$col <<- paginate$col + paginate$ncol
                    updateWidget()
                }
            )
            btnColNext$set_icon("go-forward")


            ## This will be paginated, at some stage:
            dfWidget <<- gdf(data, expand = TRUE)
            ## dfWidget$remove_popup_menu() ## - called by $add_dnd_columns()
            dfWidget$add_dnd_columns()
            add(dfView, dfWidget, expand = TRUE)

            ## if the data.frame gets edited, update the iNZDocument
            addHandlerChanged(
                dfWidget,
                handler = function(h, ...) {
                    di <- as.integer(rownames(dfWidget$get_frame()))
                    dj <- colnames(dfWidget$get_frame())
                    same <-
                        dfWidget$get_frame() == GUI$getActiveData()[di, dj, drop = FALSE] |
                        # only one is NA
                        (is.na(dfWidget$get_frame()) + is.na(GUI$getActiveData()[di, dj, drop = FALSE])) == 2L
                    same <- ifelse(is.na(same), FALSE, same)
                    if (all(same)) return()
                    if (sum(!same) > 1L) {
                        gmessage("Multiple values changed somehow ... ")
                        updateDfView()
                        return()
                    }

                    changed <- as.integer(which(!same, arr.ind = TRUE))
                    new <- dfWidget$get_frame()[changed[1], changed[2]]

                    if (iNZightTools::is_dt(new)) {
                        gmessage("Editing datetimes not supported.")
                        updateDfView()
                        return()
                    }

                    .dataset <- GUI$getActiveData()
                    code <- sprintf(".dataset[%i, \"%s\"] <- %s",
                        di[changed[1]],
                        dj[changed[2]],
                        ifelse(is.numeric(new), new, paste0("\"", new, "\""))
                    )
                    eval(parse(text = code))
                    attr(.dataset, "code") <- code
                    block_update <<- TRUE
                    on.exit(block_update <<- TRUE)
                    GUI$update_document(.dataset)
                }
            )

            # pagination
            pageGp <- ggroup(container = dfView)
            addSpring(pageGp)

            lbl <- glabel("Rows per page:")
            font(lbl) <- list(size = 8)
            add(pageGp, lbl)

            pageSize <- gspinbutton(10L, 100L, by = 10L,
                value = paginate$nrow,
                container = pageGp,
                handler = function(h, ...) {
                    paginate$nrow <<- svalue(h$obj)
                    updateWidget()
                }
            )
            addSpace(pageGp, 10L)

            pageLbl <<- glabel("", container = pageGp)
            font(pageLbl) <<- list(size = 8)

            btnPrev <<- gbutton("", container = pageGp,
                handler = function(h, ...) {
                    if (paginate$row == 1L) return()
                    paginate$row <<- paginate$row - paginate$nrow
                    updateWidget()
                }
            )
            btnPrev$set_icon("go-up")

            btnNext <<- gbutton("", container = pageGp,
                handler = function(h, ...) {
                    if (paginate$row + paginate$nrow - 1L >= nrow(GUI$getActiveData())) return()
                    paginate$row <<- paginate$row + paginate$nrow
                    updateWidget()
                }
            )
            btnNext$set_icon("go-down")

            findRowBtn <- gbutton("", container = pageGp,
                handler = function(h, ...) {
                    row_n <- ginput("Find a specific row in the dataset",
                        title = "Find row",
                        icon = "question",
                        parent = GUI$win
                    )
                    if (is.null(row_n) || length(row_n) == 0) return()
                    n <- as.integer(row_n)
                    if (is.na(n)) {
                        gmessage(sprintf("Invalid row number, `%s`", row_n),
                            title = "Invalid row number",
                            icon = "error",
                            parent = GUI$win
                        )
                        return()
                    }
                    if (n < 1L || n > nrow(GUI$getActiveData())) {
                        gmessage(
                            sprintf("Row number should be between %i and %i",
                                1L, nrow(GUI$getActiveData())
                            ),
                            title = "Invalid row number",
                            icon = "error",
                            parent = GUI$win
                        )
                        return()
                    }
                    paginate$row <<- (n %/% paginate$nrow) * paginate$nrow
                    updateWidget()
                    svalue(dfWidget, index = TRUE) <<- n %% paginate$nrow + 1L
                }
            )
            icon <- RGtk2::gtkImage(
                file = system.file("images/icon-search-number.png",
                    package = "iNZight"
                )
            )
            findRowBtn$widget$setImage(icon)
            findRowBtn$widget$image$show()
            tooltip(findRowBtn) <- "Jump to row number"

            updateDfView()
        },
        ## create variable view (invisible)
        createVarView = function() {
            varView <<- gvbox(expand = TRUE)
            varWidget <<- gtable(data.frame(), expand = TRUE)
            varWidget$remove_popup_menu()
            addDropSource(varWidget,
                handler = function(h, ...) {
                    svalue(h$obj)
                }
            )
            add(varView, varWidget, expand = TRUE)

            updateVarView()
        },
        ## change the currently active View
        changeView = function() {
            if (current == "") return()
            if (current == "data") show("variables")
            if (current == "variables") show("data")
        },
        ## set view to data.frame view
        dataView = function() {
            show("data")
        },
        ## set view to list of columns
        listView = function() {
            show("variables")
        }
    )
)
