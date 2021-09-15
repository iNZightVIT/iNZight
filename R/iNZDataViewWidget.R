iNZDataViewWidget <- setRefClass(
    "iNZDataViewWidget",
    fields = list(
        GUI = "ANY", ## the iNZight GUI object
        widget = "ANY",
        dataGp = "ANY", ## group containing the 2 different view groups
        current = "character",
        dfView = "ANY", ## group that contains data view
        varView = "ANY", ## group that contains variable view
        data = "ANY",
        paginate = "list",
        columns = "character",
        ## max size before dataview gets deactived
        dataThreshold = "numeric",
        varWidget = "ANY",
        searchBox = "ANY",
        searchGp = "ANY"
    ),
    methods = list(
        initialize = function(gui, dataThreshold) {
            initFields(
                GUI = gui,
                dfView = NULL,
                varView = NULL,
                current = "",
                dataThreshold = dataThreshold,
                paginate = list(cols = 1:50, rows = 1:20)
            )

            widget <<- gvbox(expand = TRUE)

            add(widget, init_search())

            dataGp <<- gvbox(expand = TRUE, container = widget)
            set_data()

            createDfView()
            createVarView()

            show("data")
        },
        show = function(what = c("data", "variables")) {
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

            switch(what,
                "data" = show_data(),
                "variables" = show_variables()
            )
            current <<- what
        },
        show_data = function() {
            add(dataGp, dfView, expand = TRUE)
        },
        show_variables = function() {
            add(dataGp, varView, expand = TRUE)
        },
        init_search = function () {
            searchGp <<- ggroup()
            addSpace(searchGp, 5)

            searchtimer <- NULL
            searchBox <<- gedit(
                handler = function(h, ...) {
                    matches <- grep(svalue(h$obj), names(GUI$getActiveData()),
                        ignore.case = TRUE)
                    if (length(matches) == 0)
                        matches <- NA_character_
                    else
                        matches <- names(GUI$getActiveData())[matches]

                    paginate <<- list(cols = 1:50, rows = 1:20)
                    columns <<- matches
                    set_data()

                    # cn <- varWidget$get_names()
                    # varWidget$set_items(matches)
                    # varWidget$set_names(cn)
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
                rows = paginate$rows[paginate$rows <= nr],
                cols = paginate$cols[paginate$cols <= nc]
            )
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
            createDfView()
            createVarView()
            if (current == "") return()
            showing <- current
            current <<- ""
            show(showing)
        },
        ## only update the variable view
        updateVarView = function() {
            createVarView()
        },
        ## only update the data.frame view
        updateDfView = function() {
            createDfView()
        },
        ## create the data.frame view (invisible)
        createDfView = function() {
            dfView <<- ggroup(container = dataGp, expand = TRUE)

            ## This will be paginated, at some stage:
            dfWidget <- gdf(data, expand = TRUE)
            ## dfWidget$remove_popup_menu() ## - called by $add_dnd_columns()
            dfWidget$add_dnd_columns()
            add(dfView, dfWidget, expand = TRUE)
            ## if the data.frame gets edited, update the iNZDocument
            addHandlerChanged(
                dfWidget,
                handler = function(h, ...) {
                    print("DATA EDITED - NEED TO UPDATE CORRECT PART OF DATA")
                    # X1 <- dfWidget[]
                    # if(class(X1) != "data.frame")
                    #     newData <- data.frame(X1)
                    # GUI$getActiveDoc()$getModel()$updateData(X1)
                }
            )
        },
        ## create variable view (invisible)
        createVarView = function() {
            varView <<- gvbox(container = dataGp, expand = TRUE)

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

            varsDf <- data.frame(
                Variable = vnames,
                Type = vtypes
            )

            varWidget <<- gtable(varsDf, expand = TRUE)

            varWidget$remove_popup_menu()
            addDropSource(varWidget,
                handler = function(h, ...) {
                    svalue(h$obj)
                }
            )
            add(varView, varWidget, expand = TRUE)
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
            # visible(varView) <<- FALSE
            # visible(dfView) <<- TRUE
        },
        ## set view to list of columns
        listView = function() {
            show("variables")
            # visible(dfView) <<- FALSE
            # visible(varView) <<- TRUE
        }
    )
)
