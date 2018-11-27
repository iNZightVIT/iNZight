iNZMenuBarWidget <- setRefClass(
    "iNZMenuBarWidget",
    fields = list(
        GUI = "ANY", container = "ANY", disposeR = "logical",
        menubar = "ANY",
        plotmenu = "ANY"
    ),
    methods = list(
        initialize = function(gui, container, disposeR) {
            initFields(GUI = gui, container = container, disposeR = disposeR)
        
            ## this is trickier, because it depends on a bunch of things 
            plotmenu <<- placeholder("Plot")
            menubar <<- gmenu(list(), container = container)

            defaultMenu()
        },
        hasData = function() {
            !all(dim(GUI$getActiveData()) == 1)
        },
        placeholder = function(name) {
            x <- gaction(name)
            enabled(x) <- FALSE
            x
        },
        defaultMenu = function() {
            setMenu(
                File = FileMenu(), 
                Dataset = DataMenu(),
                Variables = VariablesMenu(),
                Plot = PlotMenu(),
                Advanced = AdvancedMenu(),
                Help = HelpMenu()
            )
        },
        setMenu = function(...) {
            svalue(menubar) <<- list(...)
        },
        updateMenu = function(what, with) {
            svalue(menubar)[[what]] <<- with
        },
        FileMenu = function() {
            list(
                load = 
                    gaction("Load ...", 
                        icon = "symbol_diamond",
                        handler = function(h, ...) iNZLoadSaveWin$new(GUI, action = "load")),
                save = 
                    gaction("Save ...", 
                        icon = "save",
                        handler = function(h, ...) iNZLoadSaveWin$new(GUI, action = "save")),
                gseparator(),
                import = 
                    gaction("Import data ...",
                        icon = "symbol_diamond",
                        tooltip = "Import a new dataset",
                        handler = function(h, ...) iNZImportWinBeta$new(GUI)),
                export = 
                    gaction("Export data ...", 
                        icon = "symbol_diamond",
                        handler = function(h, ...) iNZSaveWin$new(GUI, type = "data", data = GUI$getActiveData())),
                gseparator(),
                example = 
                    gaction("Example data ...", 
                        icon = "symbol_diamond",
                        tooltip = "Load an example dataset",
                        handler = function(h, ...) iNZImportExampleWin$new(GUI)),
                gseparator(),
                preferences = 
                    gaction ("Preferences ...",
                        icon = "symbol_diamond",
                        tooltip = "Customise iNZight",
                        handler = function(h, ...) iNZPrefsWin$new(.self)),
                exit = 
                    gaction("Exit",
                        icon = "symbol_diamond",
                        handler = function(h, ...) if (disposeR) q(save = "no") else dispose(GUI$win))
            )
        },
        DataMenu = function() {
            if (!hasData()) return(placeholder("Dataset"))
            list(
                filter = 
                    gaction("Filter ...",
                        icon = "symbol_diamond",
                        handler = function(h, ...) iNZFilterWin$new(GUI)),
                sort = 
                    gaction("Sort by variable(s) ...",
                        icon = "symbol_diamond",
                        handler = function(h, ...) iNZSortbyDataWin$new(GUI)),
                aggregate = 
                    gaction("Aggregate ...",
                        icon = "symbol_diamond",
                        handler = function(h, ...) iNZAgraDataWin$new(GUI)),
                stack = 
                    gaction("Stack ...",
                        icon = "symbol_diamond",
                        handler = function(h, ...) iNZstackVarWin$new(GUI)),
                gseparator(),
                rename = 
                    gaction("Rename ...",
                        icon = "symbol_diamond",
                        handler = function(h, ...) iNZrenameDataWin$new(GUI)),
                restore = 
                    gaction("Restore original dataset",
                        icon = "symbol_diamond",
                        handler = function(h, ...) GUI$restoreDataset()),
                delete = 
                    gaction("Delete current dataset",
                        icon = "symbol_diamond",
                        handler = function(h, ...) GUI$deleteDataset()),
                gseparator(),
                surveydesign = 
                    gaction("Specify survey design [beta] ...",
                        icon = "symbol_diamond",
                        handler = function(h, ...) iNZSurveyDesign$new(GUI)),
                removedesign = 
                    gaction("Remove design",
                        icon = "symbol_diamond",
                        handler = function(h, ...) GUI$removeDesign()),
                gseparator(),
                expandtable = 
                    gaction("Expand table",
                        icon = "symbol_diamond",
                        handler = function(h, ...) iNZexpandTblWin$new(GUI))
            )
        },
        VariablesMenu = function() {
            if (!hasData()) return(placeholder("Variables"))
            list(
                cont2cat = 
                    gaction("Convert to categorical ...",
                        icon = "symbol_diamond",
                        tooltip = "Convert a variable to a categorical type",
                        handler = function(h, ...) iNZconToCatWin$new(GUI)),
                "Categorical Variables" = list(
                    reorder = 
                        gaction("Reorder levels ...",
                            icon = "symbol_diamond",
                            tooltip = "Reorder the levels of a categorical variable",
                            handler = function(h, ...) iNZreorderWin$new(GUI)),
                    collapse = 
                        gaction("Collapse levels ...",
                            icon = "symbol_diamond",
                            tooltip = "Collapse two or more levels into one",
                            handler = function(h, ...) iNZcllpsWin$new(GUI)),
                    rename = 
                        gaction("Rename levels ...",
                            icon = "symbol_diamond",
                            tooltip = "Rename a categorical variable's levels",
                            handler = function(h, ...) iNZrenameWin$new(GUI)),
                    combine = 
                        gaction("Combine categorical variables ...",
                            icon = "symbol_diamond",
                            tooltip = "Combine two or more categorical variables",
                            handler = function(h, ...) iNZcmbCatWin$new(GUI))
                    ),
                "Numeric Variables" = list(
                    transform = 
                        gaction("Transform ...",
                            icon = "symbol_diamond",
                            tooltip = "Transform a variable using a function",
                            handler = function(h, ...) iNZtrnsWin$new(GUI)),
                    standardise = 
                        gaction("Standardise ...",
                            icon = "symbol_diamond",
                            tooltip = "Standardise a numeric variable",
                            handler = function(h, ...) iNZstdVarWin$new(GUI)),
                    class = 
                        gaction("Form class intervals ...",
                            icon = "symbol_diamond",
                            tooltip = "",
                            handler = function(h, ...) iNZfrmIntWin$new(GUI)),
                    rank = 
                        gaction("Rank numeric variables ...",
                            icon = "symbol_diamond",
                            tooltip = "",
                            handler = function(h, ...) iNZrankNumWin$new(GUI)),
                    cat = 
                        gaction("Convert to categorical (multiple) ...",
                            icon = "symbol_diamond",
                            tooltip = "",
                            handler = function(h, ...) iNZctocatmulWin$new(GUI))
                    ),
                rename =
                    gaction("Rename variables ..."),
                create =
                    gaction("Create new variables ..."),
                miss2cat =
                    gaction("Missing to categorical ..."),
                reshape = 
                    gaction("Reshape dataset ..."),
                delete = 
                    gaction("Delete variables ...")
            )
        },
        PlotMenu = function() {
            if (!hasData()) return(placeholder("Plot"))
            plotmenu
        },
        setPlotMenu = function(menu) {
            plotmenu <<- menu
            updateMenu("Plot", PlotMenu())
        },
        AdvancedMenu = function() {
            if (!hasData()) return(placeholder("Advanced"))
            list(
                quickexplore = gaction("Quick Explore")
            )
        },
        HelpMenu = function() {
            list(
                about = gaction("About iNZight")
            )
        }
    )
)
