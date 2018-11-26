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
                    gaction("Restore original dataset"),
                delete = 
                    gaction("Delete current dataset"),
                gseparator(),
                surveydesign = 
                    gaction("Specify survey design [beta] ..."),
                removedesign = 
                    gaction("Remove design"),
                gseparator(),
                expandtable = 
                    gaction("Expand table")
            )
        },
        VariablesMenu = function() {
            if (!hasData()) return(placeholder("Variables"))
            list(
                transform = gaction("Transform")
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
