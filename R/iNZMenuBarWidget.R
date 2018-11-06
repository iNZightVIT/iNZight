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
                load = gaction("Load ...", 
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZLoadSaveWin$new(GUI, action = "load")),
                save = gaction("Save ...", 
                    icon = "save",
                    handler = function(h, ...) iNZLoadSaveWin$new(GUI, action = "save")),
                gseparator(),
                import = gaction("Import data ...",
                    icon = "symbol_diamond",
                    tooltip = "Import a new dataset",
                    handler = function(h, ...) iNZImportWinBeta$new(GUI)),
                export = gaction("Export data ...", 
                    icon = "symbol_diamond",
                    handler = function(h, ...) iNZSaveWin$new(GUI, type = "data", data = GUI$getActiveData())),
                gseparator(),
                example = gaction("Example data ...", 
                    icon = "symbol_diamond",
                    tooltip = "Load an example dataset",
                    handler = function(h, ...) iNZImportExampleWin$new(GUI)),
                gseparator(),
                exit = gaction("Exit",
                    icon = "symbol_diamond",
                    handler = function(h, ...) if (disposeR) q(save = "no") else dispose(GUI$win))
            )
        },
        DataMenu = function() {
            if (!hasData()) return(placeholder("Dataset"))
            list(
                reset = gaction("Reset")
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
