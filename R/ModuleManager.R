#' Module Manager for iNZight add-ons
#'
#' @field GUI an `iNZGUI` object
#' @field win a `gwindow` object
#' @field m_dir path to the user's module directory, grabbed from `GUI`
ModuleManager <- setRefClass(
    "ModuleManager",
    contains = "iNZWindow",
    fields = list(
        GUI = "ANY",
        win = "ANY",
        m_dir = "character",
        confirm = "logical",
        modules = "data.frame",
        module_table = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            "Initialize the module manager window"
            ok <- callSuper(gui,
                title = "Module Manager",
                width = "large",
                height = "large",
                ok = "Done",
                action = .self$close,
                cancel = NULL,
                # help = "user_guides/file_options/#import",
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods()

            initFields(
                m_dir = gui$addonModuleDir,
                confirm = as.logical(Sys.getenv("INZIGHT_CONFIRM_DIALOGS", TRUE)),
                modules = data.frame()
            )

            if (!file.exists(m_dir)) create_module_directory(m_dir)

            ## --- show user where modules are installed:
            ## TODO: add button to specify new location / link to preferences
            info_tbl <- glayout()

            lbl <- glabel("Addon module directory:")
            font(lbl) <- list(weight = "bold")
            info_tbl[1L, 1L, anchor = c(1, 0)] <- lbl

            m_dir_lbl <- glabel(m_dir)
            info_tbl[1L, 2L, expand = TRUE] <- m_dir_lbl

            add_body(info_tbl)

            #
            ## --- list installed modules
            refresh_modules()
            module_table <<- gdf(modules)
            module_table$remove_popup_menu()
            module_table$hide_row_names(TRUE)

            add_body(module_table, expand = TRUE)

        },
        create_module_directory = function(dir) {
            if (is.null(dir) || trimws(dir) == "")
                stop("Please specify module directory. See ?iNZight")
            if (!interactive())
                stop(sprintf("Module directory does not exist. Please create %s.", dir))

            conf <- gconfirm(
                paste0(
                    "You need to create a modules directory to install modules into.\n\n",
                    "Would you like to create the following directory?\n\n",
                    dir
                ),
                title = "Create module directory?",
                icon = "question",
                parent = GUI$win
            )
            if (!conf) return()

            if (dir.create(dir)) return()
            gmessage(
                paste0(
                    "Please create the following directory manually:\n\n",
                    dir
                ),
                title = "Error creating directory",
                icon = "error",
                parent = GUI$win
            )
        },
        refresh_modules = function() {
            "Update list of modules installed"
            modules <<- data.frame(
                Select = FALSE,
                Name = "Module Name",
                Description = "Here is a description",
                Subscribed = "latest",
                Version = "1.0.0",
                `Update Available` = "TRUE"
            )
        }
    )
)
