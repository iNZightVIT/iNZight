#' Module Manager for iNZight add-ons
#'
#' @field GUI an `iNZGUI` object
#' @field win a `gwindow` object
#' @field m_dir path to the user's module directory, grabbed from `GUI`
NewModuleManager <- setRefClass(
    "NewModuleManager",
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

## New module class
#' iNZight Module
#'
#' Provides a basic module for extending new ones
#'
#' @title iNZight Module class for addons
#'
#' @author Tom Elliott
#'
#' @export iNZModule
#' @exportClass iNZModule
iNZModule <- setRefClass(
    "iNZModule",
    fields = list(
        GUI = "ANY",
        mod = "ANY",
        modwin = "ANY",
        mainGrp = "ANY",
        homeButton = "ANY"
    ),
    methods = list(
        initialize = function(gui, mod,
            name = mod$info$title %||% "Module",
            embedded = TRUE,
            uses_code_panel = FALSE
        ) {
            initFields(GUI = gui, mod = mod)

            # if (embedded) {}
            modwin <<- GUI$initializeModuleWindow(.self,
                title = name,
                scroll = TRUE
            )
            mainGrp <<- modwin$body
            mainGrp$set_borderwidth(5L)

            homeButton <<- gbutton("Home",
                handler = function(h, ...) close()
            )

            GUI$plotToolbar$update(NULL)

            # add(modwin$footer, helpButton, expand = TRUE, fill = TRUE)
            add(modwin$footer, homeButton, expand = TRUE, fill = TRUE)

            if (GUI$preferences$dev.features && GUI$preferences$show.code)
                visible(GUI$code_panel$panel) <<- uses_code_panel

        },
        get_data = function() {
            GUI$getActiveData()
        },
        install_dependencies = function(pkgs, optional, github) {
            # add the iNZight repository:
            dkr <- "https://r.docker.stat.auckland.ac.nz"
            repo <- options()$repos
            if (!dkr %in% repo) repo <- c(dkr, repo)

            if (!missing(pkgs)) {
                pkgs <- pkgs[!pkgs %in% rownames(utils::installed.packages())]
                if (length(pkgs) > 0) {
                    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",
                        xlab = "", ylab = "")
                    text(0, 0, "Installing dependencies, please wait ...")

                    utils::install.packages(pkgs, quiet = TRUE, repos = repo, dependencies = TRUE)
                }
            }

            if (!missing(optional)) {
                optional <- optional[!optional %in% rownames(utils::installed.packages())]
                if (length(optional) > 0) {
                    tryCatch(
                        utils::install.packages(optional, quiet = TRUE, repos = repo, dependencies = TRUE),
                        finally = {}
                    )
                }
            }

            if (!missing(github)) {
                remotes::install_github(github, repos = repo)
            }

            plot(0, 0, type = "n", bty = "n",
                xaxt = "n", yaxt = "n",
                xlab = "", ylab = "")
        },
        add_body = function(x, ...) {
            add(mainGrp, x)
        },
        close = function() {
            ## run module-specific closure?

            ## delete the module window
            GUI$close_module()
            ## display the default view (data, variable, etc.)
            GUI$plotToolbar$restore()
            GUI$menuBarWidget$defaultMenu()
            GUI$updatePlot()
            invisible(TRUE)
        }
    )
)


new_module <- function(info) {
    menu <- list(Modules = list(info$title))
    environment()
}

load_module <- function(dir) {
    # load a module into a nice structure ...

    # parse description file
    mod_desc <- file.path(dir, "DESCRIPTION")
    info <- list(
        title = desc::desc_get_field("Title", file = mod_desc),
        description = desc::desc_get_field("Description", file = mod_desc),
        author = desc::desc_get_field("Author", file = mod_desc),
        version = desc::desc_get_version(file = mod_desc),
        pkgs = desc::desc_get_deps(file = mod_desc),
        github = desc::desc_get_field("Github", file = mod_desc)
    )
    e <- new_module(info)

    modRdir <- file.path(dir, "R")
    if (dir.exists(modRdir))
        lapply(
            list.files(modRdir, full.names = TRUE),
            function(x) source(x, local = e)
        )

    # now load all the bits and pieces
    mdir <- file.path(dir, "module")

    ## special loading of menu - this should do some fancy checking, etc..
    if (file.exists(file.path(mdir, "menu.R")))
        source(file.path(mdir, "menu.R"), local = e)

    if (file.exists(file.path(mdir, "main.R")))
        source(file.path(mdir, "main.R"), local = e)

    class(e) <- "inzmodule"
    e
}

#' @export
print.inzmodule <- function(x, ...) {
    cli::cli_h1("{x$info$title}")
    cli::cli_text("Author: {.emph {x$info$author}}")
    cli::cli_text("Version {x$info$version}")

    cli::cli_h3("Description")
    cli::cli_text("{x$info$description}")
}

## helpers for modules
item <- function(title, action) {
    structure(list(title = title, action = action),
        class = "inzmenuitem"
    )
}

run_module <- function(ui, mod) {
    n <- ls(envir = mod)
    moduleName <- if (!is.null(mod$module_name)) mod$module_name else {
        cl <- sapply(n, function(x) class(mod[[x]]))
        n[which(cl == "refObjectGenerator")[1]]
    }
    mod[[moduleName]]$new(ui, mod = mod)
}
