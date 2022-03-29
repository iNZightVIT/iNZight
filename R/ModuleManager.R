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
        installed_modules = "list",
        available_modules = "list",
        g_mods = "ANY",
        module_table = "ANY",
        mod_info_title = "ANY",
        mod_info_author = "ANY",
        mod_info_version = "ANY",
        mod_info_subscribed = "ANY",
        mod_info_description = "ANY"
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

            initFields(
                m_dir = gui$addonModuleDir,
                confirm = as.logical(Sys.getenv("INZIGHT_CONFIRM_DIALOGS", TRUE)),
                installed_modules = list(),
                available_modules = yaml::read_yaml(
                    "https://raw.githubusercontent.com/iNZightVIT/addons/feature/modular/modules.yml",
                )
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

            g_mods <<- ggroup()
            add_body(g_mods, expand = TRUE)

            #
            ## --- list installed modules
            refresh_modules()
            module_table <<- gtable(make_modules_df(), container = g_mods)

            # TODO: remove dropdown from header

            addHandlerSelectionChanged(module_table,
                function(h, ...) update_info_panel())

            g_mod_info <- gvbox()
            g_mod_info$set_borderwidth(5L)
            # size(g_mod_info) <- c(600, -1)
            add(g_mods, g_mod_info)

            mod_info_title <<- glabel("Select a module",
                container = g_mod_info,
                fill = TRUE,
                anchor = c(-1, 0)
            )

            addSpace(g_mod_info, 10)

            mod_info_tbl <- glayout(container = g_mod_info, expand = TRUE)
            ii <- 1L

            mod_info_author <<- glabel("")
            mod_info_tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- glabel("Author: ")
            mod_info_tbl[ii, 2:3, anchor = c(-1, 0), expand = TRUE, fill = TRUE] <- mod_info_author
            ii <- ii + 1L

            mod_info_version <<- glabel("")
            mod_info_tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- glabel("Latest version: ")
            mod_info_tbl[ii, 2L, anchor = c(-1, 0), expand = TRUE, fill = TRUE] <- mod_info_version

            mod_info_subscribed <<- gcombobox("")
            visible(mod_info_subscribed) <<- FALSE
            mod_info_tbl[ii, 3L, fill = TRUE] <- mod_info_subscribed
            ii <- ii + 1L

            mod_info_description <<- gtext("", width = 500, height = 50)
            RGtk2::gtkTextViewSetLeftMargin(mod_info_description$widget, 0)
            enabled(mod_info_description) <<- FALSE
            mod_info_tbl[ii, 1L, anchor = c(1, 1), expand = TRUE] <- glabel("Description: ")
            mod_info_tbl[ii, 2:3, anchor = c(-1, 0), expand = TRUE, fill = TRUE] <- mod_info_description
            ii <- ii + 1L


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
        module_load = function(dir) {
            mod_desc <- file.path(dir, "DESCRIPTION")
            info <- list(
                title = desc::desc_get_field("Title", file = mod_desc),
                description = desc::desc_get_field("Description", file = mod_desc),
                author = desc::desc_get_field("Author", file = mod_desc),
                version = desc::desc_get_version(file = mod_desc),
                pkgs = desc::desc_get_deps(file = mod_desc),
                github = desc::desc_get_field("Github", file = mod_desc),
                subscribed = "stable",
                update_available = ""
            )
            if (file.exists(file.path(dir, "VERSION")))
                info$subscribed <- scan(file.path(dir, "VERSION"))

            si <- which(sapply(available_modules, function(x) x$title) == info$title)
            if (si > 0) {
                amod <- available_modules[[si]]
                if (info$subscribed == "stable") {
                    si <- which(sapply(available_modules, function(x) x$title) == info$title)
                    info$update_available <- ifelse(
                        info$version < numeric_version(amod$latest),
                        available_modules[[si]]$latest,
                        ""
                    )
                }
                info$versions <- c("stable",
                    if (amod$development == "") NULL else amod$development,
                    amod$versions
                )
            }
            info
        },
        refresh_modules = function() {
            "Update list of modules installed"
            mdirs <- list.dirs(m_dir, recursive = FALSE)
            installed_modules <<- lapply(mdirs, .self$module_load)
            names(installed_modules) <<- basename(mdirs)
        },
        make_modules_df = function() {
            mdf <- lapply(names(available_modules),
                function(mod) {
                    amod <- available_modules[[mod]]
                    imod <- installed_modules[[mod]]
                    data.frame(
                        # Select = FALSE,
                        Name = amod$title,
                        # Description = mod$description,
                        # Subscribed = mod$subscribed,
                        Latest = ifelse(is.null(imod), "", as.character(imod$version)),
                        Installed = amod$version
                    )
                }
            )
            do.call(rbind, mdf)
        },
        update_info_panel = function() {
            modi <- module_table$get_selected()
            if (modi == 0) {
                # hide everything
            } else {
                # show everything
                mod <- names(available_modules)[modi]
                amod <- available_modules[[mod]]
                imod <- installed_modules[[mod]]
                svalue(mod_info_title) <<- amod$title
                font(mod_info_title) <<- list(size = 12, weight = "bold")

                svalue(mod_info_author) <<- amod$author
                svalue(mod_info_version) <<- amod$version
                if (!is.null(amod$versions) && !is.null(imod)) {
                    mod_info_subscribed$set_items(imod$versions)
                    mod_info_subscribed$set_value(imod$subscribed)
                    visible(mod_info_subscribed) <<- TRUE
                } else {
                    visible(mod_info_subscribed) <<- FALSE
                }
                svalue(mod_info_description) <<- amod$description
            }

            # print(svalue(module_table))
        },
        closeHandler = function(h, ...) {
            if (!is.null(module_table)) delete(g_mods, module_table)
            return(TRUE)
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
        homeButton = "ANY",
        helpButton = "ANY"
    ),
    methods = list(
        initialize = function(gui, mod,
            name = mod$info$title %||% "Module",
            embedded = TRUE,
            uses_code_panel = FALSE,
            help = NULL
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

            if (!is.null(help)) {
                helpButton <<- gbutton("Help",
                    handler = function(h, ...) browseURL(help)
                )
                add(modwin$footer, helpButton, expand = TRUE, fill = TRUE)
            }

            add(modwin$footer, homeButton, expand = TRUE, fill = TRUE)

            GUI$plotToolbar$update(NULL)

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

convert_menu_items <- function(item, ...) UseMethod("convert_menu_items")

#' @export
convert_menu_items.default <- function(item, ...) item

#' @export
convert_menu_items.list <- function(item, ...) lapply(item, convert_menu_items, ...)

#' @export
convert_menu_items.inzmenuitem <- function(item, gui, mod) {
    gaction(
        item$title,
        handler = function(h, ...) item$action(gui, mod)
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
