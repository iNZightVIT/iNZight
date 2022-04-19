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
        g_mod_info = "ANY"
        # mod_info_title = "ANY",
        # mod_info_author = "ANY",
        # mod_info_version = "ANY",
        # mod_info_subscribed = "ANY",
        # mod_info_description = "ANY"
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

            available_modules <<- lapply(names(available_modules),
                function(z) c(list(name = z), available_modules[[z]])
            )
            names(available_modules) <<-
                sapply(available_modules, function(x) x$name)

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
            module_table <<- gtable(data.frame(Modules = "None"), container = g_mods)
            refresh_modules()

            # TODO: remove dropdown from header

            addHandlerSelectionChanged(module_table,
                function(h, ...) update_info_panel())

            g_mod_info <<- gvbox()
            g_mod_info$set_borderwidth(5L)
            size(g_mod_info) <<- c(700, -1)

            add(g_mods, g_mod_info)
            update_info_panel()

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
                info$subscribed <- scan(file.path(dir, "VERSION"), what = character())

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

            module_table$set_items(
                make_modules_df()
            )
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
                        Latest = amod$version,
                        Installed = ifelse(is.null(imod), "", as.character(imod$version))
                    )
                }
            )
            do.call(rbind, mdf)
        },
        update_info_panel = function() {
            # delete all contents
            sapply(g_mod_info$children,
                function(x) delete(g_mod_info, x)
            )
            g_info <- gvbox(expand = TRUE, container = g_mod_info)

            modi <- module_table$get_selected()
            if (length(modi) == 0L) {
                # hide everything
                lbl <- glabel("Select a module from the list")
                add(g_info, lbl, anchor = c(-1, 0))
                return()
            }

            # show everything
            mod <- names(available_modules)[modi]
            amod <- available_modules[[mod]]
            imod <- installed_modules[[mod]]

            ## -- module title
            mod_info_title <- glabel(amod$title)
            font(mod_info_title) <- list(size = 12, weight = "bold")
            add(g_info, mod_info_title, anchor = c(-1, 0), fill = TRUE)

            addSpace(g_info, 10)

            ## -- module information
            mod_info_tbl <- glayout(container = g_info, expand = TRUE)
            ii <- 1L

            mod_info_author <- glabel(amod$author)
            mod_info_tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- "Author(s): "
            mod_info_tbl[ii, 2:3, anchor = c(-1, 0), expand = TRUE, fill = TRUE] <- mod_info_author
            ii <- ii + 1L

            mod_info_version <- glabel(amod$version)
            mod_info_tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- "Latest version: "
            mod_info_tbl[ii, 2:3, anchor = c(-1, 0), expand = TRUE, fill = TRUE] <- mod_info_version

            mod_info_description <- gtext(amod$description,
                width = 500, height = 50)
            RGtk2::gtkTextViewSetLeftMargin(mod_info_description$widget, 0)
            enabled(mod_info_description) <- FALSE
            mod_info_tbl[ii, 1L, anchor = c(1, 1), expand = TRUE] <- "Description: "
            mod_info_tbl[ii, 2:3, anchor = c(-1, 0), expand = TRUE, fill = TRUE] <- mod_info_description
            ii <- ii + 1L

            ## Version selection box
            mod_versions <- c(
                "None",
                "Stable",
                if (!is.null(amod$development) && amod$development != "")
                    "Development" else NULL,
                amod$versions
            )
            mod_version <- gcombobox(mod_versions,
                selected = if (is.null(imod)) 1L else which(mod_versions == imod$subscribed),
                handler = function(h, ...)
                    install_module(amod, svalue(h$obj))
            )
            mod_info_tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- "Installed version: "
            mod_info_tbl[ii, 2L, fill = TRUE] <- mod_version

            if (!is.null(imod)) {
                del_btn <- gbutton("Uninstall",
                    handler = function(h, ...) {
                        uninstall_module(amod)
                    }
                )
                mod_info_tbl[ii, 3L] <- del_btn
            }

            ii <- ii + 1L

            # if (!is.null(amod$versions) && !is.null(imod)) {
            #     mod_info_subscribed$set_items(imod$versions)
            #     mod_info_subscribed$set_value(imod$subscribed)
            #     visible(mod_info_subscribed) <<- TRUE
            # } else {
            #     visible(mod_info_subscribed) <<- FALSE
            # }






            # print(svalue(module_table))
        },
        install_module = function(mod, ref, confirm = TRUE) {
            if (ref == "None") return()
            # downloads the named module@ref

            str <- sprintf("%s/archive/refs/%s/%s.zip",
                gsub("\\.git$", "", mod$url),
                ifelse(ref %in% c("Stable", "Development"),
                    "heads",
                    "tags"
                ),
                switch(ref,
                    "Stable" = mod$stable,
                    "Development" = mod$development,
                    ref
                )
            )

            if (confirm) {
                c <- gconfirm(
                    sprintf("You are about to install %s (%s).",
                        mod$title, ref
                    ),
                    parent = GUI$win
                )
                if (!c) return(NULL)
            }
            message("Installing ", str)

            mod_path <- file.path(m_dir, mod$name)
            if (dir.exists(mod_path)) {
                if (confirm) {
                    c <- gconfirm("This will remove the previous version. Continue?",
                        parent = GUI$win
                    )
                    if (!c) return(NULL)
                }
            }

            tf <- tempfile(fileext = ".zip")
            message("Downloading module ...")
            utils::download.file(str, tf, quiet = TRUE)

            zdir <- basename(utils::unzip(tf, list = TRUE)$Name[1])
            message("Exacting ...")
            utils::unzip(tf, exdir = m_dir)
            message("Moving items into place ...")

            if (dir.exists(mod_path)) unlink(mod_path, TRUE, TRUE)
            file.rename(file.path(m_dir, zdir), mod_path)

            # writes a file VERSION indicating which version is being tracked
            cat(ref, file = file.path(mod_path, "VERSION"))

            mod_lib <- file.path(mod_path, "lib")
            dir.create(mod_lib)
            deps <- desc::desc_get_deps(file = file.path(mod_path, "DESCRIPTION"))
            deps <- deps[!deps %in% rownames(installed.packages())]
            gh_deps <- desc::desc_get_field(
                "Github",
                file = file.path(mod_path, "DESCRIPTION")
            )
            remotes::install_github(gh_deps,
                lib = mod_lib,
                upgrade = "never"
            )
            utils::install.packages(deps$package, lib = mod_lib)

            # install dependencies into custom

            message(sprintf("Module %s installed!", mod$name))
            refresh_modules()
            update_info_panel()
        },
        uninstall_module = function(mod, confirm = TRUE) {
            if (confirm) {
                c <- gconfirm(
                    sprintf("Are you sure you want to uninstall %s?", mod$title),
                    parent = GUI$win
                )
                if (!c) return(NULL)
            }
            d <- file.path(m_dir, mod$name)
            if (dir.exists(d)) unlink(d, TRUE, TRUE)

            refresh_modules()
            update_info_panel()
        },
        closeHandler = function(h, ...) {
            if (!is.null(module_table)) delete(g_mods, module_table)
            GUI$load_addons()
            GUI$menuBarWidget$defaultMenu()
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
        helpButton = "ANY",
        loaded_packages = "character"
    ),
    methods = list(
        initialize = function(gui, mod,
            name = mod$info$title %||% "Module",
            embedded = TRUE,
            uses_code_panel = FALSE,
            help = NULL
        ) {
            initFields(GUI = gui, mod = mod)

            ## special loading of menu - this should do some fancy checking, etc..

            print(mod)
            deps <- mod$info$pkgs$package
            message("\nLoading module dependencies ...")
            search_original_pkgs <- search()

            for (pkg in deps) {
                cat("+", pkg, "...")
                mod_lib <- file.path(mod$mod_dir, "lib")
                in_local_lib <- dir.exists(mod_lib) && dir.exists(file.path(mod_lib, pkg))
                pkg_lib <- if (in_local_lib) mod_lib else NULL
                loaded <- require(pkg,
                    lib.loc = pkg_lib,
                    character.only = TRUE,
                    quietly = TRUE
                )
                if (loaded) {
                    pv <- packageVersion(pkg, lib.loc = pkg_lib)
                    cat("", as.character(pv), "\n")
                } else cat(" failed\n")
            }
            search_final_pkgs <- search()
            loaded_packages <<- search_final_pkgs[!search_final_pkgs %in% search_original_pkgs]

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
        body_space = function(x) addSpace(mainGrp, x),
        close = function() {
            ## run module-specific closure?

            ## clean up search path
            message("Detaching module dependencies ...")
            cleanup <- sapply(loaded_packages, detach, character.only = TRUE)
            rm(cleanup)

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
    iNZModules <- e$iNZModule <- utils::getFromNamespace("iNZModule", "iNZight")

    modRdir <- file.path(dir, "R")
    if (dir.exists(modRdir))
        lapply(
            list.files(modRdir, full.names = TRUE),
            function(x) source(x, local = e)
        )

    # now load all the bits and pieces
    mdir <- file.path(dir, "module")

    if (file.exists(file.path(mdir, "menu.R")))
        source(file.path(mdir, "menu.R"), local = e)

    if (file.exists(file.path(mdir, "main.R")))
        source(file.path(mdir, "main.R"), local = e)

    e$mod_dir <- dir

    class(e) <- "inzmodule"
    e
}

## also need a 'close module' method which can remove the loaded libraries ...


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
