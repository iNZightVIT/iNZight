iNZImportExampleWin <- setRefClass("iNZImportExampleWin",
    fields = list(
        GUI = "ANY",
        importFileWin = "ANY",
        datasets = "ANY",
        pkgs = "character",
        dsPkg = "ANY",
        dsData = "ANY",
        dsTitle = "ANY"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Load Example Data",
                width = "small",
                height = "small",
                ok = "Load",
                action = .self$load,
                help = "user_guides/file_options/#example",
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) {
                return()
            }
            on.exit(.self$show())
            usingMethods("load")

            ## packages that have data in them:
            pkgsL <- list(
                "iNZight" = "Default",
                "iNZightMR" = "Multiple Response",
                "iNZightTS" = "Time Series",
                "iNZightMaps" = "Maps",
                "survey" = "Survey",
                "FutureLearnData" = "FutureLearn"
            )

            # TODO: modify how datasets are searched for and stored
            # to easily include from module libraries

            ## modules that have data in them:
            if (!is.null(gui$addonModuleDir) && dir.exists(gui$addonModuleDir)) {
                mods <- list.dirs(gui$addonModuleDir, recursive = FALSE)
                lapply(mods, function(mod) {
                    lib <- file.path(mod, "lib")
                    if (!dir.exists(lib)) {
                        return(NULL)
                    }
                    pkgs <- list.files(lib)
                })
            }

            pkgs <<- unlist(pkgsL) ## becomes a named vector

            pkgs <<- pkgs[names(pkgs) %in% rownames(installed.packages())]

            tbl <- glayout(homogeneous = TRUE)
            ii <- 1L

            ## select box populated with packages (defined above)
            lbl <- glabel("Module (package) :")
            dsPkg <<- gcombobox(pkgs, selected = 1)
            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- dsPkg
            ii <- ii + 1L

            ## select box populated with datasets in chosen package
            lbl <- glabel("Dataset :")
            dsData <<- gcombobox("", selected = 0)
            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- dsData
            ii <- ii + 1L

            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- glabel("Title :")
            dsTitle <<- glabel("")
            tbl[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- dsTitle

            add_body(tbl)

            setDataMenu()

            ## Change Handlers:
            addHandlerChanged(
                dsPkg,
                function(h, ...) {
                    ## set the dataset menu
                    setDataMenu()
                }
            )
            addHandlerChanged(
                dsData,
                function(h, ...) {
                    ttl <- datasets[svalue(dsData, index = TRUE), "Title"]
                    if (ttl == "") {
                        ttl <- svalue(dsData)
                    }
                    svalue(dsTitle) <<- ttl
                }
            )

            invisible(.self)
        }, # initialize
        setDataMenu = function() {
            svalue(dsTitle) <<- ""
            pkgname <- names(pkgs)[svalue(dsPkg, index = TRUE)]
            ds <- data(package = pkgname)$results
            # filter out non-data.frame ones
            dfs <- sapply(
                ds[, "Item"],
                function(x) {
                    d <- x
                    if (pkgname == "survey" && grepl("\\(.+\\)", x)) {
                        d <- gsub(" \\(.+\\)", "", x)
                        x <- gsub("\\)", "", gsub(".+\\(", "", x))
                        x <- gsub(" \\(.+", "", x)
                    }
                    check <- sprintf("data(%s, package = '%s')", x, pkgname)
                    eval(parse(text = check))
                    eval(parse(text = sprintf("is.data.frame(%s)", d)))
                }
            )
            datasets <<- ds[dfs, , drop = FALSE]
            dsData$set_items(datasets[, "Item"])
        },
        load = function() {
            ## Set the data - will need to 'load' it into an evironment, then
            ## reassign it:
            ind <- svalue(dsData, index = TRUE)
            dataName <- dname <- datasets[ind, "Item"]
            pkgname <- names(pkgs)[svalue(dsPkg, index = TRUE)]

            if (pkgname == "survey") {
                if (grepl("\\(.+\\)", dname)) {
                    dataName <- gsub("\\)", "", gsub(".+\\(", "", dataName))
                    dname <- gsub(" \\(.+", "", dname)
                }
            }

            tmp.env <- new.env()
            data(
                list = dataName,
                package = pkgname,
                envir = tmp.env
            )

            ## Set the name to the title (or Item if title missing)
            data <- tmp.env[[dname]]
            GUI$rhistory$add(
                sprintf(
                    "## Load example data set\ndata(%s, package = '%s')",
                    dname,
                    pkgname
                ),
                keep = TRUE
            )

            attr(data, "name") <- sprintf("%s_ex", dname)
            attr(data, "code") <- sprintf(".dataset <- %s", dname)

            GUI$setDocument(iNZDocument$new(data = data, preferences = GUI$preferences), reset = TRUE)

            ## clean up
            rm("tmp.env")
            close()
        }
    ) # methods list
) # setRefClass
