## --------------------------------------------
## The super class for the data modification window
## When a new data modification window is opened,
## a current one is closed if it exists
## List:
## iNZConToCatWin: Convert variables to a categorical type
## iNZTransformWin: transform variables using various functions
## iNZcllpsWin: collapse multiple factor levels into one
## iNZrenameWin: rename factor levels
## iNZreorderWi: reorder factor levels
## iNZcmbCatWin: combine categorical variables
## iNZcrteVarWin: create new variables using an expression
## iNZfrmIntWin: form class intervals for a numeric variable
## iNZrnmVarWin: rename variables. This overwrites the old variable name, i.e. does not create a new variable
## iNZstdVarWin: standardise variables
## iNZdeleteVarWin: delete variables
## iNZmissCatWin: Missing as Cat
## iNZrankNumWin: Rank the numerical variables X (vector, matrix)
## iNZctocatmulWin: Convert multiple variables to categorical type in the same time
## iNZRenameDataWin: Rename the dataset
## -------------------------------------------
iNZDataModWin <- setRefClass(
    "iNZDataModWin",
    fields = list(),
    contains = "iNZWindow",
    methods = list(
        initialize = function(...) {
            ok <- callSuper(...)
            usingMethods(makeNames, checkNames, updateData)
            invisible(ok)
        },
        ## this is used to autogenerate names for variables
        makeNames = function(vars) {
            vnames <- names(GUI$getActiveData(lazy = TRUE))
            iNZightTools::make_names(vars, vnames)
        },
        ## this checks names exist; returns TRUE if everything is OK
        checkNames = function(var) {
            if (any(w <- var %in% names(GUI$getActiveData(lazy = TRUE)))) {
                if (length(var == 0)) {
                    gmessage(
                        "A variable with that name already exists. Please choose another one.",
                        title = "Variable name already exists",
                        icon = "error",
                        parent = GUI$modWin
                    )
                } else {
                    gmessage(
                        paste(sep = "\n",
                            "The follow variable names already exist:",
                            paste(collapse = ", ", var[w]),
                            "Please choose new names."
                        ),
                        title = "Variable names already exist",
                        icon = "error"
                    )
                }
                return(FALSE)
            }
            return(TRUE)
        },
        updateData = function(newdata) {
            GUI$update_document(newdata)
        }
    )
)

## Convert variables to a categorical type
iNZConToCatWin <- setRefClass(
    "iNZConToCatWin",
    contains = "iNZDataModWin",
    fields = list(
        varData = "ANY", ## data that is dragged into droptarget
        varLbl = "ANY",
        varname = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Convert to categorical",
                width = "small",
                height = "small",
                ok = "Convert",
                action = .self$convert,
                help = "user_guides/variables/#convert1",
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("convert")

            add_heading(
                "Choose a variable from the dropdown box below,",
                "or drag and drop a variable onto it, to create a",
                "categorical version of the chosen variable."
            )

            tbl <- glayout()
            ii <- 1L

            lbl <- glabel("Select numeric variable :")
            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl

            data <- GUI$getActiveData(lazy = TRUE)
            nvars <- iNZightTools::vartypes(data) %in% c("num", "dt")
            numvars <- names(data)[nvars]
            varLbl <<- gcombobox(numvars,
                selected = 0L,
                handler = function(h, ...) {
                    varData <<- svalue(varLbl)
                    svalue(varname) <<- makeNames(paste0(varData, ".cat"))
                }
            )
            tbl[ii, 2L, expand = TRUE] <- varLbl
            ii <- ii + 1L

            lbl <- glabel("Specify name for new variable :")
            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl

            varname <<- gedit("", width = 20)
            tbl[ii, 2L, expand = TRUE] <- varname
            ii <- ii + 1L

            addDropTarget(varLbl,
                handler = function(h, ...) {
                    dropData <- GUI$getActiveDoc()$getData()[h$dropdata][[1L]]
                    if (all(is_cat(dropData))) {
                        gmessage("Already a categorical variable!",
                            parent = GUI$win, icon = 'warning')
                    } else {
                        svalue(varLbl) <<- h$dropdata
                    }
                }
            )

            add_body(tbl)
        },
        convert = function() {
            orgVar <- svalue(varLbl)
            name <- gsub('\\n+', "", svalue(varname), perl = TRUE)
            if (name == "" || !is.character(name))
                gmessage("Please choose a non-empty name for the new variable",
                title = "Invalid variable choice",
                parent = GUI$modWin)
            else if (length(orgVar) == 0L)
                gmessage("Please choose a variable to convert",
                title = "Invalid variable choice",
                parent = GUI$modWin)
            else if (checkNames(name)) {
                .dataset <- GUI$get_data_object(lazy = FALSE)
                newdata <- iNZightTools::convert_to_cat(.dataset, vars = orgVar, names = name)
                updateData(newdata)
                close()
            }
        }
    )
)

## transform variables using various functions
iNZTransformWin <- setRefClass(
    "iNZTransformWin",
    fields = list(
        data = "data.frame",
        varbox = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Transform variables",
                width = "small",
                height = "med",
                help = "user_guides/variables/#transform",
                ok = "Close",
                cancel = NULL,
                action = close,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            initFields(data = GUI$getActiveData(lazy = FALSE))

            ## need to specify the methods that we want to use in
            ## do.call later on

            add_heading(
                "Choose variables from the dropdown",
                "and click a button to apply the transformation.",
                "Repeat for as many variables as needed, then close the window."
            )

            add_heading(
                "Alternatively, drag-and-drop variable names from the data viewer",
                "directly onto a button to apply the transformation."
            )

            add_heading(
                "Note: only numeric variables can be transformed.",
                size = 8, weight = "bold"
            )

            body_space(5L)

            numvars <- names(data)[sapply(data, iNZightTools::is_num)]
            varbox <<- gcombobox(numvars, selected = 0L)
            add_body(varbox)

            body_space(5L)


            ## function names: the X will be converted to the variable name (e.g., log.height, height.squared, etc)
            ##  Display name           new name     function
            transforms <- list(
                "Natural Log (base e)" = c("log.e.X",   "log"),
                "Log (base 10)" = c("log.10.X",  "log10"),
                "Exponential (e^x)" = c("exp.X",     "exp"),
                "Square (X^2)" = c("X.squared", "square"),
                "Square root" = c("root.X",    "sqrt"),
                "Reciprocal (1/X)" = c("recip.X",   "reciprocal")
            )

            tbl <- glayout()
            NCOL <- 2L
            trans_btns <- sapply(
                seq_along(transforms),
                function(i) {
                    row_i <- (i - 1L) %/% NCOL + 1L
                    col_i <- (i - 1L) %% NCOL + 1L
                    btn_i <- gbutton(names(transforms)[i],
                        handler = function(h, ...) transform(transforms[[i]], svalue(varbox))
                    )
                    tbl[row_i, col_i] <- btn_i

                    addDropTarget(btn_i,
                        handler = function(h, ...) transform(transforms[[i]], h$dropdata)
                    )
                }
            )

            add_body(tbl, anchor = c(0, 0))
        },
        ## check whether the data is illegible for transformation
        checkData = function(varData) {
            !any(is_cat(varData))
        },
        transform = function(trans, var) {
            if (!checkData(data[[var]])) {
                gmessage("Not a numeric variable",
                    title = "Error: non-numeric variable",
                    icon = "error",
                    parent = GUI$modWin)
                return()
            }

            vname <- makeNames(gsub("X", var, trans[1L]))
            if (!checkNames(vname)) {
                gmessage("Unable to create new variable",
                    title = "Error creating variable",
                    icon = "error",
                    parent = GUI$modWin)
                return()
            }

            fn <- trans[2L]
            .dataset <- GUI$get_data_object(lazy = FALSE)
            reciprocal <- function(x) 1 / x
            square <- function(x) x^2
            newdata <- iNZightTools::transform_vars(.dataset, vars = var, fn, names = vname)
            updateData(newdata)

            data <<- GUI$getActiveData(lazy = TRUE)
            nvars <- iNZightTools::vartypes(data) %in% c("num", "dt")
            numvars <- names(data)[nvars]
            varbox$set_items(numvars)
        }
    )
)

## collapse multiple factor levels into one
iNZCollapseWin <- setRefClass(
    "iNZCollapseWin",
    fields = list(
        factor_menu = "ANY",
        factor_levels = "ANY",
        new_varname = "ANY",
        new_level = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Collapse Levels",
                width = "small",
                height = "med",
                help = "user_guides/variables/#collapse",
                ok = "Collapse",
                action = .self$collapse,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("collapse")

            add_heading(
                "Choose a variable,",
                "then choose two or more levels to collapse into one."
            )

            lbl4 <- glabel("New variable name: ")
            lbl5 <- glabel("Collapsed level name: ")

            ## choose a factor column from the dataset and display
            ## its level in a gtable
            factorIndices <- iNZightTools::vartypes(GUI$getActiveData(lazy = TRUE)) %in% c("cat")
            factor_menu <<- gcombobox(
                names(GUI$getActiveData(lazy = TRUE))[factorIndices],
                selected = 0
            )
            addHandlerChanged(factor_menu,
                handler = function(h, ...) {
                    factor_levels[] <<-
                        levels(GUI$getActiveData(lazy = TRUE)[[svalue(factor_menu)]])
                    svalue(new_varname) <<-
                        makeNames(sprintf("%s_coll", svalue(h$obj)))
                }
            )
            add_body(factor_menu)

            lbl <- glabel("Hold CTRL to choose many")
            font(lbl) <- list(size = 8, weight = "bold")
            add_body(lbl, anchor = c(-1, 0))

            factor_levels <<- gtable(
                list(Levels = ""),
                multiple = TRUE,
                expand = TRUE
            )
            addHandlerSelectionChanged(factor_levels,
                handler = function(h, ...) {
                    svalue(new_level) <<- paste(svalue(h$obj), collapse = "_")
                }
            )
            add_body(factor_levels, expand = TRUE)

            ## name boxes
            new_varname <<- gedit("")
            new_level <<- gedit("")

            tbl <- glayout()
            tbl[1L, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl4
            tbl[1L, 2L, expand = TRUE] <- new_varname
            tbl[2L, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl5
            tbl[2L, 2L, expand = TRUE] <- new_level

            add_body(tbl)
        },
        ## check whether the specified levels are ellegible
        ## for collapsing
        checkLevels = function(levels) {
            if (!is.null(levels) && length(levels) >= 2) return(TRUE)

            gmessage(title = "ALERT",
                    icon = "warning",
                    msg = "Need to select at least two levels to collapse",
                    parent = GUI$modWin)
            FALSE
        },
        collapse = function() {
            if (!checkLevels(svalue(factor_levels))) return()

            var <- svalue(factor_menu)
            lvls <- svalue(factor_levels)
            name <- svalue(new_varname)
            lvlname <- svalue(new_level)

            if (lvlname %in% levels(GUI$getActiveData(lazy = TRUE)[[var]]) &&
                !lvlname %in% lvls) {
                ## checking that the new level name isn't one of the other
                ## level names (excluding those being collapsed)
                gmessage("That level name already exists. Please choose another.",
                    title = "Invalid level name",
                    parent = GUI$modWin,
                    icon = "warning")
            } else if (checkNames(name)) {
                .dataset <- GUI$get_data_object(lazy = FALSE)
                data <- iNZightTools::collapse_cat(.dataset, var, levels = lvls, new_level = lvlname, name)
                updateData(data)
                dispose(GUI$modWin)
            }
        }
    )
)

## rename factor levels
iNZRenameFactorLevelsWin <- setRefClass(
    "iNZRenameFactorLevelsWin",
    fields = list(
        factor_menu = "ANY",
        factor_name = "ANY",
        level_table = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Rename Levels",
                width = "small",
                height = "large",
                help = "user_guides/variables/#renamelevs",
                ok = "Rename",
                action = .self$rename,
                show_code = FALSE,
                scroll = TRUE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("rename")

            lbl1 <- glabel("Choose variable: ")
            lbl2 <- glabel("New variable name: ")

            ## choose a factor column from the dataset and display
            ## its levels together with their order
            factorIndices <- iNZightTools::vartypes(GUI$getActiveData(lazy = TRUE)) %in% c("cat")
            factor_menu <<- gcombobox(
                names(GUI$getActiveData(lazy = TRUE))[factorIndices],
                selected = 0L,
                handler = function(h, ...) displayLevels()
            )
            factor_name <<- gedit("")

            tbl <- glayout()
            tbl[1L, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl1
            tbl[1L, 2L, expand = TRUE] <- factor_menu
            tbl[2L, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl2
            tbl[2L, 2L, expand = TRUE] <- factor_name
            add_body(tbl)

            body_space(10L)

            ## Use a separate table for the levels:
            add_body(
                glabel("Specify new level names: "),
                fill = TRUE,
                anchor = c(-1, 0)
            )
            level_table <<- glayout()
            visible(level_table) <<- FALSE
            add_body(level_table)
        },
        displayLevels = function() {
            # delete existing levels (e.g., if user changes variable choice)
            if (length(level_table$children)) {
                try(
                    invisible(
                        sapply(level_table$children, level_table$remove_child)
                    ),
                    silent = TRUE
                )
            }

            var <- GUI$getActiveData(lazy = TRUE)[[svalue(factor_menu)]]
            var_levels <- levels(var)
            invisible(
                sapply(
                    seq_along(var_levels),
                    function(i) {
                        level_table[i, 1L, expand = TRUE, anchor = c(1, 0)] <<-
                            glabel(var_levels[i])
                        level_table[i, 2L] <<- gedit(var_levels[i])
                    }
                )
            )

            # set the name
            svalue(factor_name) <<- sprintf("%s_rename", svalue(factor_menu))
            visible(level_table) <<- TRUE
        },
        changeLevels = function() {
            if (svalue(factor_menu) == 0L) {
                gmessage(msg = "Please choose a factor to reorder",
                    icon = "error",
                    parent = GUI$modWin
                )
                return(FALSE)
            }

            var <- GUI$getActiveData(lazy = TRUE)[[svalue(factor_menu)]]
            var_levels <- levels(var)
            new_levels <- sapply(level_table[seq_along(var_levels), 2L], svalue)
            names(var_levels) <- new_levels

            ## check if all order numbers are unique
            if (anyDuplicated(new_levels) > 0L) {
                gmessage(msg = "Please choose unique names for the levels",
                    icon = "error",
                    parent = GUI$modWin
                )
                return(FALSE)
            }

            changed <- sapply(seq_along(var_levels),
                function(i) new_levels[i] != var_levels[i]
            )
            as.list(var_levels)[changed]
        },
        rename = function() {
            var <- svalue(factor_menu)
            newlvls <- changeLevels()

            ## newFactor will be FALSE, if the user input was wrong
            name <- svalue(factor_name)
            if (!is.list(newlvls) || !checkNames(name)) return()

            .dataset <- GUI$get_data_object(lazy = FALSE)
            data <- iNZightTools::rename_levels(.dataset, var, tobe_asis = newlvls, name)
            updateData(data)
            close()
        }
    )
)

## reorder factor levels
iNZReorderLevelsWin <- setRefClass(
    "iNZReorderLevelsWin",
    fields = list(
        factorMenu = "ANY",
        factorName = "ANY",
        sortMenu = "ANY",
        levelOrder = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Reorder levels",
                width = "small",
                height = "med",
                help = "user_guides/variables/#reorderLvls",
                ok = "Reorder",
                action = .self$reorder,
                show_code = FALSE,
                scroll = TRUE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("reorder")

            tbl <- glayout()

            ## Choose variable to reorder:
            tbl[1, 1, expand = TRUE, anchor = c(1, 0)] <- glabel("Variable to reorder:")
            factorIndices <- iNZightTools::vartypes(GUI$getActiveData(lazy = TRUE)) %in% c("cat")
            factorMenu <<- gcombobox(
                names(GUI$getActiveData(lazy = TRUE))[factorIndices],
                selected = 0
            )
            tbl[1, 2, expand = TRUE] <- factorMenu

            ## Name for the new variable
            tbl[2, 1, expand = TRUE, anchor = c(1, 0)] <- glabel("New variable name:")
            factorName <<- gedit("")
            tbl[2, 2] <- factorName

            ## Sort method: frequency (default), or manual
            tbl[3, 1, expand = TRUE, anchor = c(1, 0)] <- glabel("Sort levels ")
            sortMenu <<- gcombobox(c("by frequency", "manually"), selected = 1)
            tbl[3, 2, expand = TRUE] <- sortMenu

            ## For manual ordering, gdf or gtable with up/down arrows ...
            levelGrp <- ggroup()
            levelOrder <<- gtable(data.frame(stringsAsFactors = TRUE), container = levelGrp)
            size(levelOrder) <<- c(-1, 280)
            tbl[4:5, 2, expand = TRUE] <- levelGrp

            levelBtnGrp <- gvbox()
            addSpace(levelBtnGrp, 20)
            levelUp <- iNZight:::gimagebutton("up",
                container = levelBtnGrp,
                size = 'LARGE_TOOLBAR',
                expand = FALSE,
                anchor = c(1, 0)
            )
            levelDown <- iNZight:::gimagebutton("down",
                container = levelBtnGrp,
                size = 'LARGE_TOOLBAR',
                expand = FALSE,
                anchor = c(1, 0)
            )
            levelHelp <- glabel("Select level, then\nuse arrows to reorder.",
                container = levelBtnGrp, anchor = c(1, 0))
            tbl[4:5, 1, anchor = c(1, 1)] <- levelBtnGrp

            visible(levelBtnGrp) <- visible(levelGrp) <- FALSE

            ## Add everything to main window
            add_body(tbl)

            ## HANDLERS
            addHandlerChanged(factorMenu,
                handler = function(h, ...) {
                    svalue(factorName) <<- makeNames(sprintf("%s.reord", svalue(factorMenu)))
                    levelOrder$set_items(
                        data.frame(
                            Levels = levels(GUI$getActiveData(lazy = TRUE)[[svalue(factorMenu)]]),
                            stringsAsFactors = TRUE
                        )
                    )
                }
            )

            addHandlerChanged(sortMenu,
                handler = function(h, ...) {
                    visible(levelBtnGrp) <-
                    visible(levelGrp) <-
                        svalue(sortMenu, index = TRUE) == 2
                }
            )

            addHandlerClicked(levelUp,
                function(h, ...) {
                    # blockHandlers(levelUp)
                    # blockHandlers(levelDown)
                    i <- svalue(levelOrder, index = TRUE)
                    if (length(i) == 0) {
                        gmessage('Select a level, then use the arrows to shift it up/down')
                        return()
                    }
                    lvls <- levelOrder$get_items()
                    if (i == 1) return()
                    li <- lvls[i]
                    lvls[i] <- lvls[i - 1]
                    lvls[i - 1] <- li
                    levelOrder$set_items(
                        data.frame(Levels = lvls, stringsAsFactors = TRUE)
                    )
                    svalue(levelOrder) <<- li
                    # unblockHandlers(levelUp)
                    # unblockHandlers(levelDown)
                }
            )
            addHandlerClicked(levelDown,
                function(h, ...) {
                    # blockHandlers(levelUp)
                    # blockHandlers(levelDown)
                    i <- svalue(levelOrder, index = TRUE)
                    if (length(i) == 0) {
                        gmessage('Select a level, then use the arrows to shift it up/down')
                        return()
                    }
                    lvls <- levelOrder$get_items()
                    if (i == length(lvls)) return()
                    li <- lvls[i]
                    lvls[i] <- lvls[i + 1]
                    lvls[i + 1] <- li
                    levelOrder$set_items(
                        data.frame(Levels = lvls, stringsAsFactors = TRUE)
                    )
                    svalue(levelOrder) <<- li
                    # unblockHandlers(levelUp)
                    # unblockHandlers(levelDown)
                }
            )
        },
        reorder = function() {
            var <- svalue(factorMenu)
            varname <- svalue(factorName)
            .dataset <- GUI$get_data_object(lazy = FALSE)

            if (!checkNames(varname)) return()
            if (svalue(sortMenu, TRUE) == 1) {
                data <- iNZightTools::reorder_levels(.dataset, var, auto = "freq", name = varname)
            } else {
                levels <- as.character(levelOrder$get_items())
                data <- iNZightTools::reorder_levels(.dataset, var, new_levels = levels, name = varname)
            }
            updateData(data)
            close()
        }
    )
)


## combine categorical variables
iNZCombineWin <- setRefClass(
    "iNZCombineWin",
    fields = list(
        factorNames = "ANY",
        newName = "ANY",
        varSep = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Combine Categorical Variables",
                width = "small",
                height = "med",
                help = "user_guides/variables/#catcombine",
                ok = "Combine",
                action = .self$combine,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("combine")

            add_heading(
                "Choose two or more variables to combine."
            )
            add_heading(
                "Hold CTRL to choose many",
                size = 8,
                weight = "bold"
            )

            lbl3 <- glabel("New Variable Name")

            ## choose a factor column from the dataset and display
            ## its level in a gtable
            factorIndices <- iNZightTools::vartypes(GUI$getActiveData(lazy = TRUE)) %in% c("cat")
            factorNames <<- gtable(
                list("Categorical Variables" = names(GUI$getActiveData(lazy = TRUE))[factorIndices]),
                multiple = TRUE,
                expand = TRUE
            )
            newName <<- gedit()

            ## separator (. or _ for now ...)
            lbl4 <- glabel("Value separator")
            varSep <<- gcombobox(c("_", "."), selected = 1, editable = TRUE)
            ## automatically fill the name field when variables are selected
            addHandlerSelectionChanged(factorNames,
                handler = function(h, ...) {
                    if (length(svalue(factorNames)) > 1)
                    svalue(newName) <<-
                        makeNames(
                            paste(svalue(factorNames),
                                collapse = svalue(varSep)
                            )
                        )
                    else svalue(newName) <<- ""
                }
            )
            addHandlerChanged(varSep,
                function(h, ...) {
                    if (length(svalue(factorNames)) <= 1) return()
                    sep <- svalue(h$obj)
                    # osep <- switch(sep, "_" = ".", "." = "_")
                    oname <- makeNames(paste(svalue(factorNames), collapse = sep))
                    if (svalue(newName) == oname) {
                        ## user hasn't changed the name, so update it
                        svalue(newName) <<-
                            makeNames(paste(svalue(factorNames), collapse = sep))
                    }
                }
            )

            add_body(factorNames, expand = TRUE)

            tbl <- glayout()
            tbl[1, 1, anchor = c(1, 0), expand = TRUE] <- lbl3
            tbl[1, 2, expand = TRUE] <- newName
            tbl[2, 1, anchor = c(1, 0), expand = TRUE] <- lbl4
            tbl[2, 2, expand = TRUE] <- varSep
            add_body(tbl)
        },
        ## check whether the specified variables are illegible
        ## for combining
        checkSelection = function(levels, name) {
            if (is.null(levels) || length(levels) < 2) {
                gmessage(title = "Not enough variables selected",
                    icon = "error",
                    msg = "Need to select at least two variables to combine",
                    parent = GUI$modWin
                )
                FALSE
            } else if (length(name) == 0) {
                gmessage(title = "New name not specified",
                    icon = "error",
                    msg = "Please specify a non-empty name for the new variable",
                    parent = GUI$modWin
                )
                FALSE
            } else {
                TRUE
            }
        },
        combine = function() {
            vars <- svalue(factorNames)
            name <- svalue(newName)
            sep <- svalue(varSep)

            chks <- checkSelection(vars, name)
            if (!chks || !checkNames(name)) return()

            .dataset <- GUI$get_data_object(lazy = FALSE)
            data <- iNZightTools::combine_vars(.dataset, vars, sep, name)
            updateData(data)
            close()
        }
    )
)

## create new variables using an expression
iNZCreateVarWin <- setRefClass(
    "iNZCreateVarWin",
    fields = list(
        var_name = "ANY",
        expression = "ANY",
        layout = "ANY",
        ex_layout = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Create new variable",
                width = "small",
                height = "small",
                help = "user_guides/variables/#create",
                ok = "Create",
                action = .self$create,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("create")

            add_heading(
                "Type an expression in the box on the right",
                "using existing variables in the dataset",
                "to create a new variable."
            )

            body_space(10L)

            layout <<- glayout(spacing = 2L)

            lbl <- glabel("Variable name")
            font(lbl) <- list(size = 8L, weight = "bold")
            layout[1L, 1L, anchor = c(-1, -1), expand = TRUE] <<- lbl

            lbl <- glabel("Expression")
            font(lbl) <- list(size = 8L, weight = "bold")
            layout[1L, 3L, anchor = c(-1, -1), expand = TRUE] <<- lbl

            var_name <<- gedit("", width = 15)
            expression <<- gedit("")
            layout[2L, 1L, fill = TRUE] <<- var_name
            layout[2L, 2L] <<- glabel(" = ")
            layout[2L, 3L, fill = TRUE] <<- expression

            add_body(layout)

            lbl <- glabel(
                paste(
                    "The new name will automatically be converted",
                    "to a valid name after creation."
                )
            )
            font(lbl) <- list(size = 8L)
            add_body(lbl, fill = TRUE, anchor = c(-1, 0))

            lbl <- glabel(
                paste(
                    "You must quote (using \") any non-numeric values",
                    "that are not the names\nof variables",
                    "(see 'date' example below)."
                )
            )
            font(lbl) <- list(size = 8L)
            add_body(lbl, fill = TRUE, anchor = c(-1, 0))

            body_space(10L)

            g_ex <- gexpandgroup("Examples")
            visible(g_ex) <- FALSE

            ex_layout <<- glayout(container = g_ex)
            examples <- list(
                c("bmi", "weight / height^2"),
                c("score_diff", "score_after - score_before"),
                c("date", format(Sys.time(), "\"%Y-%m-%d\"")),
                c("random_noise", "rnorm(N, 100, 5)")
            )
            if ("N" %in% names(GUI$getActiveData(lazy = TRUE))) {
                examples[[4]][2] <- "rnorm(dplyr::n(), 100, 5)"
            }

            sapply(seq_along(examples),
                function(i) {
                    lbl <- glabel(examples[[i]][1])
                    ex_layout[i, 1L, anchor = c(1, 0), expand = TRUE] <<- lbl
                    ex_layout[i, 2L] <<- glabel(" = ")
                    lbl <- glabel(examples[[i]][2])
                    ex_layout[i, 3L, anchor = c(-1, 0), expand = TRUE] <<- lbl
                }
            )

            add_body(g_ex)
            body_spring()
        },
        create = function() {
            .dataset <- GUI$get_data_object(lazy = FALSE)

            vname <- iNZightTools::make_names(
                svalue(var_name),
                names(GUI$getActiveData(lazy = TRUE))
            )

            expr <- svalue(expression)
            if (! "N" %in% names(GUI$getActiveData(lazy = TRUE))) {
                expr <- stringr::str_replace(expr,
                    "([^a-zA-Z0-9])N([^a-zA-Z0-9])",
                    "\\1dplyr::n()\\2"
                )
            }

            data <- try(
                iNZightTools::create_vars(
                    .dataset,
                    vars = vname,
                    vars_expr = expr
                ),
                silent = TRUE
            )

            if (inherits(data, 'try-error')) {
                print(data)
                err <- strsplit(data, "\n")[[1]]
                ew <- grepl('Evaluation error', err, fixed = TRUE)
                err <- ifelse(any(ew), gsub('Evaluation error:', '', err[ew]), '')

                gmessage(paste(sep = "\n\n", 'Invalid expression:', err),
                    icon = 'error',
                    parent = GUI$modWin
                )
                return()
            }

            updateData(data)
            close()
        }
    )
)


iNZFormClassIntervalsWin <- setRefClass(
    "iNZFormClassIntervalsWin",
    contains = "iNZDataModWin",
    fields = list(
        variable = "ANY",
        discrete = "logical",
        type = "ANY",
        tbl_width = "ANY", tbl_count = "ANY",
        tbl_range = "ANY", tbl_manual = "ANY",
        tbl_format = "ANY", tbl_format_lower = "ANY", tbl_format_upper = "ANY",
        size_lbl = "ANY",
        n_interval = "ANY",
        interval_width = "ANY",
        start_point = "ANY", end_point = "ANY",
        label_format = "ANY",
        label_lower = "ANY", label_upper = "ANY",
        breaks = "ANY",
        varname = "ANY",
        preview_levels = "ANY",
        okBtn = "ANY", skip_update = "logical"
    ),
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Form class intervals",
                width = "med",
                height = "large",
                help = "user_guides/variables/#classints",
                ok = "Create",
                action = .self$create,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("create")

            ## ------------------------------ MAIN CONTENT
            tbl <- glayout()
            ii <- 1L

            .dataset <- GUI$getActiveData(lazy = TRUE)
            nvars <- iNZightTools::vartypes(.dataset) %in% c("num", "dt")
            numvars <- names(.dataset)[nvars]
            lbl <- glabel("Variable :")
            variable <<- gcombobox(numvars,
                selected = 0,
                handler = function(h, ...) {
                    if (h$obj$get_index() == 0L) {
                        visible(tbl_width) <<- FALSE
                        visible(tbl_count) <<- FALSE
                        visible(tbl_range) <<- FALSE
                        visible(tbl_format) <<- FALSE
                        visible(tbl_format_lower) <<- FALSE
                        visible(tbl_format_upper) <<- FALSE
                        return()
                    }
                    x <- .dataset[[svalue(h$obj)]]
                    x <- x[!is.na(x)]
                    discrete <<- all(x == round(x))
                    # set visibility of enabled/disabled things:
                    tbl_range$remove_child(start_point)
                    tbl_range$remove_child(end_point)
                    start_point <<- gspinbutton(
                        min(x) - diff(range(x)),
                        max(x),
                        by = 0.1,
                        value = min(x),
                        handler = function(h, ...) {
                            visible(tbl_format_lower) <<- svalue(h$obj) > min(x)
                            create_intervals()
                        }
                    )
                    end_point <<- gspinbutton(
                        min(x),
                        max(x) + diff(range(x)),
                        by = 0.1,
                        value = max(x),
                        handler = function(h, ...) {
                            visible(tbl_format_upper) <<- svalue(h$obj) < max(x)
                            create_intervals()
                        }
                    )
                    size(start_point) <<- c(250, -1)
                    tbl_range[1L, 2:3] <<- start_point
                    tbl_range[2L, 2:3] <<- end_point

                    fmts <- if (discrete) c("[a,b]", "a-b")
                        else c("(a,b]", "[a,b)")
                    label_format$set_items(fmts)

                    label_lower$set_items(
                        if (discrete)
                            c(paste(spec_char("lte"), "a"))
                        else
                            c("< a")
                    )
                    label_upper$set_items(
                        if (discrete)
                            c(paste(spec_char("gte"), "b"), "b+")
                        else
                            c("> b", "b+")
                    )

                    type$invoke_change_handler()
                }
            )
            size(variable) <<- c(250, -1)
            tbl[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:3] <- variable
            ii <- ii + 1L

            lbl <- glabel("Interval method :")
            type <<- gradio(
                c("Equal width", "Fixed width", "Equal count", "Manual"),
                selected = 1L,
                handler = function(h, ...) {
                    # set visibility of things
                    k <- h$obj$get_index()
                    if (variable$get_index() == 0L) return()
                    visible(tbl_width) <<- k == 1 || k == 3
                    visible(tbl_count) <<- k == 2
                    visible(tbl_range) <<- k <= 2
                    visible(tbl_manual) <<- k == 4
                    visible(tbl_format) <<- TRUE
                    if (k == 3L) {
                        visible(tbl_format_lower) <<- FALSE
                        visible(tbl_format_upper) <<- FALSE
                    } else {
                        skip_update <<- TRUE
                        start_point$invoke_change_handler()
                        end_point$invoke_change_handler()
                        skip_update <<- FALSE
                    }
                    create_intervals()
                }
            )
            tbl[ii, 1L, anchor = c(1, 1), expand = TRUE] <- lbl
            tbl[ii, 2:3, fill = TRUE] <- type
            ii <- ii + 1L

            add_body(tbl)
            add_body(gseparator())

            tbl_width <<- glayout()
            visible(tbl_width) <<- FALSE
            ii <- 1L

            lbl <- glabel("Number of intervals :")
            n_interval <<- gspinbutton(2L, 100L, by = 1L,
                value = 4L,
                handler = function(h, ...) {
                    create_intervals()
                }
            )
            size(n_interval) <<- c(250, -1)
            tbl_width[ii, 1L, anchor = c(1, 0), expand = TRUE] <<- lbl
            tbl_width[ii, 2:3] <<- n_interval
            ii <- ii + 1L

            add_body(tbl_width)

            tbl_count <<- glayout()
            visible(tbl_count) <<- FALSE
            ii <- 1L

            lbl <- "Interval width :"
            interval_width <<- gspinbutton(1L, 100L, by = 1L,
                value = 10L,
                handler = function(h, ...) {
                    create_intervals()
                }
            )
            size(interval_width) <<- c(250, -1)
            tbl_count[ii, 1L, anchor = c(1, 0), expand = TRUE] <<- lbl
            tbl_count[ii, 2:3] <<- interval_width
            ii <- ii + 1L

            add_body(tbl_count)

            body_space(2L)

            tbl_range <<- glayout()
            visible(tbl_range) <<- FALSE
            ii <- 1L

            lbl <- glabel("Start point :")
            start_point <<- glabel("Choose variable")
            size(start_point) <<- c(250, -1)
            tbl_range[ii, 1L, anchor = c(1, 0), expand = TRUE] <<- lbl
            tbl_range[ii, 2:3] <<- start_point
            ii <- ii + 1L

            lbl <- glabel("End point :")
            end_point <<- glabel("Choose variable")
            tbl_range[ii, 1L, anchor = c(1, 0), expand = TRUE] <<- lbl
            tbl_range[ii, 2:3] <<- end_point
            ii <- ii + 1L

            add_body(tbl_range)

            tbl_manual <<- glayout()
            visible(tbl_manual) <<- FALSE
            ii <- 1L

            lbl <- glabel("Breakpoints :")
            breaks <<- gedit("",
                handler = function(h, ...) create_intervals())
            size(breaks) <<- c(250, -1)
            tbl_manual[ii, 1L, anchor = c(1, 0), expand = TRUE] <<- lbl
            tbl_manual[ii, 2:3] <<- breaks
            ii <- ii + 1L

            lbl <- glabel("Comma separated break points.\ne.g., 5, 10, 20, 30")
            font(lbl) <- list(size = 9)
            tbl_manual[ii, 2:3, anchor = c(-1, 0), expand = TRUE] <<- lbl
            ii <- ii + 1L

            add_body(tbl_manual)

            tbl_format <<- glayout()
            ii <- 1L
            visible(tbl_format) <<- FALSE

            lbl <- glabel("Label format :")
            label_format <<- gradio(
                "",
                horizontal = TRUE,
                handler = function(h, ...) create_intervals())
            size(label_format) <<- c(250, -1)
            tbl_format[ii, 1L, anchor = c(1, 0), expand = TRUE] <<- lbl
            tbl_format[ii, 2:3] <<- label_format
            ii <- ii + 1L

            add_body(tbl_format)

            tbl_format_lower <<- glayout()
            visible(tbl_format_lower) <<- FALSE
            lbl <- glabel("Format lower bound :")
            label_lower <<- gradio("", horizontal = TRUE,
                handler = function(h, ...) create_intervals())
            size(label_lower) <<- c(250, -1)
            tbl_format_lower[1L, 1L, anchor = c(1, 0), expand = TRUE] <<- lbl
            tbl_format_lower[1L, 2:3] <<- label_lower
            add_body(tbl_format_lower)

            tbl_format_upper <<- glayout()
            visible(tbl_format_upper) <<- FALSE
            lbl <- glabel("Format upper bound :")
            label_upper <<- gradio("", horizontal = TRUE,
                handler = function(h, ...) create_intervals())
            size(label_upper) <<- c(250, -1)
            tbl_format_upper[1L, 1L, anchor = c(1, 0), expand = TRUE] <<- lbl
            tbl_format_upper[1L, 2:3] <<- label_upper
            add_body(tbl_format_upper)

            tbl <- glayout()
            ii <- 1L

            lbl <- glabel("Class Interval labels :")
            font(lbl) <- list(size = 9, weight = "bold")
            tbl[ii, 1L, anchor = c(-1, 0), expand = TRUE] <- lbl
            ii <- ii + 1L

            preview_levels <<- gtext("", height = 100)
            enabled(preview_levels) <<- FALSE

            tbl[ii, 1:3] <- preview_levels
            ii <- ii + 1L
            add_body(tbl)

            skip_update <<- FALSE

        },
        create_intervals = function(preview = TRUE) {
            if (skip_update) return()

            data <- GUI$getActiveData(lazy = TRUE)

            break_points <- NULL
            if (svalue(type) == "Manual") {
                if (trimws(svalue(breaks)) == "") return()
                xr <- range(data[[svalue(variable)]], na.rm = TRUE)
                break_points <- as.numeric(strsplit(svalue(breaks), ",")[[1]])
                break_points <- c(xr[1], break_points, xr[2])
            }

            .dataset <- GUI$get_data_object(lazy = FALSE)
            if (preview && !iNZightTools::is_survey(.dataset)) {
                .dataset <- .dataset[svalue(variable)]
            }
            result <- iNZightTools::form_class_intervals(
                .dataset,
                variable = svalue(variable),
                method = switch(svalue(type),
                    "Equal width" = "equal",
                    "Fixed width" = "width",
                    "Equal count" = "count",
                    "Manual" = "manual"
                ),
                n_intervals = svalue(n_interval),
                interval_width = svalue(interval_width),
                format = svalue(label_format),
                range = c(
                    as.numeric(svalue(start_point)),
                    as.numeric(svalue(end_point))
                ),
                format_lowest = svalue(label_lower),
                format_highest = svalue(label_upper),
                break_points = break_points
            )

            if (preview) {
                lvls <- if (iNZightTools::is_survey(result))
                    levels(result$variables[[ncol(result$variables)]])
                    else levels(result[[2]])
                lvls <- paste(lvls, collapse = ", ")
                svalue(preview_levels) <<- lvls
            } else {
                updateData(result)
                dispose(GUI$modWin)
            }
        },
        create = function() create_intervals(preview = FALSE)
    )
)

## rename variables. This overwrites the old variable name, i.e. does not
## create a new variable
iNZRenameVarWin <- setRefClass(
    "iNZRenameVarWin",
    fields = list(
        names_table = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Rename variables",
                width = "small",
                height = "large",
                help = "user_guides/variables/#renamevars",
                ok = "Rename",
                action = .self$rename,
                show_code = FALSE,
                scroll = TRUE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("rename")

            add_heading(
                "Type new names for variables in the boxes below."
            )
            body_space(10L)

            vnames <- names(GUI$getActiveData(lazy = TRUE))

            names_table <<- glayout()
            invisible(
                sapply(seq_along(vnames),
                    function(pos) {
                        names_table[pos, 1L, anchor = c(1, 0), expand = TRUE] <<-
                            glabel(vnames[pos])
                        names_table[pos, 2L] <<- gedit(vnames[pos])
                    }
                )
            )
            add_body(names_table)
        },
        rename = function() {
            old_names <- sapply(names_table[, 1L], svalue)
            new_names <- sapply(names_table[, 2L], svalue)

            tbl <- table(new_names)
            if (any(tbl > 1L)) {
                dup <- names(tbl)[tbl > 1L]
                gmessage(
                    paste(sep = " ",
                        "You cannot use the same name twice.",
                        "Please rename the following variables:",
                        paste("\n - ", dup, collapse = "")
                    ),
                    title = "Duplicated variable names",
                    icon = "warning",
                    parent = GUI$modWin
                )
                return()
            }

            w <- old_names != new_names
            if (!any(w)) return()

            name_list <- setNames(as.list(old_names[w]), new_names[w])

            .dataset <- GUI$get_data_object(lazy = FALSE)
            data <- iNZightTools::rename_vars(.dataset, tobe_asis = name_list)
            updateData(data)
            close()
        }
    )
)

## standardise variables
iNZStandardiseWin <- setRefClass(
    "iNZStandardiseWin",
    fields = list(
        numVar = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Standardise variables",
                width = "small",
                height = "med",
                help = "user_guides/variables/#standardize",
                ok = "Standardise",
                action = .self$standardise,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("standardise")

            add_heading(
                "Select variables to standardise.",
                "Standardised variable will have mean 0 and standard deviation 1."
            )
            add_heading(
                "Hold CTRL to choose many",
                weight = "bold", size = 8
            )

            body_space(5L)

            ## display only numeric variables
            numIndices <- iNZightTools::vartypes(GUI$getActiveData(lazy = TRUE)) %in% c("num", "dt")
            numVar <<- gtable(
                list("Variables" = names(GUI$getActiveData(lazy = TRUE))[numIndices]),
                multiple = TRUE
            )

            add_body(numVar, expand = TRUE)
        },
        standardise = function() {
            if (length(svalue(numVar)) == 0) return()

            varnames <- svalue(numVar)
            names <- makeNames(paste0(varnames, ".std"))
            .dataset <- GUI$get_data_object(lazy = FALSE)
            data <- iNZightTools::standardize_vars(.dataset, vars = varnames, names)
            updateData(data)
            close()
        }
    )
)

## delete variables
iNZDeleteVarWin <- setRefClass(
    "iNZDeleteVarWin",
    fields = list(
        vars = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Delete variables",
                width = "small",
                height = "med",
                help = "user_guides/variables/#deletevars",
                ok = "Delete",
                action = .self$delete,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("delete")

            add_heading(
                "Select variables to delete from the dataset."
            )
            add_heading(
                "Hold CTRL to choose many",
                size = 8L,
                weight = "bold"
            )

            vars <<- gtable(
                list(Variable = names(GUI$getActiveData(lazy = TRUE))),
                multiple = TRUE
            )
            add_body(vars, expand = TRUE)
        },
        delete = function() {
            v <- svalue(vars)
            if (length(v) == 0L) return()
            if (length(v) == length(names(GUI$getActiveData(lazy = TRUE)))) {
                gmessage(
                    "You can't delete all of the variables ... you'll have nothing left!",
                    title = 'Oops...',
                    icon = 'error',
                    parent = GUI$modWin
                )
                return()
            }

            conf <- gconfirm(
                title = "Confirm variable deletion",
                msg = paste(
                    "You are about to delete the",
                    "following variables:\n",
                    paste("\n - ", v, collapse = ""),
                    "\n\nAre you sure?"
                ),
                icon = "question"
            )
            if (!conf) return()

            .dataset <- GUI$get_data_object(lazy = FALSE)
            data <- iNZightTools::delete_vars(.dataset, vars = v)
            updateData(data)
            close()
        }
    )
)

## Missing as Cat
iNZMissToCatWin <- setRefClass(
    "iNZMissToCatWin",
    field = list(
        vars = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Missing to Categorical",
                width = "small",
                height = "med",
                help = "user_guides/variables/#missingcat",
                ok = "Convert",
                action = .self$convert,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("convert")

            add_heading(
                "Create a new variable identifying missing values",
                "in the chosen variable(s)."
            )

            add_heading(
                "Hold CTRL to choose many",
                size = 8L,
                weight = "bold"
            )

            vars <<- gtable(
                list(Variables = names(GUI$getActiveData(lazy = TRUE))),
                multiple = TRUE
            )
            add_body(vars, expand = TRUE)
        },
        convert = function() {
            if (length(svalue(vars)) == 0L) return()

            v <- svalue(vars)
            names <- makeNames(paste0(v, "_miss"))

            .dataset <- GUI$get_data_object(lazy = FALSE)
            data <- iNZightTools::missing_to_cat(.dataset, vars = v, names)
            updateData(data)
            close()
        }
    )
)


# iNZrankNumWin: Rank the numerical variables X (vector, matrix)
iNZRankWin <- setRefClass(
  "iNZRankWin",
  fields = list(
      rank_vars = "ANY"
  ),
  contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Rank variables",
                width = "small",
                height = "med",
                help = "user_guides/variables/#rank",
                ok = "Rank",
                action = .self$rank,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("rank")

            add_heading(
                "Choose variables to rank.",
                "A new variable will be created with the rank order",
                "of the chosen variable(s)."
            )
            add_heading(
                "Hold CTRL to choose many.",
                weight = "bold",
                size = 8L
            )

            ## display only numeric variables
            numIndices <- iNZightTools::vartypes(GUI$getActiveData(lazy = TRUE)) %in% c("num", "dt")
            rank_vars <<- gtable(
                list(Variables = names(GUI$getActiveData(lazy = TRUE))[numIndices]),
                multiple = TRUE
            )

            add_body(rank_vars, expand = TRUE, fill = TRUE)

            visible(GUI$modWin) <<- TRUE
        },
        rank = function() {
            if (length(svalue(rank_vars)) == 0L) return()
            vars <- svalue(rank_vars)
            .dataset <- GUI$get_data_object(lazy = FALSE)
            data <- iNZightTools::rank_vars(.dataset, vars)
            updateData(data)
            close()
        }
    )
)

## Convert multiple variables to categorical type in the same time
iNZConToCatMultiWin <- setRefClass(
    "iNZConToCatMultiWin",
    fields = list(
        num_vars = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Convert to Categorical",
                width = "small",
                height = "med",
                help = "user_guides/variables/#convert2",
                ok = "Convert",
                action = .self$convert,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("convert")

            add_heading("Select variables to convert to categorical.")
            add_heading(
                "Hold CTRL to choose many.",
                weight = "bold",
                size = 8L
            )

            ## display only numeric variables
            numIndices <- iNZightTools::vartypes(GUI$getActiveData(lazy = TRUE)) %in% c("num", "dt")
            num_vars <<- gtable(
                list(Variables = names(GUI$getActiveData(lazy = TRUE))[numIndices]),
                multiple = TRUE
            )
            add_body(num_vars, expand = TRUE, fill = TRUE)
        },
        convert = function() {
            if (length(svalue(num_vars)) == 0) return()

            vars <- svalue(num_vars)
            varnames <- makeNames(paste(vars, "cat", sep = "."))

            .dataset <- GUI$get_data_object(lazy = FALSE)
            data <- iNZightTools::convert_to_cat(.dataset, vars, names = varnames)
            updateData(data)
            dispose(GUI$modWin)
        }
    )
)

iNZRenameDataWin <- setRefClass(
    "iNZRenameDataWin",
    fields = list(
        name = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list (
        initialize = function(gui) {
            ok <- callSuper(gui,
                title = "Rename dataset",
                width = "small",
                height = "small",
                ok = "Rename",
                action = .self$rename_data,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("rename_data")

            lbl <- glabel("Enter a new name for the current dataset")
            font(lbl) <- list(weight = "bold", family = "sans")

            curname <- attr(GUI$getActiveData(lazy = TRUE), "name", exact = TRUE)
            if (length(curname) == 0) curname <- ""
            name <<- gedit(curname)

            add_body(lbl)
            add_body(name)
        },
        rename_data = function() {
            newname <- svalue(name)
            if (newname == "") {
                gmessage("Please enter a name", icon = "error", parent = GUI$win)
            } else if (newname %in% GUI$dataNameWidget$nameLabel$get_items()) {
                gmessage("Oops... that name is used by another dataset. Try something else!")
            } else {
                GUI$getActiveDoc()$dataModel$setName(newname)
                close()
            }
        }
    )
)


## Convert variables to a date time type
iNZConToDtWin <- setRefClass(
    "iNZConToDtWin",
    fields = list(
        data = "data.frame",
        dt_vars = "ANY",
        vname = "ANY",
        time_fmt = "ANY",
        df_orig = "ANY", df_conv = "ANY"
    ),
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            if (iNZightTools::is_survey(gui$get_data_object(lazy = TRUE))) {
                gmessage(
                    "Survey designs are not handled by this action yet.",
                    title = "Surveys not handled",
                    icon = "error"
                )
                close()
            }

            ok <- callSuper(gui,
                title = "Convert to Dates and Times",
                width = "large",
                height = "large",
                help = "user_guides/variables/#dtconvert",
                ok = "Convert",
                action = .self$convert,
                show_code = FALSE,
                scroll = FALSE,
                body_direction = "horizontal"
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("convert", "add_format", "del_format")

            initFields(data = GUI$getActiveData(lazy = TRUE))

            add_heading(
                "Choose variable(s) to convert to a date/time.",
                "Specify the order of time values either from the drop-down",
                "or by using the Advanced Selection buttons.",
                "For help specifying values, use the Help button at the bottom."
            )

            left_panel <- gvbox()
            size(left_panel) <- c(400, -1)
            add_body(left_panel)

            dt_vars <<- gtable(
                names(data),
                multiple = TRUE,
                container = left_panel
            )
            names(dt_vars) <<- "Choose variable(s)"

            addHandlerSelectionChanged(dt_vars,
                function(h, ...) select_variable())

            addSpace(left_panel, 5)

            name_string <- glabel("Name for the new variable",
                container = left_panel,
                anchor = c(-1, 0)
            )
            vname <<- gedit("", container = left_panel)

            dt.formats <- c(
                "",
                "year month date",
                "year month date Hour Minute Second",
                "year month date Hour Minute Second pm/am",
                "day month year",
                "day month year Hour Minute Second",
                "day month year Hour Minute Second pm/am",
                "Unix timestamp (secs from 1970)"
            )

            glabel("Specify date/time format",
                container = left_panel,
                anchor = c(-1, 0)
            )

            time_fmt <<- gcombobox(
                items = dt.formats,
                container = left_panel,
                editable = TRUE,
                handler = function(h,...) convert(preview = TRUE)
            )

            lbl <- glabel(
                add_lines(
                    paste(sep = " ",
                        "Choose a format from the dropdown above,",
                        "or click the buttons below in the order",
                        "they appear in the 'Original' column on the",
                        "right."
                    ),
                    nchar = 80L
                ),
                container = left_panel,
                anchor = c(-1, 0)
            )
            font(lbl) <- list(size = 8L)

            tbl <- glayout(
                container = left_panel,
                homogeneous = TRUE
            )
            tbl[1L, 1L] <- gbutton("year", handler = add_format)
            tbl[1L, 2L] <- gbutton("month", handler = add_format)
            tbl[1L, 3L] <- gbutton("day", handler = add_format)
            tbl[1L, 4L] <- gbutton("pm/am", handler = add_format)
            tbl[2L, 1L] <- gbutton("Hour", handler = add_format)
            tbl[2L, 2L] <- gbutton("Minute", handler = add_format)
            tbl[2L, 3L] <- gbutton("Second", handler = add_format)

            tbl[1L, 5L] <- gbutton("delete", handler = del_format)

            tbl[2L, 5L] <- gbutton("clear",
                handler = function(h, ...) svalue(time_fmt) <<- ""
            )

            lbl <- glabel(
                paste(sep = "\n",
                    "'Delete' will remove the last added value",
                    "'Clear' will remove all values"
                ),
                container = left_panel,
                anchor = c(-1, 0)
            )
            font(lbl) <- list(size = 8L)

            body_space(10)

            df_orig <<- gtable(
                data.frame(
                    Original = "",
                    stringsAsFactors = TRUE
                )
            )
            add_body(df_orig, expand = TRUE)

            df_conv <<- gtable(
                data.frame(
                    Converted = "",
                    stringsAsFactors = TRUE
                )
            )
            add_body(df_conv, expand = TRUE)
        },
        select_variable = function() {
            if (length(svalue(dt_vars)) == 0L) {
                df_orig$set_items("")
                df_conv$set_items("")
                svalue(vname) <<- ""
                return()
            }

            vars <- apply(
                as.data.frame(data[svalue(dt_vars)]),
                1,
                paste, collapse = " "
            )
            svalue(vname) <<- makeNames(
                sprintf("%s_dt",
                    paste(paste(svalue(dt_vars), collapse = "_"))
                )
            )
            df_orig$set_items(data.frame(Original = vars))

            convert(preview = TRUE)
        },
        add_format = function(h, ...) {
            svalue(time_fmt) <<- paste(
                svalue(time_fmt),
                svalue(h$obj)
            )
            time_fmt$invoke_change_handler()
        },
        del_format = function(h, ...) {
            fmt <- svalue(time_fmt)
            fmt <- strsplit(fmt, " ")[[1]]
            if (length(fmt) < 2L) {
                fmt <- ""
            } else {
                fmt <- paste(fmt[-length(fmt)], collapse = " ")
            }
            svalue(time_fmt) <<- fmt
        },
        convert = function(preview = FALSE) {
            if (length(svalue(dt_vars)) == 0) return()
            if (svalue(time_fmt) == "") return()
            if (svalue(vname) == "") return()

            .dataset <- GUI$get_data_object(lazy = FALSE)
            if (preview)
                .dataset <- .dataset[svalue(dt_vars)]

            tryCatch(
                {
                    res <- iNZightTools::convert_to_datetime(
                        .dataset,
                        svalue(dt_vars),
                        svalue(time_fmt),
                        svalue(vname)
                    )
                },
                warning = function(w) {
                    if (w$message == "Failed to parse") {
                        df_conv$set_items(
                            data.frame(Converted = "Invalid format")
                        )
                    } else {
                        df_conv$set_items(
                            data.frame(Converted = w$message)
                        )
                    }
                },
                finally = {
                    if (!exists("res")) return()
                    if (preview) {
                        df_conv$set_items(
                            data.frame(
                                Converted = res[[svalue(vname)]]
                            )
                        )
                    } else {
                        updateData(res)
                        close()
                    }
                }
            )
        }
    )
)


## Extract parts from a datetime variable
iNZExtFromDtWin <- setRefClass(
    "iNZExtFromDtWin",
    contains = "iNZDataModWin",
    fields = list(
        data = "ANY",
        dt_var = "ANY",
        element_tree = "ANY",
        vname = "ANY",
        df_orig = "ANY", df_prev = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            if (iNZightTools::is_survey(gui$get_data_object(lazy = TRUE))) {
                gmessage(
                    "Survey designs are not handled by this action yet.",
                    title = "Surveys not handled",
                    icon = "error"
                )
                close()
            }

            ok <- callSuper(gui,
                title = "Extract values from date/time",
                width = "large",
                height = 450L,
                help = "user_guides/variables/#dtextract",
                ok = "Extract",
                action = .self$extract,
                show_code = FALSE,
                scroll = FALSE,
                body_direction = "horizontal"
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("extract")

            initFields(data = GUI$getActiveData(lazy = TRUE))

            dt_vars <- names(data)[iNZightTools::vartypes(data) == "dt"]
            if (length(dt_vars) == 0L) {
                gmessage(
                    "No datetime variables to extract information from",
                    title = "No datetime variables",
                    icon = "info",
                    parent = GUI$win
                )
                close()
                return()
            }

            mainGroup <- gvbox()
            add_body(mainGroup)

            addSpace(mainGroup, 5)

            date_string <- glabel(
                "Select variable to extract information from",
                container = mainGroup,
                anchor = c(-1, 0)
            )

            dt_var <<- gcombobox(
                items = dt_vars,
                selected = 0L,
                container = mainGroup,
                handler = function(h,...) set_variable()
            )

            for.var <- glabel(
                paste(sep = "\n",
                    "Select elements to extract",
                    "(click + of lowest-level information for options)"
                ),
                container = mainGroup,
                anchor = c(-1, 0)
            )

            offspring <- function(path = character(0), lst, ...) {
                if (length(path))
                    obj <- lst[[path]]
                else
                    obj <- lst

                nms <- names(obj)
                hasOffspring <- sapply(nms,
                    function(i) {
                        newobj <- obj[[i]]
                        is.recursive(newobj) && !is.null(names(newobj))
                    }
                )

                data.frame(
                    Name = nms,
                    hasOffspring = hasOffspring,
                    stringsAsFactors = FALSE
                )
            }

            l <- iNZightTools:::get_dt_comp_tree(iNZightTools:::inz_dt_comp)

            element_tree <<- gtree(
                offspring = offspring,
                offspring.data = l,
                container = mainGroup
            )

            addHandlerClicked(element_tree,
                function(h, ...) set_component())

            date_string <- glabel("Name for new variable",
                container = mainGroup,
                anchor = c(-1, 0)
            )
            vname <<- gedit("", container = mainGroup)
            addHandlerKeystroke(vname,
                function(h, ...) extract(preview = TRUE)
            )

            body_space(10)

            df_orig <<- gtable(
                data.frame(
                    Original = "",
                    stringsAsFactors = TRUE
                )
            )
            add_body(df_orig, expand = TRUE)

            df_prev <<- gtable(
                data.frame(
                    Extracted = "",
                    stringsAsFactors = TRUE
                )
            )
            add_body(df_prev, expand = TRUE)
        },
        set_variable = function() {
            varname <- svalue(dt_var)
            if (length(varname) == 0L) {
                df_orig$set_items(data.frame(Original = ""))
                df_prev$set_items(data.frame(Extracted = ""))
                svalue(vname) <<- ""
                return()
            }

            varx <- as.character(data[[varname]])
            df_orig$set_items(data.frame(Original = varx))
            svalue(element_tree) <<- character()
            svalue(vname) <<- ""

            set_component()
        },
        set_component = function() {
            if (length(svalue(dt_var)) == 0L) return()
            if (length(svalue(element_tree)) == 0L) return()

            component <- svalue(element_tree)
            svalue(vname) <<- makeNames(
                sprintf("%s%s",
                    svalue(dt_var),
                    iNZightTools:::get_dt_comp(component[length(component)])$suffix
                )
            )

            extract(preview = TRUE)
        },
        extract = function(preview = FALSE) {
            if (length(svalue(dt_var)) == 0L) return()
            if (length(svalue(element_tree)) == 0L) return()
            if (svalue(vname) == "") return()

            .dataset <- GUI$getActiveData(lazy = FALSE)
            if (preview)
                .dataset <- .dataset[svalue(dt_var)]

            component <- svalue(element_tree)
            tryCatch(
                {
                    res <- iNZightTools::extract_dt_comp(
                        .dataset,
                        svalue(dt_var),
                        component[length(component)],
                        svalue(vname)
                    )
                },
                warning = function(w) {
                    df_prev$set_items(
                        data.frame(
                            Extracted = "Unable to extract element"
                        )
                    )
                },
                finally = {
                    if (!exists("res")) return()
                    if (preview) {
                        df_prev$set_items(
                            data.frame(
                                Extracted = res[[svalue(vname)]]
                            )
                        )
                    } else {
                        updateData(res)
                        close()
                    }
                }
            )
        }
    )
)

## Aggregate datetimes
iNZAggDtWin <- setRefClass(
    "iNZAggDtWin",
    contains = "iNZDataModWin",
    fields = list(
        dt_var = "ANY",
        type = "character",
        format = "ANY",
        method = "ANY",
        df_prev = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            if (iNZightTools::is_survey(gui$get_data_object(lazy = TRUE))) {
                gmessage(
                    "Survey designs are not handled by this action yet.",
                    title = "Surveys not handled",
                    icon = "error"
                )
                close()
            }

            ok <- callSuper(gui,
                title = "Aggregate date/time",
                width = "large",
                height = "large",
                help = "user_guides/variables/#dtaggregate",
                ok = "Aggregate",
                action = .self$aggregate,
                show_code = FALSE,
                scroll = FALSE,
                body_direction = "horizontal"
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("aggregate")

            add_heading(
                "Choose a date/time variable,",
                "then choose the time period to aggregate over.",
                "This will calculate the chosen values for all other",
                "variables in the dataset within each period."
            )

            left_panel <- gvbox()
            add_body(left_panel, expand = TRUE)

            glabel("Date/time variable",
                container = left_panel,
                anchor = c(-1, 0))

            cols <- names(GUI$getActiveData(lazy = TRUE))
            dt_var <<- gcombobox(cols,
                selected = 0L,
                container = left_panel,
                handler = function(h, ...) select_variable()
            )

            glabel("Aggregation interval :",
                container = left_panel,
                anchor = c(-1, 0))

            format <<- gcombobox("",
                container = left_panel,
                handler = function(h, ...) aggregate(preview = TRUE))

            glabel("Aggregation summary :",
                container = left_panel,
                anchor = c(-1, 0))

            method <<- gtable(
                list(Summary = c("Sum", "Mean", "Median", "Min", "Max")),
                container = left_panel
            )
            addHandlerSelectionChanged(method,
                handler = function(h, ...) aggregate(preview = TRUE)
            )

            body_space(10L)

            right_panel <- gvbox()
            add_body(right_panel, expand = TRUE)

            lbl <- glabel("Original dataset",
                container = right_panel,
                anchor = c(-1, 0))
            font(lbl) <- list(weight = "bold")

            df_orig <- gtable(head(GUI$getActiveData(lazy = TRUE)),
                container = right_panel)
            size(df_orig) <- c(450, -1)

            lbl <- glabel("Aggregated dataset",
                container = right_panel,
                anchor = c(-1, 0))
            font(lbl) <- list(weight = "bold")

            df_prev <<- gtable(
                data.frame(Preview = "Preview will show here"),
                container = right_panel)
        },
        select_variable = function() {
            var <- svalue(dt_var)
            df_prev$set_items(
                data.frame(Preview = "Preview will show here")
            )
            if (length(var) == 0L) return()

            x <- GUI$getActiveData(lazy = TRUE)[[var]]
            type <<- ""
            values <- character()

            if (lubridate::is.POSIXct(x) || lubridate::is.Date(x)) {
                type <<- "dt"
                values <- c("Weekly", "Monthly", "Quarterly", "Yearly")
            } else if (all(grepl("^[Y]?[0-9]+\\s?[W][0-9]+$", x, TRUE))) {
                type <<- "yearweek"
                values <- c("Quarterly", "Yearly")
            } else if (all(grepl("^[Y]?[0-9]+\\s?[M][0-9]+$", x, TRUE))) {
                type <<- "yearmonth"
                values <- c("Quarterly", "Yearly")
            } else if (all(grepl("^[Y]?[0-9]+\\s?[Q][0-9]+$", x, TRUE))) {
                type <<- "yearquarter"
                values <- c("Yearly")
            } else {
                gmessage("That variable does not contain date/time information.",
                    title = "Unsupported variable",
                    icon = "warning",
                    parent = GUI$modWin
                )
                return()
            }

            format$set_items(values)
            aggregate(preview = TRUE)
        },
        aggregate = function(preview = FALSE) {
            if (length(svalue(dt_var)) == 0L) return()
            if (length(svalue(format)) == 0L) return()
            if (length(svalue(method)) == 0L) return()

            .dataset <- GUI$getActiveData(lazy = FALSE)

            if (type == "dt" && length(svalue(format))) {
                part <- switch(svalue(format),
                    "Weekly" = "Year Week",
                    "Monthly" = "Year Month",
                    "Quarterly" = "Year Quarter",
                    "Yearly" = "Decimal Year"
                )

                v <- colnames(.dataset)[sapply(.dataset, iNZightTools::is_num) & !sapply(.dataset, iNZightTools::is_dt)]
                res <- iNZightTools::aggregate_dt(
                    .dataset,
                    svalue(dt_var),
                    part,
                    NULL,
                    tolower(svalue(method)),
                    v
                )

            } else {
                v <- colnames(.dataset)[sapply(.dataset, iNZightTools::is_num) & !sapply(.dataset, iNZightTools::is_dt)]
                dt_name <- sprintf("%s.%s", svalue(dt_var), type)
                res <- .dataset |>
                    dplyr::mutate(
                        !!rlang::sym(dt_name) := (!!getFromNamespace(type, "tsibble"))(!!rlang::sym(svalue(dt_var)))
                    ) |>
                    iNZightTools::aggregate_data(
                        dt_name,
                        tolower(svalue(method)),
                        v
                    )
            }
            for (i in seq_along(colnames(res))) {
                if (isTRUE(all.equal(res[[i]], rep(0, length(res[[i]])))))
                    res[i] <- NULL
            }

            if (preview) {
                df_prev$set_items(res)
            } else {
                GUI$new_document(data = res, suffix = "aggregated")
                close()
            }
        },
        updateView = function() {
            df <- aggregate()
            if (length(df) != 0)
                newview$set_items(df)
        }
    )
)

iNZDataReportWin <- setRefClass(
    "iNZDataReportWin",
    fields = list(
        output_format = "ANY",
        file_path = "ANY",
        file_ext = "ANY"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui) {
            if (!requireNamespace("dataMaid", quietly = TRUE)) {
                gmessage("Unable to do that ... missing dependencies.")
                return()
            }

            ok <- callSuper(gui,
                title = "Generate Data Report",
                width = "small",
                height = "small",
                ok = "Generate",
                action = .self$generate_report,
                show_code = FALSE,
                scroll = FALSE
            )
            if (!ok) return()
            on.exit(.self$show())
            usingMethods("generate_report")

            tbl <- glayout()
            add_body(tbl)
            ii <- 1L

            lbl <- glabel("Report format :")
            font(lbl) <- list(weight = "bold")
            output_format <<- gcombobox(
                c("PDF", "Word Document", "HTML"),
                selected = 3L,
                handler = function(h, ...) {
                    # if (svalue(h$obj) != "HTML") {
                    #     gmessage("Not yet supported", type = "warning")
                    #     h$obj$set_index(3L)
                    # }

                    # file_ext <<- switch(
                    #     svalue(h$obj),
                    #     "PDF" = ".pdf",
                    #     "Word Document" = ".docx",
                    #     "HTML" = ".html"
                    # )
                    file_path <<- tempfile(fileext = ".Rmd")
                }
            )
            tbl[ii, 1L, anchor = c(1, 0), fill = TRUE] <- lbl
            tbl[ii, 2:4, expand = TRUE] <- output_format
            ii <- ii + 1L

            # invoke change to set file_ext, file_path (DRY)
            output_format$invoke_change_handler()

            # lbl <- glabel("File name :")
            # font(lbl) <- list(weight = "bold")
            # file_path <<- gfilebrowse(
            #     initial.filename = sprintf("%s.%s"),
            #     type = "save",
            #     filter = c("PDF" = "pdf", "Word Document" = "docx", "HTML" = "html"),
            #     handler = function(h, ...) {
            #         print(svalue(h$obj))
            #     }
            # )
            # tbl[ii, 1, anchor = c(1, 0), fill = TRUE] <- lbl
            # tbl[ii, 2:4, expand = TRUE] <- file_path
            # ii <- ii + 1

            show()
        },
        generate_report = function() {
            success <- FALSE
            tryCatch(
                {
                    dataMaid::makeDataReport(
                        GUI$getActiveData(lazy = FALSE),
                        output = switch(svalue(output_format),
                            "PDF" = "pdf",
                            "Word Document" = "word",
                            "HTML" = "html"
                        ),
                        file = file_path,
                        reportTitle = GUI$dataNameWidget$datName,
                        replace = TRUE
                    )
                    success <- TRUE
                },
                error = function(e) {
                    gmessage("Unable to generate report :(", type = "error")
                    print(e)
                }
            )

            if (success) close()
        }
    )
)
