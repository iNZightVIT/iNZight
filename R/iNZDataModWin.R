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
            vnames <- names(GUI$getActiveData())
            iNZightTools::make_names(vars, vnames)
        },
        ## this checks names exist; returns TRUE if everything is OK
        checkNames = function(var) {
            if (any(w <- var %in% names(GUI$getActiveData()))) {
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

            data <- GUI$getActiveData()
            numvars <- names(data)[sapply(data, iNZightTools::is_num)]
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
                .dataset <- GUI$get_data_object()
                newdata <- iNZightTools::convertToCat(.dataset, orgVar, name)
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
            initFields(data = GUI$getActiveData())

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
            .dataset <- GUI$get_data_object()
            newdata <- iNZightTools::transformVar(.dataset, var, fn, vname)
            updateData(newdata)

            data <<- GUI$getActiveData()
            numvars <- names(data)[sapply(data, iNZightTools::is_num)]
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
            factorIndices <- sapply(GUI$getActiveData(), is_cat)
            factor_menu <<- gcombobox(
                names(GUI$getActiveData())[factorIndices],
                selected = 0
            )
            addHandlerChanged(factor_menu,
                handler = function(h, ...) {
                    factor_levels[] <<-
                        levels(GUI$getActiveData()[svalue(factor_menu)][[1L]])
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

            if (lvlname %in% levels(GUI$getActiveData()[[var]]) &&
                !lvlname %in% lvls) {
                ## checking that the new level name isn't one of the other
                ## level names (excluding those being collapsed)
                gmessage("That level name already exists. Please choose another.",
                    title = "Invalid level name",
                    parent = GUI$modWin,
                    icon = "warning")
            } else if (checkNames(name)) {
                .dataset <- GUI$get_data_object()
                data <- iNZightTools::collapseLevels(.dataset, var, lvls, lvlname, name)
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
            factorIndices <- sapply(GUI$getActiveData(), is_cat)
            factor_menu <<- gcombobox(
                names(GUI$getActiveData())[factorIndices],
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

            var <- GUI$getActiveData()[[svalue(factor_menu)]]
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

            var <- GUI$getActiveData()[[svalue(factor_menu)]]
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

            .dataset <- GUI$get_data_object()
            data <- iNZightTools::renameLevels(.dataset, var, newlvls, name)
            updateData(data)
            close()
        }
    )
)

## reorder factor levels
iNZReorderWin <- setRefClass(
    "iNZReorderWin",
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
                title = "Standardise variables",
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
            factorIndices <- sapply(GUI$getActiveData(), is_cat)
            factorMenu <<- gcombobox(
                names(GUI$getActiveData())[factorIndices],
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
                            Levels = levels(GUI$getActiveData()[, svalue(factorMenu)]),
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
            .dataset <- GUI$get_data_object()

            if (!checkNames(varname)) return()
            if (svalue(sortMenu, TRUE) == 1) {
                data <- iNZightTools::reorderLevels(.dataset, var,
                    freq = TRUE, name = varname)
            } else {
                levels <- as.character(levelOrder$get_items())
                data <- iNZightTools::reorderLevels(.dataset, var,
                    levels, name = varname)
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
            factorIndices <- sapply(GUI$getActiveData(), is_cat)
            factorNames <<- gtable(
                list("Categorical Variables" = names(GUI$getActiveData())[factorIndices]),
                multiple = TRUE,
                expand = TRUE
            )
            newName <<- gedit()

            ## separator (. or _ for now ...)
            lbl4 <- glabel("Value separator")
            varSep <<- gcombobox(c("_", "."), selected = 1)
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

            .dataset <- GUI$get_data_object()
            data <- iNZightTools::combineCatVars(.dataset, vars, sep, name)
            updateData(data)
            close()
        }
    )
)

## create new variables using an expression
iNZcrteVarWin <- setRefClass(
    "iNZcrteVarWin",
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Create New Variables"
            size(GUI$modWin) <<- c(450, 200)

            mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
            mainGroup$set_borderwidth(15)

            lbl1 <- glabel("Type in an expression to compute a new variable")
            font(lbl1) <- list(weight = "bold", family = "sans")
            helpbtn <- gimagebutton(stock.id = "gw-help",
                handler = function(h, ...) help_page("user_guides/variables/#create")
            )
            titlelyt <- glayout(homegenous = FALSE)
            titlelyt[1L, 4:19, expand = TRUE, anchor = c(0, 0)] <- lbl1
            titlelyt[1L, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn

            lbl2 <- glabel("Examples")
            font(lbl2) <- list(weight = "bold", family = "sans")
            newVarName <- gedit("new.variable", width = 15) ## name of the new variable
            newVarExp <- gedit("  ") ## expression used to create new var
            submitButton <- gbutton("Submit",
                handler = function(h,...) {
                    expr <- svalue(newVarExp)
                    name <- svalue(newVarName)
                    .dataset <- GUI$get_data_object()
                    data <- try(
                        iNZightTools::createNewVar(.dataset, name, expr),
                        silent = TRUE
                    )

                    if (inherits(data, 'try-error')) {
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
                    dispose(GUI$modWin)
                }
            )

            tbl <- glayout()
            tbl[1L, 2L, anchor = c(-1, 1)] <- "av.height"
            tbl[1L, 3L, anchor = c(-1, 1)] <- " = "
            tbl[1L, 4L, anchor = c(-1, 1), expand = TRUE] <- "(m.height + f.height)/2"
            tbl[2L, 2L, anchor = c(-1, 1)] <- "wgt.diff"
            tbl[2L, 3L, anchor = c(-1, 1)] <- " = "
            tbl[2L, 4L, anchor = c(-1, 1), expand = TRUE] <- "wgt.After - wgt.Before"
            tbl[4L, 2L, anchor = c(-1, 1)] <- newVarName
            tbl[4L, 3L, anchor = c(-1, 1)] <- " = "
            tbl[4L, 4L, anchor = c(-1, 1), expand = TRUE] = newVarExp

            add(mainGroup, titlelyt)
            add(mainGroup, lbl2)
            add(mainGroup, tbl)
            add(mainGroup, submitButton)
            add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)

            visible(GUI$modWin) <<- TRUE
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

            .dataset <- GUI$getActiveData()
            numvars <- names(.dataset)[sapply(.dataset, iNZightTools::is_num)]
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

            data <- GUI$getActiveData()

            break_points <- NULL
            if (svalue(type) == "Manual") {
                if (trimws(svalue(breaks)) == "") return()
                xr <- range(data[[svalue(variable)]], na.rm = TRUE)
                break_points <- as.numeric(strsplit(svalue(breaks), ",")[[1]])
                break_points <- c(xr[1], break_points, xr[2])
            }

            .dataset <- GUI$get_data_object()
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
                format.lowest = svalue(label_lower),
                format.highest = svalue(label_upper),
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
iNZrnmVarWin <- setRefClass(
    "iNZrnmVarWin",
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Rename Variables"
            scrolledWindow <- gtkScrolledWindow()
            scrolledWindow$setPolicy("GTK_POLICY_AUTOMATIC", "GTK_POLICY_AUTOMATIC")

            mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
            mainGroup$set_borderwidth(15)
            helpbtn <- gimagebutton(stock.id = "gw-help",
                container = mainGroup,
                anchor = c(1, -1),
                handler = function(h, ...) help_page("user_guides/variables/#renamevars")
            )

            lbl1 <- glabel("Old Variables")
            lbl2 <- glabel("New Variables")
            oldNames <- names(GUI$getActiveData())

            tbl <- glayout()
            tbl[1L, 1L, expand = TRUE, anchor = c(-1, -1)] <- lbl1
            tbl[1L, 2L, expand = TRUE, anchor = c(-1, -1)] <- lbl2
            invisible(
                sapply(1:length(oldNames),
                    function(pos) {
                        tbl[1L + pos, 1L] <- glabel(oldNames[pos])
                        tbl[1L + pos, 2L] <- gedit(oldNames[pos])
                    }
                )
            )

            renameButton <- gbutton('Rename',
                handler = function(h, ...) {
                    onames <- names(GUI$getActiveData())
                    vnames <- sapply(tbl[-1, 2], svalue)

                    if (any(table(vnames) > 1)) {
                        gmessage("You have duplicated some variable names.",
                            title = "Duplicated variable names",
                            icon = 'warning',
                            parent = GUI$modWin
                        )
                        return()
                    }

                    ## only pass through variables that change
                    w <- vnames != onames
                    namelist <- as.list(vnames[w])
                    names(namelist) <- onames[w]

                    .dataset <- GUI$get_data_object()
                    data <- iNZightTools::renameVars(.dataset, namelist)
                    updateData(data)
                    dispose(GUI$modWin)
                }
            )
            add(mainGroup, tbl)
            add(mainGroup, renameButton)

            ## method of gtkScrolledWindow to add a GtkWidget (not a gWidgets2 class)
            ## as a child using a viewport
            scrolledWindow$addWithViewport(mainGroup$widget)
            add(GUI$modWin, scrolledWindow, expand = TRUE, fill = TRUE)
            size(GUI$modWin) <<- c(500, -1)

            visible(GUI$modWin) <<- TRUE
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
            numIndices <- sapply(GUI$getActiveData(), function(x) !is_cat(x))
            numVar <<- gtable(
                list("Variables" = names(GUI$getActiveData())[numIndices]),
                multiple = TRUE
            )

            add_body(numVar, expand = TRUE)
        },
        standardise = function() {
            if (length(svalue(numVar)) == 0) return()

            varnames <- svalue(numVar)
            names <- makeNames(paste0(varnames, ".std"))
            .dataset <- GUI$get_data_object()
            data <- iNZightTools::standardizeVars(.dataset, varnames, names)
            updateData(data)
            close()
        }
    )
)

## delete variables
iNZdeleteVarWin <- setRefClass(
    "iNZdeleteVarWin",
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Delete Variables"
            mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
            mainGroup$set_borderwidth(15)

            ## instructions through glabels
            lbl1 <- glabel("Select Variables to delete")
            font(lbl1) <- list(weight="bold", family = "sans")

            helpbtn <- gimagebutton(stock.id = "gw-help",
                handler = function(h, ...) help_page("user_guides/variables/#deletevars")
            )

            titlelyt <- glayout(homegenous = FALSE)
            titlelyt[1L, 4:19, expand = TRUE, anchor = c(0, 0)] <- lbl1
            titlelyt[1L, 20L, expand = TRUE, anchor = c(1, -1)] <- helpbtn

            lbl2 <- glabel("(Hold Ctrl to choose many)")
            font(lbl2) <- list(weight="bold", family = "sans")
            listOfVars <- gtable(names(GUI$getActiveData()),
                multiple = TRUE,
                expand = TRUE
            )
            names(listOfVars) <- "Variables"

            deleteButton <- gbutton(
                "Delete",
                handler = function(h,...) {
                    vars <- svalue(listOfVars)
                    if (length(vars) == 0) return()
                    if (length(vars) == length(names(GUI$getActiveData()))) {
                        gmessage(
                            "You can't delete all of the variables ... you'll have nothing left!",
                            title = 'Oops...',
                            icon = 'error',
                            parent = GUI$modWin
                        )
                        return()
                    }
                    confirmDel <- gconfirm(
                        title = "Are you sure?",
                        msg = paste(
                            "You are about to delete the",
                            "following variables:\n",
                            paste(vars, collapse = "\n")
                        ),
                        icon = "question"
                    )

                    if (!confirmDel) return()

                    .dataset <- GUI$get_data_object()
                    data <- iNZightTools::deleteVars(.dataset, vars)
                    updateData(data)
                    dispose(GUI$modWin)
                }
            )

            add(mainGroup, titlelyt)
            add(mainGroup, lbl2)
            add(mainGroup, listOfVars, expand = TRUE)
            add(mainGroup, deleteButton)
            add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)

            visible(GUI$modWin) <<- TRUE
        }
    )
)

## Missing as Cat
iNZmissCatWin <- setRefClass(
    "iNZmissCatWin",
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Missing as Categorical"
            mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
            mainGroup$set_borderwidth(15)

            ## instructions through glabels
            lbl1 <- glabel(
                paste(sep = "\n",
                    "Select Variables to be transformed",
                    "Resulting Variables will be categorical with a level for missing observations"
                )
            )
            font(lbl1) <- list(weight = "bold", family = "sans")

            helpbtn <- gimagebutton(stock.id = "gw-help",
                handler = function(h, ...) help_page("user_guides/variables/#missingcat")
            )

            titlelyt <- glayout(homegenous = FALSE)
            titlelyt[1L, 1:19, expand = TRUE] <- lbl1
            titlelyt[1L, 20L, expand = TRUE, anchor = c(1, -1)] <- helpbtn

            lbl2 <- glabel("(Hold Ctrl to choose many)")
            font(lbl2) <- list(weight = "bold", family = "sans")

            listOfVars <- gtable(names(GUI$getActiveData()),
                multiple = TRUE,
                expand = TRUE
            )
            names(listOfVars) <- "Variables"

            convertButton <- gbutton(
                "Convert",
                handler = function(h,...) {
                    if (length(svalue(listOfVars)) == 0) return()

                    vars <- svalue(listOfVars)
                    names <- makeNames(paste0(vars, "_miss"))

                    .dataset <- GUI$get_data_object()
                    data <- iNZightTools::missingToCat(.dataset, vars, names)
                    updateData(data)
                    dispose(GUI$modWin)
                }
            )

            add(mainGroup, titlelyt)
            add(mainGroup, lbl2)
            add(mainGroup, listOfVars, expand = TRUE)
            add(mainGroup, convertButton)
            add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)

            visible(GUI$modWin) <<- TRUE
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
            numIndices <- sapply(GUI$getActiveData(), function(x) !is_cat(x))
            rank_vars <<- gtable(
                list(Variables = names(GUI$getActiveData())[numIndices]),
                multiple = TRUE
            )

            add_body(rank_vars, expand = TRUE, fill = TRUE)

            visible(GUI$modWin) <<- TRUE
        },
        rank = function() {
            if (length(svalue(rank_vars)) == 0L) return()
            vars <- svalue(rank_vars)
            .dataset <- GUI$get_data_object()
            data <- iNZightTools::rankVars(.dataset, vars)
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
            numIndices <- sapply(GUI$getActiveData(), function(x) !is_cat(x))
            num_vars <<- gtable(
                list(Variables = names(GUI$getActiveData())[numIndices]),
                multiple = TRUE
            )
            add_body(num_vars, expand = TRUE, fill = TRUE)
        },
        convert = function() {
            if (length(svalue(num_vars)) == 0) return()

            vars <- svalue(num_vars)
            varnames <- makeNames(paste(vars, "cat", sep = "."))

            .dataset <- GUI$get_data_object()
            data <- iNZightTools::convertToCat(.dataset, vars, varnames)
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

            curname <- attr(GUI$getActiveData(), "name", exact = TRUE)
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
iNZconTodtWin <- setRefClass(
    "iNZconTodtWin",
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Convert to Dates and Times"

            scrolledWindow <- gtkScrolledWindow()
            scrolledWindow$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")

            mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
            mainGroup$set_borderwidth(15)

            title <- glabel("Convert to a Date-Time variable")
            font(title) <- list(size = 14, weight = "bold")
            helpbtn <- gimagebutton(stock.id = "gw-help",
                handler = function(h, ...)help_page("user_guides/variables/#dtconvert")
            )
            titlelyt <- glayout(homegenous = FALSE)
            titlelyt[1L, 4:19, expand = TRUE, anchor = c(0,0)] <- title
            titlelyt[1L, 20L, expand = TRUE, anchor = c(1, -1)] <- helpbtn
            add(mainGroup, titlelyt)
            addSpace(mainGroup, 5)

            date_string <- glabel("Select variable to convert from",
                container = mainGroup,
                anchor = c(-1, 0)
            )

            var1 <- gcombobox(c("", names(GUI$getActiveData())),
                container = mainGroup,
                handler = function(h, ...) {
                    varname = svalue(var1)
                    if (varname == "") {
                        dfview$set_items("")
                        convertedview$set_items("")
                        svalue(newVarname) = ""
                        return()
                    }

                    varx <- GUI$getActiveData()[[varname]]
                    svalue(newVarname) <-
                        makeNames(paste(varname, ".dt", sep = ""))

                    dfview$set_items(
                        data.frame(Original = varx, stringsAsFactors = TRUE)
                    )

                    if (svalue(var2) == "") return()

                    convname <- svalue(var2)
                    .dataset <- GUI$getActiveData()
                    name <- svalue(newVarname)

                    data <- tryCatch(
                        convertedview$set_items(
                            data.frame(
                                Converted =
                                    iNZightTools::convert_to_datetime(
                                        .dataset,
                                        varname,
                                        convname,
                                        name
                                        )[[svalue(newVarname)]],
                                stringsAsFactors = TRUE
                            )
                        ),
                        warning = function(w) {
                            if (w$message == "Failed to parse") {
                                convertedview$set_items(
                                    data.frame(
                                        Converted = "Invalid format",
                                        stringsAsFactors = TRUE
                                    )
                                )
                            } else {
                                convertedview$set_items(
                                    data.frame(
                                        Converted = w$message,
                                        stringsAsFactors = TRUE
                                    )
                                )
                            }
                        }
                    )
                }
            )

            factorsbox <- gvbox(container = mainGroup)
            factors <- gtable(names(GUI$getActiveData()),
                multiple = TRUE,
                expand = TRUE,
                container = factorsbox
            )
            names(factors) <- "Variables"

            addHandlerSelectionChanged(factors,
                function(h, ...) {
                    factorname <- svalue(factors)
                    varx <- ""
                    new_name <- ""

                    for (num in 1:length(factorname)) {
                        name <- factorname[num]
                        varx <- paste(varx, GUI$getActiveData()[[name]])
                        new_name <- paste(new_name, name, ".", sep = "")
                    }

                    dfview$set_items(
                        data.frame(Original = varx, stringsAsFactors = TRUE)
                    )
                    svalue(newVarname) <- makeNames(paste(new_name, "dt", sep = ""))

                    if (svalue(var2) == "") return()

                    convname <- svalue(var2)
                    .dataset <- GUI$getActiveData()
                    name <- svalue(newVarname)

                    data <- tryCatch(
                        convertedview$set_items(
                            data.frame(
                                Converted =
                                    iNZightTools::convert_to_datetime(
                                        .dataset,
                                        factorname,
                                        convname,
                                        name
                                        )[[svalue(newVarname)]],
                                stringsAsFactors = TRUE
                            )
                        ),
                        warning = function(w) {
                            if (w$message == "Failed to parse") {
                                convertedview$set_items(
                                    data.frame(
                                        Converted = "Invalid format",
                                        stringsAsFactors = TRUE
                                    )
                                )
                            } else {
                                convertedview$set_items(
                                    data.frame(
                                        Converted = w$message,
                                        stringsAsFactors = TRUE
                                    )
                                )
                            }
                        }
                    )
                }
            )
            visible(factorsbox) <- FALSE
            size(factorsbox) <- c(-1, 250)

            checkbox <- gcheckbox("Click to use multiple variables",
                container = mainGroup,
                handler = function(h, ...) {
                    if (svalue(checkbox) == TRUE) {
                        visible(factorsbox) <- TRUE
                        visible(var1) <- FALSE
                        date_string$set_value(
                            "Select variables to convert from \n(Use CNTRL to add/remove)"
                        )
                    } else {
                        visible(factorsbox) <- FALSE
                        visible(var1) <- TRUE
                        date_string$set_value("Select variable to convert from")
                    }
                }
            )

            addSpace(mainGroup, 5)

            name_string <- glabel("Name for the new variable",
                container = mainGroup,
                anchor = c(-1, 0)
            )
            newVarname <- gedit("", container = mainGroup)

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

            for.var <- glabel(
                "Select the order format of your data",
                container = mainGroup,
                anchor = c(-1, 0)
            )

            var2 <- gcombobox(
                items = dt.formats,
                container = mainGroup,
                editable = TRUE,
                handler = function(h,...) {
                    if (svalue(checkbox) == TRUE) {
                        factorname <- svalue(factors)
                    } else {
                        factorname <- svalue(var1)
                    }

                    convname <- svalue(var2)
                    if (length(factorname) != 0 & convname == "") {
                        convertedview$set_items("")
                    } else {
                        .dataset <- UI$getActiveData()
                        name <- svalue(newVarname)
                        data <- tryCatch(
                            convertedview$set_items(
                                data.frame(
                                    Converted =
                                        iNZightTools::convert_to_datetime(
                                            .dataset,
                                            factorname,
                                            convname,
                                            name
                                            )[[svalue(newVarname)]],
                                    stringsAsFactors = TRUE
                                )
                            ),
                            warning = function(w) {
                                if (w$message == "Failed to parse") {
                                    convertedview$set_items(
                                        data.frame(
                                            Converted = "Invalid format",
                                            stringsAsFactors = TRUE
                                        )
                                    )
                                } else {
                                    convertedview$set_items(
                                        data.frame(
                                            Converted = w$message,
                                            stringsAsFactors = TRUE
                                        )
                                    )
                                }
                            }
                        )
                    }
                }
            )

            g2 <- gexpandgroup(
                container = mainGroup,
                text = "Advanced selection"
            )
            visible(g2) <- FALSE

            tbl <- glayout(container = g2, expand = TRUE)
            tbl[1L, 1L] <- gbutton("year",
                handler = function(h, ...)
                    svalue(var2) = paste(svalue(var2), svalue(h$obj))
            )
            tbl[1L, 2L] <- gbutton("month",
                handler = function(h, ...)
                    svalue(var2) = paste(svalue(var2), svalue(h$obj))
            )
            tbl[1L, 3L] <- gbutton("day",
                handler = function(h, ...)
                    svalue(var2) = paste(svalue(var2), svalue(h$obj))
            )
            tbl[1L, 4L] <- gbutton("pm/am",
                handler = function(h, ...)
                    svalue(var2) = paste(svalue(var2), svalue(h$obj))
            )
            tbl[2L, 1L] <- gbutton("Hour",
                handler = function(h, ...)
                    svalue(var2) = paste(svalue(var2), svalue(h$obj))
            )
            tbl[2L, 2L] <- gbutton("Minute",
                handler = function(h, ...)
                    svalue(var2) = paste(svalue(var2), svalue(h$obj))
            )
            tbl[2L, 3L] <- gbutton("Second",
                handler = function(h, ...)
                    svalue(var2) = paste(svalue(var2), svalue(h$obj))
            )
            tbl[3L, 3L] <- gbutton("delete",
                handler = function(h, ...) {
                    number_of_space <-
                        nchar(svalue(var2)) - nchar(gsub(" ", "", svalue(var2)))

                    if (number_of_space == 1 | svalue(var2) == "") {
                        svalue(var2) <- ""
                    } else {
                        svalue(var2) <-
                            stringr::str_match(svalue(var2), "(^.+)\\s")[, 2]
                    }
                }
            )
            tbl[3L, 4L] <- gbutton("clear",
                handler = function(h, ...) svalue(var2) = ""
            )

            g3 <- ggroup(container = mainGroup)
            dfview <- gtable(
                data.frame(
                    Original = "",
                    stringsAsFactors = TRUE
                ),
                container = g3
            )
            size(dfview) <- c(-1, 250)

            convertedview <- gtable(
                data.frame(
                    Converted = "",
                    stringsAsFactors = TRUE
                ),
                container = g3
            )
            size(convertedview) <- c(-1, 250)

            okbtn <- gbutton("Convert",
                container = mainGroup,
                handler = function(h,...) {
                    if (svalue(checkbox) == TRUE) {
                        factorname <- svalue(factors)
                    } else {
                        factorname <- svalue(var1)
                    }

                    convname <- svalue(var2)
                    name <- svalue(newVarname)
                    var.dt <- iNZightTools::convert_to_datetime(
                        GUI$getActiveData(),
                        factorname,
                        convname,
                        name
                    )
                    updateData(var.dt)
                    dispose(GUI$modWin)
                }
            )

            scrolledWindow$addWithViewport(mainGroup$widget)
            add(GUI$modWin, scrolledWindow, expand = TRUE, fill = TRUE)
            size(GUI$modWin) <<- c(300, 600)
            visible(GUI$modWin) <<- TRUE
        }
    )
)


## Extract parts from a datetime variable
iNZExtfromdtWin <- setRefClass(
    "iNZExtfromdtWin",
    contains = "iNZDataModWin",
    fields = list(
        varname = "ANY",
        component = "ANY",
        newname = "ANY",
        extractedview = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Convert to Dates and Times"

            scrolledWindow <- gtkScrolledWindow()
            scrolledWindow$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")

            mainGroup <- gvbox()
            mainGroup$set_borderwidth(15)

            title <- glabel("Extract parts of the datetime")
            font(title) <- list(size = 14, weight = "bold")
            helpbtn <- gimagebutton(stock.id = "gw-help",
                handler = function(h, ...)
                    help_page("user_guides/variables/#dtextract")
            )
            titlelyt <- glayout(homegenous = FALSE)
            titlelyt[1L, 4:19, expand = TRUE, anchor = c(0, 0)] <- title
            titlelyt[1L, 20L, expand = TRUE, anchor = c(1, -1)] <- helpbtn

            add(mainGroup, titlelyt)
            addSpace(mainGroup, 5)

            date_string <- glabel(
                "Select variable to extract information from",
                container = mainGroup,
                anchor = c(-1, 0)
            )

            var1 <- gcombobox(
                items = c(
                    "",
                    c(
                        names(dplyr::select_if(GUI$getActiveData(), lubridate::is.Date)),
                        names(dplyr::select_if(GUI$getActiveData(), lubridate::is.POSIXct))
                    )
                ),
                container = mainGroup,
                handler = function(h,...) {
                    varname <<- svalue(var1)
                    if (varname == "") {
                        dfview$set_items(data.frame(Original = "", stringsAsFactors = TRUE))
                        extractedview$set_items(data.frame(Extracted = "", stringsAsFactors = TRUE))
                    } else {
                        varx = GUI$getActiveData()[[varname]]
                        dfview$set_items(data.frame(Original = as.character(varx), stringsAsFactors = TRUE))
                        if (component == "") return()

                        updatePreview()
                    }
                }
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

            l <- list(
                Date = list(
                    "Date only" = "Date only",
                    Year = list(
                        "Year" = "Year",
                        "Century" = "Century",
                        "Decimal Year" = "Decimal Year"
                    ),
                    Quarter = list(
                        "Year Quarter" = "Year Quarter",
                        "Quarter" = "Quarter"
                    ),
                    Month = list(
                        "Year Month" = "Year Month",
                        "Month (full)" = "Month (full)",
                        "Month (abbreviated)" = "Month (abbreviated)",
                        "Month (number)" = "Month (number)"
                    ),
                    Week = list(
                        "Week of the year (Sunday as first day of the week)" =
                            "Week of the year (Sunday as first day of the week)",
                        "Week of the year (Monday as first day of the week)" =
                            "Week of the year (Monday as first day of the week)"
                    ),
                    Day = list(
                        "Day of the year" = "Day of the year",
                        "Day of the week (name)" = "Day of the week (name)",
                        "Day of the week (abbreviated)" =
                            "Day of the week (abbreviated)",
                        "Day of the week (number, Monday as 1)" =
                            "Day of the week (number, Monday as 1)",
                        "Day of the week (number, Sunday as 0)" =
                            "Day of the week (number, Sunday as 0)",
                        "Day" = "Day"
                    )
                ),
                Time = list(
                    "Time only" = "Time only",
                    "Hours (decimal)" = "Hours (decimal)",
                    "Hour" = "Hour",
                    "Minute" = "Minute",
                    "Second" = "Second"
                )
            )

            atree <- gtree(
                offspring = offspring,
                offspring.data = l,
                container = mainGroup
            )

            component <<- ""
            addHandlerClicked(atree,
                function(h, ...) {
                    component <<- svalue(atree)[length(svalue(atree))]
                    svalue(newVarname) <- makeNames(
                        paste(varname, ".",
                            switch(component,
                                "Date only" = "Date",
                                "Decimal Year" = "Decimal.Year",
                                "Year Quarter" = "Year.Quarter",
                                "Year Month" = "Year.Month",
                                "Month (abbreviated)" = "Month.cat",
                                "Month (full)" = "Month.cat",
                                "Month (number)" = "Month.number",
                                "Week of the year (Sunday as first day of the week)" = "Week.year",
                                "Week of the year (Monday as first day of the week)" = "Week.year",
                                "Day of the year" = "Day.year",
                                "Day of the week (name)" = "Day.week",
                                "Day of the week (abbreviated)" = "Day.week.abbreviated",
                                "Day of the week (number)" = "Day.week.number",
                                "Day of the week (number, Monday as 1)" = "Day.week.number",
                                "Day of the week (number, Sunday as 0)" = "Day.week.number",
                                "Time only" = "Time",
                                "Hours (decimal)" = "Hour.decimal",
                                component
                            ),
                            sep = ""
                        )
                    )
                    newname <<- svalue(newVarname)
                    updatePreview()
                }
            )

            date_string <- glabel("Name for new variable",
                container = mainGroup,
                anchor = c(-1, 0)
            )
            newVarname <- gedit("", container = mainGroup)
            addHandlerKeystroke(newVarname,
                function(h, ...) {
                    newname <<- ifelse(
                        svalue(newVarname) == "",
                        "Extracted",
                        svalue(newVarname)
                    )
                    updatePreview()
                }
            )

            preview_string <- glabel("Preview",
                container = mainGroup,
                anchor = c(-1, 0)
            )

            g2 <- ggroup(container = mainGroup)
            dfview <- gtable(
                data.frame(
                    Original = "",
                    stringsAsFactors = TRUE
                ),
                container = g2
            )
            size(dfview) <- c(-1, 250)
            extractedview <<- gtable(
                data.frame(
                    Extracted = "",
                    stringsAsFactors = TRUE
                ),
                container = g2
            )
            size(extractedview) <<- c(-1, 250)

            okbtn <- gbutton("Extract",
                container = mainGroup,
                handler = function(h,...) {
                    .dataset <- GUI$getActiveData()
                    exp <- iNZightTools::extract_part(.dataset, varname, component, newname)
                    updateData(exp)
                    dispose(GUI$modWin)
                }
            )

            scrolledWindow$addWithViewport(mainGroup$widget)
            add(GUI$modWin, scrolledWindow, expand = TRUE, fill = TRUE)
            size(GUI$modWin) <<- c(300, 700)
            visible(GUI$modWin) <<- TRUE
        },
        updatePreview = function() {
            .dataset <- GUI$getActiveData()
            list <- list(
                "Date only",
                "Year",
                "Century",
                "Decimal Year",
                "Year Quarter",
                "Quarter",
                "Year Month",
                "Month (full)",
                "Month (abbreviated)",
                "Month (number)",
                "Year Week",
                "Week of the year (Monday as first day of the week)",
                "Week of the year (Sunday as first day of the week)",
                "Day of the year",
                "Day of the week (name)",
                "Day of the week (abbreviated)",
                "Day of the week (number, Monday as 1)",
                "Day of the week (number, Sunday as 0)",
                "Day",
                "Time only",
                "Hours (decimal)",
                "Hour",
                "Minute",
                "Second"
            )

            if (component %in% list) {
                d <- iNZightTools::extract_part(.dataset, varname, component, newname)
                extractedview$set_items(
                    data.frame(
                        Extracted = as.character(d[[newname]]),
                        stringsAsFactors = TRUE
                    )
                )
            }
        }
    )
)

## Aggregate datetimes
iNZAggregatedtWin <- setRefClass(
    "iNZAggregatedtWin",
    contains = "iNZDataModWin",
    fields = list(
        GUI = "ANY",
        col = "ANY",
        format = "ANY",
        method = "ANY",
        newview = "ANY",
        var = "ANY",
        key = "ANY",
        name = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Aggregate datetimes to monthly or quarterly"

            mainGroup <- gvbox()
            mainGroup$set_borderwidth(15)

            title <- glabel("Aggregate datetimes to monthly or quarterly")
            font(title) <- list(size = 14, weight = "bold")
            helpbtn <- gimagebutton(stock.id = "gw-help",
                handler = function(h, ...)
                    help_page("user_guides/variables/#dtaggregate")
            )

            titlelyt <- glayout(homegenous = FALSE)
            titlelyt[1L, 4:19, expand = TRUE, anchor = c(0, 0)] <- title
            titlelyt[1L, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn

            add(mainGroup, titlelyt)
            addSpace(mainGroup, 5)

            var1_string <- glabel("Select a column", container = mainGroup)

            col <<- ""
            var1 <- gcombobox(
                items = c("", names(GUI$getActiveData())),
                container = mainGroup
            )
            addHandlerChanged(var1,
                function(h, ...) {
                    col <<- svalue(var1)
                    var <<- GUI$getActiveData()[[col]]
                    newview$set_items("")

                    if (lubridate::is.POSIXct(var) == TRUE |
                        lubridate::is.Date(var) == TRUE) {
                        key <<- "dt"
                        var2$set_items(formatlist)
                    } else if (all(grepl("W", var)) == TRUE) {
                        key <<- "W"
                        var2$set_items(c("", "Quarterly", "Yearly"))
                    } else if (all(grepl("M", var)) == TRUE) {
                        key <<- "M"
                        var2$set_items(c("", "Quarterly", "Yearly"))
                    } else if (all(grepl("Q", var)) == TRUE) {
                        key <<- "Q"
                        var2$set_items(c("", "Yearly"))
                    } else {
                        key <<- ""
                        var2$set_items("")
                        newview$set_items("Selected variable not supported")
                    }
                }
            )

            formatlist <- c("", "Weekly", "Monthly", "Quarterly", "Yearly")

            var2_string <- glabel("Choose format", container = mainGroup)

            format <<- ""
            var2 <- gcombobox(items = formatlist, container = mainGroup)
            addHandlerChanged(var2,
                function(h, ...) {
                    format <<- svalue(var2)

                    if (format == "")
                        newview$set_items()

                    if (format != "" & method != "" & col != "")
                        updateView()
                }
            )

            var3_string <- glabel("How to aggregate", container = mainGroup)

            method <<- ""
            var3 <- gtable(c("Sum", "Mean", "Median"), container = mainGroup)
            size(var3) <- c(-1, 150)
            addHandlerSelectionChanged(var3,
                function(h, ...) {
                    method <<- svalue(var3)
                    updateView()
                }
            )

            name <<- "newcol"

            prevTbl <- glayout(homogeneous = FALSE, container = mainGroup)

            string1 <- glabel("Original dataset")
            originview <- gtable(
                data.frame(GUI$getActiveData(), stringsAsFactors = TRUE)
            )
            prevTbl[1L, 1L, expand = TRUE] <- string1
            prevTbl[2L, 1L, expand = TRUE] <- originview
            size(originview) = c(-1, 250)

            string2 <- glabel("New dataset")
            newview <<- gtable(data.frame("", stringsAsFactors = TRUE))
            prevTbl[1L, 2L, expand = TRUE] <- string2
            prevTbl[2L, 2L, expand = TRUE] <- newview
            size(newview) <<- c(-1, 250)

            aggregatebtn <- gbutton("Aggregate",
                container = mainGroup,
                handler = function(h,...) {
                    .dataset <- GUI$getActiveData()
                    data <- aggregate()

                    attr(data, "name") <-
                        paste(
                            attr(.dataset, "name", exact = TRUE),
                            "aggregated",
                            sep = "."
                        )
                    attr(data, "code") <-
                        gsub(".dataset",
                            attr(.dataset, "name", exact = TRUE),
                            attr(data, "code")
                        )

                    GUI$setDocument(iNZDocument$new(data = data))
                    dispose(GUI$modWin)
                }
            )

            add(GUI$modWin, mainGroup, expand = TRUE)
            visible(GUI$modWin) <<- TRUE
        },
        aggregate = function() {
            .dataset <- GUI$getActiveData()
            if (key == "dt" & format != "") {
                part <- switch(format,
                    "Weekly" = "Year Week",
                    "Monthly" = "Year Month",
                    "Quarterly" = "Year Quarter",
                    "Yearly" = "Year"
                )

                df <- iNZightTools::extract_part(.dataset, col, part, format)
                df <- iNZightTools::aggregateData(df, format, method)

                colname <- subset(colnames(df),
                    grepl("[:.:]missing$",colnames(df))
                )

                for (i in 1:length(colname)) {
                    if (all(df[[colname[i]]] == 0))
                        df[, colname[i]] <- NULL
                }
                return(df)
            } else if (key != "" & key != "dt" & format != "") {
                newdata <- iNZightTools::separate(.dataset, col,
                    "left", "right", key, "Column")
                df <- iNZightTools::aggregatedt(newdata, format, key, format)
                df <- iNZightTools::aggregateData(df, format, method)

                colname = subset(colnames(df), grepl("[:.:]missing$",colnames(df)))
                for (i in 1:length(colname)) {
                    if (all(df[[colname[i]]] == 0))
                        df[, colname[i]] <- NULL
                }
                return(df)
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
                        GUI$getActiveData(),
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
