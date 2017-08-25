## --------------------------------------------
## The super class for the data modification window
## When a new data modification window is opened,
## a current one is closed if it exists
## List:
## iNZconToCatWin: Convert variables to a categorical type
## iNZtrnsWin: transform variables using various functions
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
## -------------------------------------------
iNZDataModWin <- setRefClass(
    "iNZDataModWin",
    fields = list(
        GUI = "ANY"
        ),
    methods = list(
        initialize = function(gui = NULL) {
            initFields(GUI = gui)
            usingMethods(insertData)
            if (!is.null(GUI)) {
               try(dispose(GUI$modWin), silent = TRUE) ## close any current mod windows
               GUI$modWin <<- gwindow(parent = GUI$win,
                                      visible = FALSE)
            }
        },
        ## insert a column with a certain name at specified index
        ## success msg is optional
        insertData = function(data, name, index, msg = NULL, closeAfter = TRUE, code = NULL) {
            ## insert the new variable in the column after the old variable
            ## or at the end if the old variable is the last column in the
            ## data
            if (index != length(names(GUI$getActiveData()))) {
                newData <- data.frame(
                    GUI$getActiveData()[, 1:index],
                    data,
                    GUI$getActiveData()[, (index+1):ncol(GUI$getActiveData())]
                )
                newNames <- c(
                    names(GUI$getActiveData())[1:index],
                    name,
                    names(GUI$getActiveData())[(index+1):ncol(GUI$getActiveData())]
                )
                newNames <- make.names(newNames, unique = TRUE)
                names(newData) <- newNames
            } else {
                newData <- data.frame(GUI$getActiveData(), data)
                names(newData) <- make.names(c(names(GUI$getActiveData()),
                                               name), unique = TRUE)
            }
            
            if (!is.null(msg))
                do.call(gmessage, msg)

            ## round-about way...
            attr(newData, "code") <- code
            print(code)
            
            GUI$getActiveDoc()$getModel()$updateData(newData)
            if (closeAfter)
                dispose(GUI$modWin)
        },
        updateData = function(newdata) {
            GUI$getActiveDoc()$getModel()$updateData(newdata)
        })
    )

## Convert variables to a categorical type
iNZconToCatWin <- setRefClass(
    "iNZconToCatWin",
    contains = "iNZDataModWin",
    fields = list(
        varData = "ANY" ## data that is dragged into droptarget
        ),
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Convert to Categorical"
            size(GUI$modWin) <<- c(200, 250)
            mainGroup <- ggroup(horizontal = FALSE)
            mainGroup$set_borderwidth(15)
            lbl1 <- glabel("1. Drag and drop a variable name onto the\nlabel below  to create a categorical version\nof that variable")
            font(lbl1) <- list(weight="bold", family = "normal")
            dropLbl <- glabel("DROP VARIABLE HERE")
            font(dropLbl) <- list(size = 14)
            lbl2 <- glabel("2. Type name for the new variable: ")
            font(lbl2) <- list(weight="bold", family = "normal")
            name.txt <- gedit("N/A", width = 20)
            okButton <- gbutton("Update Data",
                                handler = function(h, ...) {
                                    convert(svalue(name.txt), svalue(dropLbl))
                                })
            font(okButton) = list(weight="bold", family = "normal")
            tbl <- glayout(container = mainGroup)
            tbl[1, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl1
            tbl[2, 1] <- gseparator()
            tbl[3, 1] <- dropLbl
            tbl[4, 1] <- gseparator()
            tbl[5, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl2
            tbl[6, 1] <- name.txt
            tbl[7, 1] <- okButton
            addDropTarget(dropLbl,
                          handler = function(h, ...) {
                              dropData <- GUI$getActiveDoc()$getData()[h$dropdata][[1]]
                              if (all(is.factor(dropData)))
                                  gmessage("Already a categorical variable!",
                                           parent = GUI$win)
                              else {
                                  svalue(h$obj) <- h$dropdata
                                  svalue(name.txt) <- paste(h$dropdata, ".cat",
                                                            sep = "")
                                  varData <<- dropData
                              }
                          })
            add(mainGroup, tbl)
            add(GUI$modWin, mainGroup, expand = TRUE)
            visible(GUI$modWin) <<- TRUE
        },
        ## convert the variable with name 'orgVar' to a factor
        ## with name 'name' and insert into data
        convert = function(name, orgVar) {
            if (name == "" || !is.character(name))
                gmessage("Please choose a non-empty name for the new variable")
            else {
                name <- gsub('\\n+', "", name, perl = TRUE)
                .dataset <- GUI$getActiveData()
                data <- iNZightTools::numToCat(.dataset, orgVar, name)
                


                ## exp <- as.formula(sprintf(
                ##     "~.DATANAME %%>%% tibble::add_column(%s = as.factor(%s), .after = varname)",
                ##     name, paste0(".DATANAME$", orgVar)))

                ## data <- interpolate(exp, var = varData, varname = orgVar)
                
                updateData(data)
                
                #insertData(out, name, index, closeAfter = FALSE,
                #           code = sprintf("newdata # MODIFY: data$%s <- as.factor(data$%s)", name, orgVar))
            }
        })
    )

## transform variables using various functions
iNZtrnsWin <- setRefClass(
    "iNZtrnsWin",
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            ## need to specify the methods that we want to use in
            ## do.call later on
            usingMethods(trnsfrm, sqr, recip)
            svalue(GUI$modWin) <<- "Transform Variables"
            mainGroup <- ggroup(horizontal = FALSE)
            mainGroup$set_borderwidth(15)
            lbl1 <- glabel("Drag and drop variable names onto the labels below\nto create new transformed variables")
            font(lbl1) <- list(weight="bold", family = "normal", size = 11)
            lnLbl <- glabel("LOG (e)")
            font(lnLbl) <- list(weight="bold", family = "normal",
                                size = 14, color = "navy")
            logLbl <- glabel("LOG (10)")
            font(logLbl) <- list(weight="bold", family = "normal",
                                 size = 14, color = "navy")
            expLbl <- glabel("EXPONENTIAL")
            font(expLbl) <- list(weight="bold", family = "normal",
                                 size = 14, color = "navy")
            sqrLbl <- glabel("SQUARE")
            font(sqrLbl) <- list(weight="bold", family = "normal",
                                 size = 14, color = "navy")
            rootLbl <- glabel("SQUARE ROOT")
            font(rootLbl) <- list(weight="bold", family = "normal",
                                  size = 14, color = "navy")
            recLbl <- glabel("RECIPROCAL")
            font(recLbl) <- list(weight="bold", family = "normal",
                                 size = 14, color = "navy")
            addTransformation(lnLbl, "log", "log.e")
            addTransformation(logLbl, "log10", "log.10")
            addTransformation(expLbl, "exp", "exp")
            addTransformation(sqrLbl, "sqr", "sqr")
            addTransformation(rootLbl, "sqrt", "root")
            addTransformation(recLbl, "recip", "recip")

            tbl <- glayout(container = mainGroup)
            tbl[1, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl1
            tbl[2, 1] <- gseparator()
            tbl[3, 1, expand = TRUE, anchor = c(0, 0)] <- lnLbl
            tbl[4, 1, expand = TRUE, anchor = c(0, 0)] <- logLbl
            tbl[5, 1, expand = TRUE, anchor = c(0, 0)] <- expLbl
            tbl[6, 1, expand = TRUE, anchor = c(0, 0)] <- sqrLbl
            tbl[7, 1, expand = TRUE, anchor = c(0, 0)] <- rootLbl
            tbl[8, 1, expand = TRUE, anchor = c(0, 0)] <- recLbl
            add(mainGroup, tbl)
            add(GUI$modWin, mainGroup, expand = TRUE)
            visible(GUI$modWin) <<- TRUE
        },
        ## check whether the data is illegible for transformation
        checkData = function(varData) {
            if (all(is.factor(varData))) {
                gmessage(title = "ERROR",
                         msg = "Categorical variables cannot be transformed",
                         parent = GUI$modWin)
                FALSE
            } else
                TRUE
        },
        ## add a drop target to a transformation field
        ## obj: the glabel object to be used as dropTarget
        ## fn: the function used for transformation
        ## nameAdd: the string that is pasted on to the var name
        addTransformation = function(obj, fn, nameAdd) {
            addDropTarget(obj, handler = function(h, ...) {
                dropData <- GUI$getActiveDoc()$getData()[h$dropdata][[1]]
                ## check whether we can transform this variable
                if (checkData(dropData)) {
                    trnsform <- do.call(trnsfrm, list(varData = dropData,
                                                      varName = h$dropdata,
                                                      trnsFn = fn,
                                                      trnsName = nameAdd))
                    do.call(insertData, modifyList(trnsform, list(
                        index = which(names(GUI$getActiveData()) == h$dropdata),
                        closeAfter = FALSE))
                            )
                }
            })
        },
        ## transform a vector and its name
        ## varData: the vector to be transformed
        ## varName: the original name of the column
        ## trnsFn: the function used for transformation
        ## trnsName: the string added on to the varName
        trnsfrm = function(varData, varName, trnsFn, trnsName) {
            out <- round(do.call(trnsFn, list(varData)), 3)
            name <- paste(trnsName, ".", varName, sep = "")
            list(data = out, name = name)
        },
        sqr = function(x) {
            x^2
        },
        recip = function(x) {
            1/x
        })
    )

## collapse multiple factor levels into one
iNZcllpsWin <- setRefClass(
    "iNZcllpsWin",
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Collapse Levels"
            size(GUI$modWin) <<- c(100, 100)
            mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
            mainGroup$set_borderwidth(15)
            ## instructions through glabels
            lbl1 <- glabel("Choose a variable")
            font(lbl1) <- list(weight = "bold",
                               family = "normal")
            lbl2 <- glabel("Choose two or more levels")
            font(lbl2) <- list(weight = "bold",
                               family = "normal")
            lbl3 <- glabel("(Hold Ctrl to choose many)")
            font(lbl3) <- list(weight = "bold",
                               family = "normal")
            ## choose a factor column from the dataset and display
            ## its level in a gtable
            factorIndices <- sapply(GUI$getActiveData(), is.factor)
            factorMenu <- gcombobox(names(GUI$getActiveData())[factorIndices],
                                    selected = 0)
            addHandlerChanged(factorMenu, handler = function(h, ...) {
                factorLvls[] <- levels(GUI$getActiveData()[svalue(factorMenu)][[1]])
            })
            factorLvls <- gtable("", multiple = TRUE, expand = TRUE)
            names(factorLvls) <- "Levels"
            cllpsButton <- gbutton(
                " - COLLAPSE -",
                handler = function(h, ...) {
                    if (checkLevels(svalue(factorLvls))) {
                        cnf <- gconfirm(
                            parent = GUI$modWin,
                            msg = paste("Collapse the levels\n",
                                paste(svalue(factorLvls), collapse = " + "),
                                "\ninto one level with the name\n'",
                                paste(svalue(factorLvls), collapse = "_"),
                                "'?")
                            )
                        if (cnf) {
                            insertData(
                                data = collapse(GUI$getActiveData()[
                                    svalue(factorMenu)][[1]],
                                    svalue(factorLvls)),
                                name = paste(
                                    svalue(factorMenu),
                                    ".coll", sep = ""),
                                index = which(names(
                                    GUI$getActiveData()) == svalue(factorMenu)),
                                closeAfter = TRUE)
                        }
                    }
                })
            add(mainGroup, lbl1)
            add(mainGroup, factorMenu)
            add(mainGroup, lbl2)
            add(mainGroup, lbl3)
            add(mainGroup, factorLvls, expand = TRUE)
            add(mainGroup, cllpsButton)
            add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)
            visible(GUI$modWin) <<- TRUE
        },
        ## check whether the specified levels are illegible
        ## for collapsing
        checkLevels = function(levels) {
            if (is.null(levels) || length(levels) < 2) {
                gmessage(title = "ALERT",
                         icon = "warning",
                         msg = "Need to select at least two levels to collapse",
                         parent = GUI$modWin)
                FALSE
            } else
                TRUE
        },
        ## collapse two or more levels into one, return the new factor
        ## varData: the original vector
        ## levels: the levels to collapse
        collapse = function(varData, levels) {
            newLevel <- paste(levels, collapse = "_")
            newFactor <- as.character(varData)
            newFactor[varData %in% levels] <- newLevel
            newFactor <- as.factor(newFactor)
            newFactor
        })
    )

## rename factor levels
iNZrenameWin <- setRefClass(
    "iNZrenameWin",
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Rename Factor Levels"
            size(GUI$modWin) <<- c(600, 200)
            ## ggroup does not automatically add scrollbars and gWidget2 does not
            ## have a function to do so. We therefore wrap around the RGtk2 class
            ## gtkScrolledWindow around the ggroup
            scrolledWindow <- gtkScrolledWindow()
            ## setting this will only display a scrollbar if necessary
            scrolledWindow$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
            mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
            mainGroup$set_borderwidth(15)
            ## instructions through glabels
            lbl1 <- glabel("Variable to rename:")
            font(lbl1) <- list(weight = "bold",
                               family = "normal")
            lbl2 <- glabel("Name of the new variable:")
            font(lbl2) <- list(weight = "bold",
                               family = "normal")
            ## choose a factor column from the dataset and display
            ## its levels together with their order
            factorIndices <- sapply(GUI$getActiveData(), is.factor)
            factorMenu <- gcombobox(names(GUI$getActiveData())[factorIndices],
                                    selected = 0)
            addHandlerChanged(factorMenu, handler = function(h, ...) {
                svalue(factorName) <- paste(svalue(factorMenu),
                                            ".rename", sep = "")
                displayLevels(tbl,
                              GUI$getActiveData()[svalue(factorMenu)][[1]])
            })
            factorName <- gedit("")
            renameButton <- gbutton("-RENAME-", handler = function(h, ...) {
                newFactor <- changeLevels(
                    tbl,
                    GUI$getActiveData()[svalue(factorMenu)][[1]])
                ## newFactor will be FALSE, if the user input was wrong
                if (newFactor)
                    insertData(data = newFactor,
                               name = svalue(factorName),
                               index = which(names(
                                   GUI$getActiveData()) == svalue(factorMenu)),
                               msg = list(
                                   msg = paste("The new factor can be found under the name '",
                                       svalue(factorName), "'", sep = ""),
                                   icon = "info"),
                               closeAfter = FALSE
                               )
            })
            tbl <- glayout()
            tbl[1, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl1
            tbl[1, 2, expand = TRUE, anchor = c(1, 0)] <- factorMenu
            tbl[2, 1:2, expand = TRUE, anchor = c(-1, 0)] <- lbl2
            tbl[3, 1:2, expand = TRUE] <- factorName
            add(mainGroup, tbl, expand = TRUE)
            add(mainGroup, renameButton)
            ## method of gtkScrolledWindow to add a GtkWidget (not a gWidgets2 class)
            ## as a child using a viewport
            scrolledWindow$addWithViewport(mainGroup$widget)
            add(GUI$modWin, scrolledWindow, expand = TRUE, fill = TRUE)
            visible(GUI$modWin) <<- TRUE
        },
        displayLevels = function(tbl, factorData) {
            ## try to delete currently displayed levels
            ## the first 4 children of tbl refer to the permanent ones
            ## i.e. everything up to and including the gedit to rename
            ## the factor
            if (length(tbl$children) > 4) {
                try(invisible(
                    sapply(tbl$children[5:length(tbl$children)],
                           tbl$remove_child)))
            }

            lbl3 <- glabel("Levels")
            font(lbl3) <- list(weight = "bold",
                               family = "normal")
            lbl4 <- glabel("Order")
            font(lbl4) <- list(weight = "bold",
                               family = "normal")
            tbl[4, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl3
            tbl[4, 2, expand = TRUE, anchor = c(-1, 0)] <- lbl4
            invisible(sapply(levels(factorData), function(x) {
                pos <- which(levels(factorData) == x)
                tbl[4 + pos, 1, expand = TRUE, anchor = c(-1, 0)] <- glabel(x)
                tbl[4 + pos, 2] <- gedit(x)
            }))
        },
        changeLevels = function(tbl, factorData) {
            if (length(tbl$children) < 5) {
                gmessage(msg = "Please choose a factor to reorder",
                         icon = "error",
                         parent = GUI$modWin)
                return(FALSE)
            }
            ## the first 4 children dont refer to the factor levels
            ## each factor lvl has 2 entries in the glayout
            ## the 5th entry refers to the glabels "Levels" and "Order"
            nrLevels <- (length(tbl$children) - 4)/2 - 1
            facLevels <- sapply(tbl[5:(5+nrLevels-1), 1], svalue)
            newFacLevels <- sapply(tbl[5:(5+nrLevels-1), 2], svalue)
            names(facLevels) <- newFacLevels
            ## check if all order numbers are unique
            if (anyDuplicated(newFacLevels) > 0) {
                gmessage(msg = "Please choose unique names for the levels",
                         icon = "error",
                         parent = GUI$modWin)
                FALSE
            }
            else {
                levels(factorData) <- as.list(facLevels)
                factorData
            }
        },
        sortByFreq = function(tbl, factorData) {
            tb <- table(factorData)
            tb <- names(tb[order(tb, decreasing = TRUE)])
            newOrder <- sapply(levels(factorData),
                               function(x) which(x == tb))
            invisible(sapply(1:length(tb),
                             function(i) svalue(tbl[4 + i, 2]) <- newOrder[i]))
        })
    )

## reorder factor levels
iNZreorderWin <- setRefClass(
    "iNZreorderWin",
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Reorder Factor Levels"
            ## ggroup does not automatically add scrollbars and gWidget2 does not
            ## have a function to do so. We therefore wrap the RGtk2 class
            ## gtkScrolledWindow around the ggroup
            scrolledWindow <- gtkScrolledWindow()
            ## setting this will only display a scrollbar if necessary
            scrolledWindow$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
            
            mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
            mainGroup$set_borderwidth(15)
            ## instructions through glabels
            lbl1 <- glabel("Variable to reorder:")
            font(lbl1) <- list(weight = "bold",
                               family = "normal")
            lbl2 <- glabel("Name of the new variable:")
            font(lbl2) <- list(weight = "bold",
                               family = "normal")
            ## choose a factor column from the dataset and display
            ## its levels together with their order
            factorIndices <- sapply(GUI$getActiveData(), is.factor)
            factorMenu <- gcombobox(names(GUI$getActiveData())[factorIndices],
                                    selected = 0)
            addHandlerChanged(factorMenu, handler = function(h, ...) {
                svalue(factorName) <- paste(svalue(factorMenu),
                                            ".reord", sep = "")
                displayLevels(tbl,
                              GUI$getActiveData()[svalue(factorMenu)][[1]])
                ## block the handlers before changing the sortby dropdown
                ## because the signal order is messed up, which causes
                ## the wrong signal to be emitted first
                blockHandlers(sortMenu)
                svalue(sortMenu, index = TRUE) <- 1
                unblockHandlers(sortMenu)
            })
            factorName <- gedit("")
            reorderButton <- gbutton("-REORDER-", handler = function(h, ...) {
                newFactor <- changeLevels(
                    tbl,
                    GUI$getActiveData()[svalue(factorMenu)][[1]])
                ## newFactor will be FALSE, if the user input was wrong
                #if (all(newFactor != FALSE))
                    insertData(data = newFactor,
                               name = svalue(factorName),
                               index = which(names(
                                   GUI$getActiveData()) == svalue(factorMenu)),
                               msg = list(
                                   msg = paste("The new factor can be found under the name '",
                                       svalue(factorName), "'", sep = ""),
                                   icon = "info"),
                               closeAfter = FALSE
                               )
            })
            tbl <- glayout()
            tbl[1, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl1
            tbl[1, 2, expand = TRUE, anchor = c(1, 0)] <- factorMenu
            tbl[2, 1:2, expand = TRUE, anchor = c(-1, 0)] <- lbl2
            tbl[3, 1:2, expand = TRUE] <- factorName
            sortGrp <- ggroup()
            sortLbl <- glabel("Sort by:", cont = sortGrp, expand = TRUE)
            sortMenu <- gcombobox(c("Manual", "Frequency"),
                                  selected = 1, cont = sortGrp, expand = TRUE)
            addHandlerChanged(sortMenu, handler = function(h, ...) {
                if (length(tbl$children) > 4) {
                    if (svalue(sortMenu, index = TRUE) == 2) {
                        sortByFreq(tbl,
                                   GUI$getActiveData()[svalue(factorMenu)][[1]])
                    }
                }
            })
            add(mainGroup, tbl, expand = TRUE)
            add(mainGroup, sortGrp)
            add(mainGroup, reorderButton)
            ## method of gtkScrolledWindow to add a GtkWidget (not a gWidgets2 class)
            ## as a child using a viewport
            scrolledWindow$addWithViewport(mainGroup$widget)
            add(GUI$modWin, scrolledWindow, expand = TRUE, fill = TRUE)
            visible(GUI$modWin) <<- TRUE
        },
        displayLevels = function(tbl, factorData) {
            ## try to delete currently displayed levels
            ## the first 4 children of tbl refer to the permanent ones
            ## i.e. everything up to and including the gedit to rename
            ## the factor
            if (length(tbl$children) > 4) {
                childEntries <- which(sapply(
                    tbl$child_positions, function(child) child$x) > 4)
                try(invisible(
                    sapply(tbl$child_positions[childEntries],
                           function(entry) tbl$remove_child(entry$child)))
                    )
            }

            lbl3 <- glabel("Levels")
            font(lbl3) <- list(weight = "bold",
                               family = "normal")
            lbl4 <- glabel("Order")
            font(lbl4) <- list(weight = "bold",
                               family = "normal")
            tbl[4, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl3
            tbl[4, 2, expand = TRUE, anchor = c(-1, 0)] <- lbl4
            invisible(sapply(levels(factorData), function(x) {
                pos <- which(levels(factorData) == x)
                tbl[4 + pos, 1, expand = TRUE, anchor = c(-1, 0)] <- glabel(x)
                tbl[4 + pos, 2] <- gedit(pos)
            }))
        },
        changeLevels = function(tbl, factorData) {
            if (length(tbl$children) < 5) {
                return(gmessage(msg = "Please choose a factor to reorder",
                         icon = "error",
                         parent = GUI$modWin))
                
            }
            ## the first 4 children dont refer to the factor levels
            ## each factor lvl has 2 entries in the glayout
            ## the 5th entry refers to the glabels "Levels" and "Order"
            childEntries <- which(sapply(
                tbl$child_positions, function(child) child$x) > 4)
            ## gWidets2 reorders the children of glayout in some weird way,
            ## so we find the order by x value, then y value (i.e. as entries
            ## tbl[4,1], tbl[4,2], tbl[5,2], ...)
            origOrder <- order(
                sapply(tbl$child_positions[childEntries], function(child) child$x),
                sapply(tbl$child_positions[childEntries], function(child) child$y))
            facLevels <- sapply(
                tbl$child_positions[childEntries][origOrder[seq(
                    1, length(origOrder), by = 2)]],
                function(child) svalue(child$child))
            facOrder <- sapply(
                tbl$child_positions[childEntries][origOrder[seq(
                    2, length(origOrder), by = 2)]],
                function(child) svalue(child$child))
            facOrder <- as.integer(facOrder)
            facOrder <- sapply(1:length(facLevels), function(x) which(facOrder == x))
            ## check if all order numbers are unique
            if (anyDuplicated(facOrder) > 0) {
                return(gmessage(msg = "Please choose a unique order for the levels",
                         icon = "error",
                         parent = GUI$modWin))
                
            }
            else if (max(facOrder) > length(facOrder)) {
                return(gmessage(msg = "Please remove holes from the order sequence",
                         icon = "error",
                         parent = GUI$modWin))
                
            }
            else {
                newFactor <- factor(factorData, levels = facLevels[facOrder])
                newFactor
            }
        },
        sortByFreq = function(tbl, factorData) {
            tb <- table(factorData)
            tb <- names(tb[order(tb, decreasing = TRUE)])
            newOrder <- sapply(levels(factorData),
                               function(x) which(x == tb))
            invisible(
                sapply(
                    1:length(newOrder),
                    function(i) {
                        xEntries <- sapply(tbl$child_positions,
                                           function(entry) entry$x) == (4 + i)
                        yEntries <- sapply(tbl$child_positions[xEntries],
                                           function(entry) entry$y) == 2
                        svalue(tbl$child_positions[[which(
                            xEntries)[yEntries]]]$child) <- newOrder[i]
                    }
                    ))
        })
    )


## combine categorical variables
iNZcmbCatWin <- setRefClass(
    "iNZcmbCatWin",
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Combine Categorical Variables"
            size(GUI$modWin) <<- c(250, 450)
            mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
            mainGroup$set_borderwidth(15)
            ## instructions through glabels
            lbl1 <- glabel("Choose a variables you want to combine")
            font(lbl1) <- list(weight = "bold",
                               family = "normal")
            lbl2 <- glabel("(Hold Ctrl to choose many)")
            font(lbl2) <- list(weight = "bold",
                               family = "normal")
            lbl3 <- glabel("New Variable Name")
            font(lbl3) <- list(weight = "bold",
                               family = "normal")
            ## choose a factor column from the dataset and display
            ## its level in a gtable
            factorIndices <- sapply(GUI$getActiveData(), is.factor)
            factorNames <- gtable(names(GUI$getActiveData())[factorIndices],
                                  multiple = TRUE, expand = TRUE)
            names(factorNames) <- "Categorical Variables"
            newName <- gedit()
            ## automatically fill the name field when variables are selected
            addHandlerSelectionChanged(factorNames, handler = function(h, ...) {
                if (length(svalue(factorNames)) > 0)
                    svalue(newName) <- paste(svalue(factorNames), collapse = ".")
            })
            cmbButton <- gbutton(
                " - COMBINE-",
                handler = function(h, ...) {
                    if (checkSelection(svalue(factorNames),
                                       svalue(newName))) {
                        cnf <- gconfirm(
                            parent = GUI$modWin,
                            msg = paste("Combine the variables\n",
                                paste(svalue(factorNames), collapse = " + "),
                                "\ninto one variable with the name\n'",
                                svalue(newName),
                                "'?", sep = "")
                            )
                        if (cnf) {
                            insertData(
                                data = combine(svalue(factorNames)),
                                name = svalue(newName),
                                index = ncol(GUI$getActiveData()),
                                msg = list(
                                    msg = "The new factor was added to the end of the data",
                                    icon = "info",
                                    parent = GUI$modWin
                                    ),
                                closeAfter = TRUE)
                        }
                    }
                })
            add(mainGroup, lbl1)
            add(mainGroup, lbl2)
            add(mainGroup, factorNames, expand = TRUE)
            add(mainGroup, lbl3)
            add(mainGroup, newName)
            add(mainGroup, cmbButton)
            add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)
            visible(GUI$modWin) <<- TRUE
        },
        ## check whether the specified variables are illegible
        ## for combining
        checkSelection = function(levels, name) {
            if (is.null(levels) || length(levels) < 2) {
                gmessage(title = "ALERT",
                         icon = "warning",
                         msg = "Need to select at least two variables to combine",
                         parent = GUI$modWin)
                FALSE
            } else if (length(name) == 0) {
                gmessage(title = "ALERT",
                         icon = "warning",
                         msg = "Please specify a non-empty name for the new variable",
                         parent = GUI$modWin)
                FALSE
            } else
                TRUE
        },
        ## combine two or more factors into one, return the new factor
        ## facNames: the original factor names
        combine = function(facNames) {
            factor(do.call(mapply, c(as.list(GUI$getActiveData()[facNames]),
                                    FUN = paste, sep = ".")))
        })
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
            lbl1 = glabel("Type in an expression to compute a new variable")
            font(lbl1) <- list(weight="bold", family = "normal")
            lbl2 = glabel("EXAMPLES")
            font(lbl2) <- list(weight="bold", family = "normal")
            newVarName = gedit("new.variable", width = 15) ## name of the new variable
            newVarExp = gedit("  ") ## expression used to create new var
            submitButton = gbutton(" - SUBMIT -", handler = function(h,...) {
                dataSet <- GUI$getActiveData()
                newValues = try(eval(parse(
                    text = paste("with(dataSet,",
                        gsub(pattern = '\\n+', "", svalue(newVarExp), perl = TRUE),
                        ")"))))
                if(class(newValues)[1] == "try-error")
                    gmessage(title = "ERROR",
                             msg = "Error in expression!",
                             icon = "error", parent = GUI$modWin)
                else {
                    newName = gsub(
                        pattern = '\\n+', "",
                        svalue(newVarName), perl = TRUE)
                    insertData(
                        data = newValues,
                        name = newName,
                        index = ncol(GUI$getActiveData()),
                        msg = list(
                            msg = paste("The new variable",
                                newName,
                                "will be inserted as the last column of the dataset"),
                            icon = "info",
                            parent = GUI$modWin
                            ),
                        closeAfter = TRUE)
                }
            })
            tbl <- glayout()
            tbl[1,2, anchor = c(-1,1)] = "av.height"
            tbl[1,3, anchor = c(-1,1)] = " = "
            tbl[1,4, anchor = c(-1,1), expand = TRUE] = "(m.height + f.height)/2"
            tbl[2,2, anchor = c(-1,1)] = "wgt.diff"
            tbl[2,3, anchor = c(-1,1)] = " = "
            tbl[2,4, anchor = c(-1,1), expand = TRUE] = "wgt.After - wgt.Before"
            tbl[4,2,anchor = c(-1,1)] = newVarName
            tbl[4,3,anchor = c(-1,1)] = " = "
            tbl[4,4, expand = TRUE, anchor = c(-1,1)] = newVarExp
            add(mainGroup, lbl1)
            add(mainGroup, lbl2)
            add(mainGroup, tbl)
            add(mainGroup, submitButton)
            add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)
            visible(GUI$modWin) <<- TRUE
        })
    )


## form class intervals for a numeric variable
iNZfrmIntWin <- setRefClass(
    "iNZfrmIntWin",
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Form Class Intervals"
            size(GUI$modWin) <<- c(400, 400)
            mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
            mainGroup$set_borderwidth(15)
            lbl1 = glabel("Choose variable :")
            font(lbl1) = list(weight = "bold", style = "normal")
            lbl2 = glabel("New variable    :")
            font(lbl2) = list(weight = "bold", style = "normal")
            lbl3 = glabel("New level names :")
            font(lbl3) = list(weight = "bold", style = "normal")
            lbl4 = glabel("Method :")
            font(lbl4) = list(weight = "bold", style = "normal")
            lbl5 = glabel("Number of intervals :")
            font(lbl5) = list(weight = "bold", style = "normal")
            newVarName = gedit("")
            ## choose a numeric column from the dataset
            numIndices <- sapply(GUI$getActiveData(), function(x) !is.factor(x))
            NumericListMenu = gcombobox(names(GUI$getActiveData())[numIndices],
                selected = 0, handler = function(h,...) {
                    svalue(newVarName) = paste(svalue(h$obj),"f", sep = ".")
                })
            binSlider = gslider(from = 2, to = 20, by = 1)
            levelNameChoices = gradio(c("(open left, closed right]", "[closed left, open right)"),
                horizontal = FALSE, selected = 1)
            binningChoices = gradio(c("Equal width intervals",
                "Equal count intervals", "Specified intervals"),
                horizontal = FALSE, selected = 1)
            proceedButton <- gbutton("- Proceed -", handler = function(h, ...) {
                 
              bins <- svalue(binSlider)
              levelLabels <- TRUE
              if (svalue(levelNameChoices) == "[closed left, open right)")
                levelLabels <- FALSE
              
              dataSet <- GUI$getActiveData()
              VarValues <- dataSet[, svalue(NumericListMenu)] 
              if (svalue(binningChoices) == "Equal width intervals")
                newVarValues <- try(cut(VarValues, bins, 
                                        right = levelLabels, include.lowest = TRUE))
              else if (svalue(binningChoices) == "Equal count intervals")
                newVarValues <- try(cut(VarValues, 
                                        quantile(VarValues, probs=seq(0,1,1/bins),na.rm=TRUE), 
                                        include.lowest = TRUE,
                                        right = levelLabels))
              
              else if(svalue(binningChoices) == "Specified intervals"){
                bins <- bins # due to the R lazy evaluation, I active them here.
                VarValues = VarValues
                newVarName = newVarName
                dataSet = dataSet
                levelLabels = levelLabels
                e1 <- environment()
                e1$open <- TRUE
                opt(bins = bins, VarValues = VarValues, 
                    newVarName = newVarName,
                    dataSet = dataSet,
                    levelLabels = levelLabels,
                    env = e1)  # assign the value into opt() environment
                  return()
                
              }
              ####%%%%%%%%%%%%%
              if(class(newVarValues)[1] == "try-error")
                gmessage(title = "ERROR",
                         msg = "Error in cutting intervals!",
                         icon = "error", parent = GUI$modWin)
              else {
                newName = gsub(
                  pattern = '\\n+', "",
                  svalue(newVarName), perl = TRUE)
                insertData(
                  data = newVarValues,
                  name = newName,
                  index = ncol(GUI$getActiveData()),
                  msg = list(
                    msg = paste("The new variable",
                                newName,
                                "will be inserted as the last column of the dataset"),
                    icon = "info",
                    parent = GUI$modWin
                  ),
                  closeAfter = TRUE)
              }
            })
            tbl <- glayout()
            tbl[1, 1] <- lbl1
            tbl[1, 2] <- NumericListMenu
            tbl[2, 1] <- lbl2
            tbl[2, 2] <- newVarName
            tbl[3, 1] <- lbl5
            tbl[4, 1:2] <- binSlider
            tbl[5, 1] <- lbl3
            tbl[5, 2] <- lbl4
            tbl[6, 1] <- levelNameChoices
            tbl[6, 2] <- binningChoices
            tbl[7, 2] <- proceedButton
            add(mainGroup, tbl)
            add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)
            visible(GUI$modWin) <<- TRUE
        }, 
        opt = function(bins, VarValues, newVarName, levelLabels, dataSet, env) {
          # windows for "Specified intervals"
          textboxList =  list()
          breaksNeeded = bins - 1
          
          newVarName # because R is lazy eval, we need to activate here first.
          env # because R is lazy eval, we need to activate here first.
          newBreaks = glayout()
          
          parentmodeWin <- GUI$modWin  # copy the previouos mod windows before we create a new one.
          
          GUI$modWin <<- gwindow("User Intervals", 
                                 parent = GUI$win, width = 150, height = 200)
          #addhandlerunrealize(levelNamesWin, handler = function(h,...){dispose(levelNamesWin)})
          breaksMain = ggroup(horizontal = FALSE, cont = GUI$modWin)
          
          
          lbl1 = glabel(paste("Specified", bins, "intervals.\nNeed", breaksNeeded, "break points"))
          font(lbl1) = list(weight = "bold", style = "normal")
          
          add(breaksMain, lbl1)
          newBreaks[1,2] = glabel(as.character(min(VarValues, na.rm = TRUE)))
          
          
          for(i in 1:breaksNeeded){                                                     #,",width = 60, height = 20 )"
            eval(parse(text=paste(c("textboxList$","lbl",i, "= gtext(\"","\"",",width = 80, height = 20)"), collapse="")))
            newBreaks[i+1,2] = eval(parse(text = paste(c("textboxList$","lbl",i), collapse="")))
          }
          
          
          newBreaks[breaksNeeded+2,2] = glabel(as.character(max(VarValues, na.rm = TRUE)))
          
          
          visible(newBreaks) = TRUE
          add(breaksMain, newBreaks)
          
          out <- NULL
          finalButton = gbutton("Submit Breaks", handler = function(h,...){
            
            cutOffPoints = numeric(0)
            for(i in 1:breaksNeeded)
              cutOffPoints= c(cutOffPoints, gsub(pattern = '\\n+', replacement = "", x = svalue(textboxList[[i]]), perl = TRUE))
            
            
            cutOffPoints = c(min(VarValues, na.rm = TRUE), 
                             gsub(pattern = '\\s+', replacement = "", x = cutOffPoints, perl = TRUE), 
                             max(VarValues, na.rm = TRUE))
            
            x <- NULL
            if(any(cutOffPoints %in% c("", " ", "", "   ", "\n", "\n\n")))
              gmessage(title = "ERROR", message = "Fill in all text boxes", icon = "error", parent = GUI$modWin)
            else if(length(unique(cutOffPoints[c(-1,-length(cutOffPoints))])) != length(cutOffPoints)-2)
              gmessage(title = "ERROR", message = "Breaks must be unique values.", icon = "error", parent = GUI$modWin)
            else{
              
              x <- TRUE
              newVarValues = try(cut(VarValues, cutOffPoints, include.lowest = TRUE, right = levelLabels))
              if(class(newVarValues)[1] == "try-error")
                gmessage(title = "ERROR",
                         msg = "Error in cutting intervals!",
                         icon = "error", parent = GUI$modWin)
              else {
                newName = gsub(
                  pattern = '\\n+', "",
                  svalue(newVarName), perl = TRUE)
                insertData(
                  data = as.factor(newVarValues),
                  name = newName,
                  index = ncol(GUI$getActiveData()),
                  msg = list(
                    msg = paste("The new variable",
                                newName,
                                "will be inserted as the last column of the dataset"),
                    icon = "info",
                    parent = GUI$modWin
                  ),
                  closeAfter = TRUE)
              }
              
            }
            dispose(parentmodeWin)
          }
          )
          add(breaksMain, finalButton)
          
        }
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
            size(GUI$modWin) <<- c(600, 200)
            ## ggroup does not automatically add scrollbars and gWidget2 does not
            ## have a function to do so. We therefore wrap the RGtk2 class
            ## gtkScrolledWindow around the ggroup
            scrolledWindow <- gtkScrolledWindow()
            ## setting this will only display a scrollbar if necessary
            scrolledWindow$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
            mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
            mainGroup$set_borderwidth(15)
            lbl1 <- glabel("Old Variables")
            lbl2 <- glabel("New Variables")
            oldNames <- names(GUI$getActiveData())
            tbl <- glayout()
            tbl[1, 1, expand = TRUE, anchor = c(-1, -1)] <- lbl1
            tbl[1, 2, expand = TRUE, anchor = c(-1, -1)] <- lbl2
            invisible(sapply(1:length(oldNames), function(pos) {
                tbl[1 + pos, 1] <- glabel(oldNames[pos])
                tbl[1 + pos, 2] <- gedit(oldNames[pos])
            }))
            renameButton <- gbutton('- RENAME -',
                                    handler = function(h, ...) {
                newNames <- sapply(tbl[, 2], svalue)
                GUI$getActiveDoc()$getModel()$setNames(newNames[-1])
                dispose(GUI$modWin)
            })
            add(mainGroup, tbl)
            add(mainGroup, renameButton)
            ## method of gtkScrolledWindow to add a GtkWidget (not a gWidgets2 class)
            ## as a child using a viewport
            scrolledWindow$addWithViewport(mainGroup$widget)
            add(GUI$modWin, scrolledWindow, expand = TRUE, fill = TRUE)
            visible(GUI$modWin) <<- TRUE
        })
    )

## standardise variables
iNZstdVarWin <- setRefClass(
    "iNZstdVarWin",
    contains = "iNZDataModWin",
    methods = list(
        initialize = function(gui) {
            callSuper(gui)
            svalue(GUI$modWin) <<- "Standardise Variables"
            size(GUI$modWin) <<- c(250, 450)
            mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
            mainGroup$set_borderwidth(15)
            ## instructions through glabels
            lbl1 <- glabel("Choose a variables you want to standardise")
            font(lbl1) <- list(weight = "bold",
                               family = "normal")
            lbl2 <- glabel("(Hold Ctrl to choose many)")
            font(lbl2) <- list(weight = "bold",
                               family = "normal")
            ## display only numeric variables
            numIndices <- sapply(GUI$getActiveData(), function(x) !is.factor(x))
            numVar <- gtable(names(GUI$getActiveData())[numIndices],
                             multiple = TRUE)
            names(numVar) <- "Variables"
            stdButton <- gbutton("Standardise", handler = function(h, ...) {
                if (length(svalue(numVar)) > 0) {
                    index <- which(numIndices)[svalue(numVar, index = TRUE)]
                    newVar <- scale(GUI$getActiveData()[, index],
                                    center = TRUE, scale = TRUE)
                    newNames <- paste(names(GUI$getActiveData())[index],
                                      ".std", sep = "")
                    insertData(data = newVar,
                               name = newNames,
                               index = ncol(GUI$getActiveData()),
                               msg = list(
                                   msg = "The new variables are added to the end of the dataset",
                                   icon = "info"
                                   ),
                               closeAfter = TRUE)
                }
            })
            add(mainGroup, lbl1)
            add(mainGroup, lbl2)
            add(mainGroup, numVar, expand = TRUE)
            add(mainGroup, stdButton)
            add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)
            visible(GUI$modWin) <<- TRUE
        })
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
            lbl1 = glabel("Select Variables to delete")
            font(lbl1) <- list(weight="bold", family = "normal")
            lbl2 = glabel("(Hold Ctrl to choose many)")
            font(lbl2) <- list(weight="bold", family = "normal")
            listOfVars = gtable(names(GUI$getActiveData()),
                multiple = TRUE, expand = TRUE)
            names(listOfVars) = "Variables"
            deleteButton = gbutton(
                "- Delete -",
                handler = function(h,...) {
                    if (length(svalue(listOfVars)) > 0) {
                        confirmDel <- gconfirm(
                            title = "Are you sure?",
                            msg = paste(
                                "Do you want to delete the",
                                "following variables:\n",
                                paste(svalue(listOfVars),
                                      collapse = "\n")
                                ),
                            icon = "question")
                        if (confirmDel) {
                            dataSet <- GUI$getActiveData()
                            dataSet <- dataSet[, !(names(dataSet) %in%
                                                   svalue(listOfVars)),
                                               drop = FALSE]
                            GUI$getActiveDoc()$getModel()$updateData(dataSet)
                            dispose(GUI$modWin)
                        }
                    }
            })
            add(mainGroup, lbl1)
            add(mainGroup, lbl2)
            add(mainGroup, listOfVars, expand = TRUE)
            add(mainGroup, deleteButton)
            add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)
            visible(GUI$modWin) <<- TRUE
        })
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
            lbl1 = glabel("Select Variables to be transformed\nResulting Variables will be categorical with a level for missing observations")
            font(lbl1) <- list(weight="bold", family = "normal")
            lbl2 = glabel("(Hold Ctrl to choose many)")
            font(lbl2) <- list(weight="bold", family = "normal")
            listOfVars = gtable(names(GUI$getActiveData()),
                multiple = TRUE, expand = TRUE)
            names(listOfVars) = "Variables"
            convertButton = gbutton(
                "- Convert -",
                handler = function(h,...) {
                    if (length(svalue(listOfVars)) > 0) {
                        dataToConvert <- GUI$getActiveData()[, svalue(listOfVars,
                                                                      index = TRUE),
                                                             drop = FALSE]
                        facIndices <- sapply(dataToConvert, is.factor)
                        ## replace NA with 'missing' and 'observed' otherwise
                        ## for non-factors
                        dataToConvert[, !facIndices][!is.na(
                            dataToConvert)[, !facIndices]] <- "observed"
                        dataToConvert[,!facIndices][is.na(
                            dataToConvert)[, !facIndices]] <- "missing"

                        ## replace NA with 'missing' for factors
                        dataToConvert[, facIndices] <- sapply(
                            dataToConvert[, facIndices],
                            function(x) {
                                levels(x) <- c(levels(x), "missing")
                                x[is.na(x)] <- "missing"
                                x
                            })
                        ## convert non-factors to factors
                        dataToConvert[, !facIndices] <- do.call(
                            cbind.data.frame,
                            lapply(dataToConvert[, !facIndices, drop = FALSE],
                                   factor))

                        newNames <- paste(names(
                            GUI$getActiveData())[svalue(listOfVars, index = TRUE)],
                                          "_miss", sep = "")
                        insertData(data = dataToConvert,
                                   name = newNames,
                                   index = ncol(GUI$getActiveData()),
                                   msg = list(
                                       msg = "The new variables are added to the end of the dataset",
                                       icon = "info"
                                       ),
                                   closeAfter = TRUE)
                    }
                })
            add(mainGroup, lbl1)
            add(mainGroup, lbl2)
            add(mainGroup, listOfVars, expand = TRUE)
            add(mainGroup, convertButton)
            add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)
            visible(GUI$modWin) <<- TRUE
        })
    )


# iNZrankNumWin: Rank the numerical variables X (vector, matrix)
iNZrankNumWin <- setRefClass(
  "iNZrankNumWin",
  contains = "iNZDataModWin",
  methods = list(
    initialize = function(gui) {
      callSuper(gui)
      svalue(GUI$modWin) <<- "Ranking Variables"
      size(GUI$modWin) <<- c(250, 450)
      mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
      mainGroup$set_borderwidth(15)
      ## instructions through glabels
      lbl1 <- glabel("Rank the numerical variables X (vector, matrix)")
      font(lbl1) <- list(weight = "bold",
                         family = "normal")
      lbl2 <- glabel("(Hold Ctrl to choose many)")
      font(lbl2) <- list(weight = "bold",
                         family = "normal")
      ## display only numeric variables
      numIndices <- sapply(GUI$getActiveData(), function(x) !is.factor(x))
      numVar <- gtable(names(GUI$getActiveData())[numIndices],
                       multiple = TRUE)
      names(numVar) <- "Variables"
      rankButton <- gbutton("Rank", handler = function(h, ...) {
        if (length(svalue(numVar)) > 0) {
          index <- which(numIndices)[svalue(numVar, index = TRUE)]
          data <- GUI$getActiveData()[, index]
          if (length(index)>1){
          newVar <- sapply(data, rank, ties.method = "min", na.last = "keep")
          }
          else 
            newVar <- rank(data, ties.method = "min", na.last = "keep")
          
          newNames <- paste(names(GUI$getActiveData())[index],
                            ".rank", sep = "")
          
          insertData(data = newVar,
                     name = newNames,
                     index = ncol(GUI$getActiveData()),
                     msg = list(
                       msg = "The new variables are added to the end of the dataset",
                       icon = "info"
                     ),
                     closeAfter = TRUE)        
        }
        else {
          gmessage("Select at leat one variable!",
                   parent = GUI$win)
        }
      })
      add(mainGroup, lbl1)
      add(mainGroup, lbl2)
      add(mainGroup, numVar, expand = TRUE)
      add(mainGroup, rankButton)
      add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)
      visible(GUI$modWin) <<- TRUE
    })
)

## Convert multiple variables to categorical type in the same time
iNZctocatmulWin <- setRefClass(
  "iNZctocatmulWin",
  contains = "iNZDataModWin",
  methods = list(
    initialize = function(gui) {
      callSuper(gui)
      svalue(GUI$modWin) <<- "Convert multiple Variables to categorical type"
      size(GUI$modWin) <<- c(250, 450)
      mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
      mainGroup$set_borderwidth(15)
      ## instructions through glabels
      lbl1 <- glabel("Choose variables you want to convert")
      font(lbl1) <- list(weight = "bold",
                         family = "normal")
      lbl2 <- glabel("(Hold Ctrl to choose many)")
      font(lbl2) <- list(weight = "bold",
                         family = "normal")
      ## display only numeric variables
      numIndices <- sapply(GUI$getActiveData(), function(x) !is.factor(x))
      numVar <- gtable(names(GUI$getActiveData())[numIndices],
                       multiple = TRUE)
      names(numVar) <- "Variables"
      ctmcButton <- gbutton("Convert", handler = function(h, ...) {
        if (length(svalue(numVar)) > 0) {
          index <- which(numIndices)[svalue(numVar, index = TRUE)]
          
          if (length(index) > 1)
            newData <- sapply(GUI$getActiveData()[, index], as.factor)
          else
            newData <- as.factor(GUI$getActiveData()[, index])
          newNames <- paste(names(GUI$getActiveData())[index],
                            ".cat", sep = "")
          insertData(data = newData,
                     name = newNames,
                     index = ncol(GUI$getActiveData()),
                     msg = list(
                       msg = "The new variables are added to the end of the dataset",
                       icon = "info"
                     ),
                     closeAfter = TRUE)
        }
      })
      add(mainGroup, lbl1)
      add(mainGroup, lbl2)
      add(mainGroup, numVar, expand = TRUE)
      add(mainGroup, ctmcButton)
      add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)
      visible(GUI$modWin) <<- TRUE
    })
)
