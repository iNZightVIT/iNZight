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
## iNZrenameDataWin: Rename the dataset
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
          GUI$getActiveData()[, (index+1):ncol(GUI$getActiveData())],
          stringsAsFactors = TRUE
        )
        newNames <- c(
          names(GUI$getActiveData())[1:index],
          name,
          names(GUI$getActiveData())[(index+1):ncol(GUI$getActiveData())]
        )
        newNames <- make.names(newNames, unique = TRUE)
        names(newData) <- newNames
      } else {
        newData <- data.frame(GUI$getActiveData(), data, stringsAsFactors = TRUE)
        names(newData) <- make.names(c(names(GUI$getActiveData()),
                                       name), unique = TRUE)
      }

      if (!is.null(msg))
        do.call(gmessage, msg)

      GUI$getActiveDoc()$getModel()$updateData(newData)
      if (closeAfter)
        dispose(GUI$modWin)
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
          gmessage('A variable with that name already exists. Please choose another one.',
                   title = 'Variable name already exists', icon = 'error',
                   parent = GUI$modWin)
        } else {
          gmessage(paste(sep = "\n",
                         "The follow variable names already exist:",
                         paste(collapse = ", ", var[w]),
                         "Please choose new names."),
                   title = "Variable names already exist", icon = 'error')
        }
        return(FALSE)
      }
      return(TRUE)
    },
    updateData = function(newdata) {
      attr(newdata, "name") <- GUI$getActiveDoc()$getModel()$name
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
      mainGroup <- gvbox()
      mainGroup$set_borderwidth(15)

      helpbtn <- gimagebutton(stock.id = "gw-help", anchor = c(1, -1), cont = mainGroup, handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#convert1")
      })

      tbl <- glayout(container = mainGroup)
      ii <- 1

      lbl <- glabel(paste("1. Drag and drop a variable name onto the",
                          "label below to create a categorical version",
                          "of that variable", sep = "\n"))
      font(lbl) <- list(weight = "bold", family = "sans")

      tbl[ii, 1, anchor = c(-1, 0), expand = TRUE] <- lbl
      ii <- ii + 1

      tbl[ii, 1] <- gseparator()
      ii <- ii + 1

      dropLbl <- glabel("DROP VARIABLE HERE")
      font(dropLbl) <- list(size = 14)
      tbl[ii, 1] <- dropLbl
      ii <- ii + 1

      tbl[ii, 1] <- gseparator()
      ii <- ii + 1

      lbl <- glabel("2. Type name for the new variable: ")
      font(lbl) <- list(weight = "bold", family = "sans")
      tbl[ii, 1, encho = c(-1, 0), expand = TRUE] <- lbl
      ii <- ii + 1

      name.txt <- gedit("No Variable Selected", width = 20)
      tbl[ii, 1] <- name.txt
      ii <- ii + 1

      okButton <- gbutton("Update Data",
                          handler = function(h, ...) {
                            orgVar <- svalue(dropLbl)
                            name <- gsub('\\n+', "", svalue(name.txt), perl = TRUE)
                            if (name == "" || !is.character(name))
                              gmessage("Please choose a non-empty name for the new variable",
                                       title = "Invalid variable choice", parent = GUI$modWin)
                            else if (orgVar == "DROP VARIABLE HERE")
                              gmessage("Please choose a variable to convert",
                                       title = "Invalid variable choice", parent = GUI$modWin)
                            else if (checkNames(name)) {
                              .dataset <- GUI$getActiveData()
                              data <- iNZightTools::convertToCat(.dataset, orgVar, name)
                              updateData(data)
                              svalue(dropLbl) <- "DROP VARIABLE HERE"
                              svalue(name.txt) <- "No Variable Selected"
                            }
                          })
      font(okButton) <- list(weight="bold", family = "sans")
      tbl[ii, 1] <- okButton
      ii <- ii + 1


      addDropTarget(dropLbl,
                    handler = function(h, ...) {
                      dropData <- GUI$getActiveDoc()$getData()[h$dropdata][[1]]
                      if (all(is_cat(dropData)))
                        gmessage("Already a categorical variable!",
                                 parent = GUI$win, icon = 'warning')
                      else {
                        svalue(h$obj) <- h$dropdata
                        svalue(name.txt) <- makeNames(paste0(h$dropdata, ".cat"))
                        varData <<- dropData
                      }
                    })

      add(GUI$modWin, mainGroup, expand = TRUE)
      visible(GUI$modWin) <<- TRUE
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
      svalue(GUI$modWin) <<- "Transform Variables"
      mainGroup <- ggroup(horizontal = FALSE)
      mainGroup$set_borderwidth(15)
      helpbtn <- gimagebutton(stock.id = "gw-help", cont = mainGroup, anchor = c(1, -1), handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#transform")
      })
      lbl1 <- glabel("Drag and drop variable names onto the labels below\nto create new transformed variables.")
      font(lbl1) <- list(weight="bold", family = "sans", size = 11)

      tbl <- glayout(container = mainGroup)
      ii <- 1

      tbl[ii, 1, expand = TRUE, anchor = c(-1, 0)] <- lbl1
      ii <- ii + 1

      tbl[ii, 1] <- gseparator()
      ii <- ii + 1

      ## function names: the X will be converted to the variable name (e.g., log.height, height.squared, etc)
      ##                  Display name           new name     function
      transforms <- list("LOG (e)"          = c("log.e.X",   "log"),
                         "LOG (10)"         = c("log.10.X",  "log10"),
                         "EXPONENTIAL"      = c("exp.X",     "exp"),
                         "SQUARE (X^2)"     = c("X.squared", "square"),
                         "SQUARE ROOT"      = c("root.X",    "sqrt"),
                         "RECIPROCAL (1/X)" = c("recip.X",   "reciprocal"))

      trLbls <- sapply(seq_along(transforms), function(i) {
        lbl <- glabel(names(transforms)[i])
        font(lbl) <- list(weight = "bold", family = "sans", size = 14, color = "navy")
        tbl[ii + i - 1, 1, expand = TRUE, anchor = c(0, 0)] <- lbl

        addDropTarget(lbl, handler = function(h, ...) {
          var <- h$dropdata
          dropData <- GUI$getActiveDoc()$getData()[var][[1]]
          ## check whether we can transform this variable
          if (checkData(dropData)) {
            name <- makeNames(gsub("X", var, transforms[[i]][1]))
            if (checkNames(name)) {
              fn <- transforms[[i]][2]
              .dataset <- GUI$getActiveData()
              data <- iNZightTools::transformVar(.dataset, var, fn, name)
              updateData(data)
            }
          }
        })
      })

      add(GUI$modWin, mainGroup, expand = TRUE)
      visible(GUI$modWin) <<- TRUE
    },
    ## check whether the data is illegible for transformation
    checkData = function(varData) {
      if (all(is_cat(varData))) {
        gmessage(title = "ERROR",
                 msg = "Categorical variables cannot be transformed",
                 parent = GUI$modWin)
        FALSE
      } else
        TRUE
    }
  ))

## collapse multiple factor levels into one
iNZcllpsWin <- setRefClass(
  "iNZcllpsWin",
  contains = "iNZDataModWin",
  methods = list(
    initialize = function(gui) {
      callSuper(gui)
      svalue(GUI$modWin) <<- "Collapse Levels"
      size(GUI$modWin) <<- c(400, 350)
      mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
      mainGroup$set_borderwidth(15)
      ## instructions through glabels
      lbl1 <- glabel("Choose a variable")
      font(lbl1) <- list(weight = "bold",
                         family = "sans")
      helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#collapse")
      })
      titlelyt <- glayout(homegenous = FALSE)
      titlelyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- lbl1
      titlelyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
      lbl2 <- glabel("Choose two or more levels")
      font(lbl2) <- list(weight = "bold",
                         family = "sans")
      lbl3 <- glabel("(Hold Ctrl to choose many)")
      font(lbl3) <- list(weight = "bold",
                         family = "sans")
      lbl4 <- glabel("New variable name: ")
      lbl5 <- glabel("Collapsed level name: ")
      ## choose a factor column from the dataset and display
      ## its level in a gtable
      factorIndices <- sapply(GUI$getActiveData(), is_cat)
      factorMenu <- gcombobox(names(GUI$getActiveData())[factorIndices],
                              selected = 0)
      addHandlerChanged(factorMenu, handler = function(h, ...) {
        factorLvls[] <- levels(GUI$getActiveData()[svalue(factorMenu)][[1]])
        svalue(newVarname) <- makeNames(sprintf("%s.coll", svalue(h$obj)))
      })
      factorLvls <- gtable("", multiple = TRUE, expand = TRUE)
      names(factorLvls) <- "Levels"
      addHandlerSelectionChanged(factorLvls, handler = function(h, ...) {
        svalue(newLvlname) <- paste(svalue(h$obj), collapse = "_")
      })
      ## name boxes
      newVarname <- gedit('')
      newLvlname <- gedit('')
      cllpsButton <- gbutton(
        " - COLLAPSE -",
        handler = function(h, ...) {
          if (checkLevels(svalue(factorLvls))) {
            var <- svalue(factorMenu)
            lvls <- svalue(factorLvls)
            name <- svalue(newVarname)
            lvlname <- svalue(newLvlname)
            if (lvlname %in% levels(GUI$getActiveData()[[var]]) &&
                !lvlname %in% lvls) {
              ## checking that the new level name isn't one of the other
              ## level names (excluding those being collapsed)
              gmessage("That level name already exists. Please choose another.",
                       title = 'Invalid level name', parent = GUI$modWin,
                       icon = 'warning')
            } else if (checkNames(name)) {
              .dataset <- GUI$getActiveData()
              data <- iNZightTools::collapseLevels(.dataset, var, lvls, lvlname, name)
              updateData(data)
              dispose(GUI$modWin)
            }
          }
        })
      add(mainGroup, titlelyt)
      add(mainGroup, factorMenu)
      add(mainGroup, lbl2)
      add(mainGroup, lbl3)
      add(mainGroup, factorLvls, expand = TRUE)
      tbl <- glayout()
      tbl[1, 1, expand = TRUE, anchor = c(1, 0)] <- lbl4
      tbl[1, 2, expand = TRUE] <- newVarname
      tbl[2, 1, expand = TRUE, anchor = c(1, 0)] <- lbl5
      tbl[2, 2, expand = TRUE] <- newLvlname
      add(mainGroup, tbl)
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
      size(GUI$modWin) <<- c(400, 500)
      ## ggroup does not automatically add scrollbars and gWidget2 does not
      ## have a function to do so. We therefore wrap around the RGtk2 class
      ## gtkScrolledWindow around the ggroup
      scrolledWindow <- gtkScrolledWindow()
      ## setting this will only display a scrollbar if necessary
      scrolledWindow$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
      mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
      mainGroup$set_borderwidth(15)
      ## instructions through glabels
      helpbtn <- gimagebutton(stock.id = "gw-help", cont = mainGroup, anchor = c(1, -1), handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#renamelevs")
      })
      lbl1 <- glabel("Choose variable: ")
      font(lbl1) <- list(weight = "bold",
                         family = "sans")
      lbl2 <- glabel("Name of the new variable:")
      font(lbl2) <- list(weight = "bold",
                         family = "sans")
      ## choose a factor column from the dataset and display
      ## its levels together with their order
      factorIndices <- sapply(GUI$getActiveData(), is_cat)
      factorMenu <- gcombobox(names(GUI$getActiveData())[factorIndices],
                              selected = 0)
      addHandlerChanged(factorMenu, handler = function(h, ...) {
        svalue(factorName) <- makeNames(paste0(svalue(factorMenu), ".rename"))
        displayLevels(tbl,
                      GUI$getActiveData()[svalue(factorMenu)][[1]])
      })
      factorName <- gedit("")
      renameButton <- gbutton("-RENAME-", handler = function(h, ...) {
        var <- svalue(factorMenu)
        newlvls <- changeLevels(tbl, GUI$getActiveData()[var][[1]])
        ## newFactor will be FALSE, if the user input was wrong
        name <- svalue(factorName)
        if (is.list(newlvls) && checkNames(name)) {
          .dataset <- GUI$getActiveData()
          data <- iNZightTools::renameLevels(.dataset, var, newlvls, name)
          updateData(data)
          dispose(GUI$modWin)
        }
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
                         family = "sans")
      lbl4 <- glabel("New Name")
      font(lbl4) <- list(weight = "bold",
                         family = "sans")
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
        return(FALSE)
      }
      else {
        changed <- sapply(seq_along(facLevels), function(i)
          newFacLevels[i] != facLevels[i])
        return(as.list(facLevels)[changed])
      }
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

      helpbtn <- gimagebutton(stock.id = "gw-help", cont = mainGroup, anchor = c(1, -1), handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#reorderlevs")
      })

      tbl <- glayout()

      ## Choose variable to reorder:
      tbl[1, 1, expand = TRUE, anchor = c(1, 0)] <- glabel("Variable to reorder:")
      factorIndices <- sapply(GUI$getActiveData(), is_cat)
      factorMenu <- gcombobox(names(GUI$getActiveData())[factorIndices],
                              selected = 0)
      tbl[1, 2, expand = TRUE] <- factorMenu

      ## Name for the new variable
      tbl[2, 1, expand = TRUE, anchor = c(1, 0)] <- glabel("New variable name:")
      factorName <- gedit("")
      tbl[2, 2] <- factorName

      ## Sort method: frequency (default), or manual
      tbl[3, 1, expand = TRUE, anchor = c(1, 0)] <- glabel("Sort levels ")
      sortMenu <- gcombobox(c("by frequency", "manually"), selected = 1)
      tbl[3, 2, expand = TRUE] <- sortMenu

      ## For manual ordering, gdf or gtable with up/down arrows ...
      levelGrp <- ggroup()
      levelOrder <- gtable(data.frame(stringsAsFactors = TRUE), container = levelGrp)
      size(levelOrder) <- c(-1, 280)
      tbl[4:5, 2, expand = TRUE] <- levelGrp

      levelBtnGrp <- gvbox()
      addSpace(levelBtnGrp, 20)
      levelUp <- iNZight:::gimagebutton("up", container = levelBtnGrp, size = 'LARGE_TOOLBAR',
                                        expand = FALSE, anchor = c(1, 0))
      levelDown <- iNZight:::gimagebutton("down", container = levelBtnGrp, size = 'LARGE_TOOLBAR',
                                          expand = FALSE, anchor = c(1, 0))
      levelHelp <- glabel("Select level, then\nuse arrows to reorder.",
                          container = levelBtnGrp, anchor = c(1, 0))
      tbl[4:5, 1, anchor = c(1, 1)] <- levelBtnGrp

      visible(levelBtnGrp) <- visible(levelGrp) <- FALSE


      ## Done button
      reorderButton <- gbutton("-REORDER-")
      tbl[6, 2, expand = TRUE] <- reorderButton

      ## Add everything to main window
      add(mainGroup, tbl)

      ## HANDLERS
      addHandlerChanged(factorMenu, handler = function(h, ...) {
        svalue(factorName) <- makeNames(sprintf("%s.reord", svalue(factorMenu)))
        levelOrder$set_items(
          data.frame(
            Levels = levels(GUI$getActiveData()[, svalue(factorMenu)]),
            stringsAsFactors = TRUE
          )
        )
      })

      addHandlerChanged(sortMenu, handler = function(h, ...) {
        visible(levelBtnGrp) <- visible(levelGrp) <- svalue(sortMenu, index = TRUE) == 2
      })

      addHandlerClicked(levelUp, function(h, ...) {
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
        lvls[i] <- lvls[i-1]
        lvls[i-1] <- li
        levelOrder$set_items(data.frame(Levels = lvls, stringsAsFactors = TRUE))
        svalue(levelOrder) <- li
        # unblockHandlers(levelUp)
        # unblockHandlers(levelDown)
      })
      addHandlerClicked(levelDown, function(h, ...) {
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
        lvls[i] <- lvls[i+1]
        lvls[i+1] <- li
        levelOrder$set_items(data.frame(Levels = lvls, stringsAsFactors = TRUE))
        svalue(levelOrder) <- li
        # unblockHandlers(levelUp)
        # unblockHandlers(levelDown)
      })

      addHandlerClicked(reorderButton, function(h, ...) {
        var <- svalue(factorMenu)
        varname <- svalue(factorName)
        .dataset <- GUI$getActiveData()

        if (checkNames(varname)) {
          if (svalue(sortMenu, TRUE) == 1)
            data <- iNZightTools::reorderLevels(.dataset, var, freq = TRUE, name = varname)
          else {
            levels <- as.character(levelOrder$get_items())
            data <- iNZightTools::reorderLevels(.dataset, var, levels, name = varname)
          }
          updateData(data)
          dispose(GUI$modWin)
        }
      })


      ## final few details
      scrolledWindow$addWithViewport(mainGroup$widget)
      add(GUI$modWin, scrolledWindow, expand = TRUE, fill = TRUE)
      size(GUI$modWin) <<- c(300, 500)
      visible(GUI$modWin) <<- TRUE
    }
  )
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
      lbl1 <- glabel("Choose 2 or more variables you want to combine")
      font(lbl1) <- list(weight = "bold",
                         family = "sans")
      helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#catcombine")
      })
      titlelyt <- glayout(homegenous = FALSE)
      titlelyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- lbl1
      titlelyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn

      lbl2 <- glabel("(Hold Ctrl to choose many)")
      font(lbl2) <- list(weight = "bold",
                         family = "sans")
      lbl3 <- glabel("New Variable Name")
      ## choose a factor column from the dataset and display
      ## its level in a gtable
      factorIndices <- sapply(GUI$getActiveData(), is_cat)
      factorNames <- gtable(names(GUI$getActiveData())[factorIndices],
                            multiple = TRUE, expand = TRUE)
      names(factorNames) <- "Categorical Variables"
      newName <- gedit()
      ## separator (. or _ for now ...)
      lbl4 <- glabel("Value separator")
      varSep <- gcombobox(c(".", "_"), selected = 1)
      ## automatically fill the name field when variables are selected
      addHandlerSelectionChanged(factorNames, handler = function(h, ...) {
        if (length(svalue(factorNames)) > 1)
          svalue(newName) <- makeNames(paste(svalue(factorNames),
                                             collapse = svalue(varSep)))
        else svalue(newName) <- ""
      })
      addHandlerChanged(varSep, function(h, ...) {
        if (length(svalue(factorNames)) <= 1) return()
        sep <- svalue(h$obj)
        osep <- switch(sep, "_" = ".", "." = "_")
        oname <- makeNames(paste(svalue(factorNames), collapse = osep))
        if (svalue(newName) == oname) {
          ## user hasn't changed the name, so update it
          svalue(newName) <- makeNames(paste(svalue(factorNames), collapse = sep))
        }
      })
      cmbButton <- gbutton(
        " - COMBINE - ",
        handler = function(h, ...) {
          if (checkSelection(svalue(factorNames),
                             svalue(newName))) {
            vars <- svalue(factorNames)
            name <- svalue(newName)
            sep <- svalue(varSep)

            if (checkNames(name)) {
              .dataset <- GUI$getActiveData()
              data <- iNZightTools::combineCatVars(.dataset, vars, sep, name)
              updateData(data)
              dispose(GUI$modWin)
            }
          }
        })
      add(mainGroup, titlelyt)
      add(mainGroup, lbl2)
      add(mainGroup, factorNames, expand = TRUE)
      tbl <- glayout()
      tbl[1, 1, anchor = c(1, 0), expand = TRUE] <- lbl3
      tbl[1, 2, expand = TRUE] <- newName
      tbl[2, 1, anchor = c(1, 0), expand = TRUE] <- lbl4
      tbl[2, 2, expand = TRUE] <- varSep
      add(mainGroup, tbl)
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
      font(lbl1) <- list(weight="bold", family = "sans")
      helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#create")
      })
      titlelyt <- glayout(homegenous = FALSE)
      titlelyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- lbl1
      titlelyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
      lbl2 = glabel("EXAMPLES")
      font(lbl2) <- list(weight="bold", family = "sans")
      newVarName = gedit("new.variable", width = 15) ## name of the new variable
      newVarExp = gedit("  ") ## expression used to create new var
      submitButton = gbutton(" - SUBMIT -", handler = function(h,...) {
        expr <- svalue(newVarExp)
        name <- svalue(newVarName)
        .dataset <- GUI$getActiveData()
        data <- try(iNZightTools::createNewVar(.dataset, name, expr), silent = TRUE)
        if (inherits(data, 'try-error')) {
          err <- strsplit(data, "\n")[[1]]
          ew <- grepl('Evaluation error', err, fixed = TRUE)
          err <- ifelse(any(ew), gsub('Evaluation error:', '', err[ew]), '')
          gmessage(paste(sep = "\n\n", 'Invalid expression:', err),
                   icon = 'error', parent = GUI$modWin)
          return()
        }
        updateData(data)
        dispose(GUI$modWin)
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
      add(mainGroup, titlelyt)
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
      helpbtn <- gimagebutton(stock.id = "gw-help", cont = mainGroup, anchor = c(1, -1), handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#classints")
      })
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
      numIndices <- sapply(GUI$getActiveData(), function(x) !is_cat(x))
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
      # size(GUI$modWin) <<- c(500, 400)
      ## ggroup does not automatically add scrollbars and gWidget2 does not
      ## have a function to do so. We therefore wrap the RGtk2 class
      ## gtkScrolledWindow around the ggroup
      scrolledWindow <- gtkScrolledWindow()
      ## setting this will only display a scrollbar if necessary
      scrolledWindow$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
      mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
      mainGroup$set_borderwidth(15)
      helpbtn <- gimagebutton(stock.id = "gw-help", cont = mainGroup, anchor = c(1, -1), handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#renamevars")
      })
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
                                onames <- names(GUI$getActiveData())
                                vnames <- sapply(tbl[-1, 2], svalue)
                                if (any(table(vnames) > 1)) {
                                  gmessage("You have duplicated some variable names.",
                                           title = "Duplicated variable names", icon = 'warning',
                                           parent = GUI$modWin)
                                  return()
                                }
                                ## only pass through variables that change
                                w <- vnames != onames
                                namelist <- as.list(vnames[w])
                                names(namelist) <- onames[w]
                                .dataset <- GUI$getActiveData()
                                data <- iNZightTools::renameVars(.dataset, namelist)
                                updateData(data)
                                dispose(GUI$modWin)
                              })
      add(mainGroup, tbl)
      add(mainGroup, renameButton)
      ## method of gtkScrolledWindow to add a GtkWidget (not a gWidgets2 class)
      ## as a child using a viewport
      scrolledWindow$addWithViewport(mainGroup$widget)
      add(GUI$modWin, scrolledWindow, expand = TRUE, fill = TRUE)
      size(GUI$modWin) <<- c(500, -1)
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
                         family = "sans")
      helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#standardize")
      })
      titlelyt <- glayout(homegenous = FALSE)
      titlelyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- lbl1
      titlelyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
      lbl2 <- glabel("(Hold Ctrl to choose many)")
      font(lbl2) <- list(weight = "bold",
                         family = "sans")
      ## display only numeric variables
      numIndices <- sapply(GUI$getActiveData(), function(x) !is_cat(x))
      numVar <- gtable(names(GUI$getActiveData())[numIndices],
                       multiple = TRUE)
      names(numVar) <- "Variables"
      stdButton <- gbutton("Standardise", handler = function(h, ...) {
        if (length(svalue(numVar)) > 0) {
          varnames <- svalue(numVar)
          names <- makeNames(paste0(varnames, ".std"))
          print(names)
          .dataset <- GUI$getActiveData()
          data <- iNZightTools::standardizeVars(.dataset, varnames, names)
          updateData(data)
        }
      })
      add(mainGroup, titlelyt)
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
      font(lbl1) <- list(weight="bold", family = "sans")
      helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#deletevars")
      })
      titlelyt <- glayout(homegenous = FALSE)
      titlelyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- lbl1
      titlelyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
      lbl2 = glabel("(Hold Ctrl to choose many)")
      font(lbl2) <- list(weight="bold", family = "sans")
      listOfVars = gtable(names(GUI$getActiveData()),
                          multiple = TRUE, expand = TRUE)
      names(listOfVars) = "Variables"
      deleteButton = gbutton(
        "- Delete -",
        handler = function(h,...) {
          vars <- svalue(listOfVars)
          if (length(vars) > 0) {
            if (length(vars) == length(names(GUI$getActiveData()))) {
              gmessage("You can't delete all of the variables ... you'll have nothing left!",
                       title = 'Oops...', icon = 'error', parent = GUI$modWin)
              return()
            }
            confirmDel <- gconfirm(
              title = "Are you sure?",
              msg = paste(
                "You are about to delete the",
                "following variables:\n",
                paste(vars, collapse = "\n")
              ),
              icon = "question")
            if (confirmDel) {
              .dataset <- GUI$getActiveData()
              data <- iNZightTools::deleteVars(.dataset, vars)
              GUI$getActiveDoc()$getModel()$updateData(data)
              dispose(GUI$modWin)
            }
          }
        })
      add(mainGroup, titlelyt)
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
      font(lbl1) <- list(weight="bold", family = "sans")
      helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#missingcat")
      })
      titlelyt <- glayout(homegenous = FALSE)
      titlelyt[1, 1:19, expand = TRUE] <- lbl1
      titlelyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
      lbl2 = glabel("(Hold Ctrl to choose many)")
      font(lbl2) <- list(weight="bold", family = "sans")
      listOfVars = gtable(names(GUI$getActiveData()),
                          multiple = TRUE, expand = TRUE)
      names(listOfVars) = "Variables"
      convertButton = gbutton(
        "- Convert -",
        handler = function(h,...) {
          if (length(svalue(listOfVars)) > 0) {
            vars <- svalue(listOfVars)
            names <- makeNames(paste0(vars, "_miss"))
            .dataset <- GUI$getActiveData()
            data <- iNZightTools::missingToCat(.dataset, vars, names)
            updateData(data)
            dispose(GUI$modWin)
          }
        })
      add(mainGroup, titlelyt)
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
                         family = "sans")
      helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#rank")
      })
      titlelyt <- glayout(homegenous = FALSE)
      titlelyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- lbl1
      titlelyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
      lbl2 <- glabel("(Hold Ctrl to choose many)")
      font(lbl2) <- list(weight = "bold",
                         family = "sans")
      ## display only numeric variables
      numIndices <- sapply(GUI$getActiveData(), function(x) !is_cat(x))
      numVar <- gtable(names(GUI$getActiveData())[numIndices],
                       multiple = TRUE)
      names(numVar) <- "Variables"
      rankButton <- gbutton("Rank", handler = function(h, ...) {
        if (length(svalue(numVar)) > 0) {
          vars <- svalue(numVar)
          .dataset <- GUI$getActiveData()
          data <- iNZightTools::rankVars(.dataset, vars)
          updateData(data)
        }
        else {
          gmessage("Select at leat one variable!",
                   parent = GUI$win)
        }
      })
      add(mainGroup, titlelyt)
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
                         family = "sans")
      helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#convert2")
      })
      titlelyt <- glayout(homegenous = FALSE)
      titlelyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- lbl1
      titlelyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
      lbl2 <- glabel("(Hold Ctrl to choose many)")
      font(lbl2) <- list(weight = "bold",
                         family = "sans")
      ## display only numeric variables
      numIndices <- sapply(GUI$getActiveData(), function(x) !is_cat(x))
      numVar <- gtable(names(GUI$getActiveData())[numIndices],
                       multiple = TRUE)
      names(numVar) <- "Variables"
      ctmcButton <- gbutton("Convert", handler = function(h, ...) {
        if (length(svalue(numVar)) > 0) {
          vars <- svalue(numVar)
          varnames <- makeNames(paste(vars, "cat", sep = "."))
          .dataset <- GUI$getActiveData()
          data <- iNZightTools::convertToCat(.dataset, vars, varnames)
          updateData(data)
          dispose(GUI$modWin)
        }
      })
      add(mainGroup, titlelyt)
      add(mainGroup, lbl2)
      add(mainGroup, numVar, expand = TRUE)
      add(mainGroup, ctmcButton)
      add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)
      visible(GUI$modWin) <<- TRUE
    })
)

iNZrenameDataWin <- setRefClass(
  "iNZrenameDataWin",
  contains = "iNZDataModWin",
  methods = list (
    initialize = function(gui) {
      callSuper(gui)
      svalue(GUI$modWin) <<- "Rename dataset"
      size(GUI$modWin) <<- c(250, 150)

      mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
      mainGroup$set_borderwidth(15)

      lbl <- glabel("Enter a new name for the current dataset")
      font(lbl) <- list(weight = "bold", family = "sans")

      curname <- attr(GUI$getActiveData(), "name", exact = TRUE)
      if (length(curname) == 0) curname <- ""
      name <- gedit(curname)

      okbtn <- gbutton("OK")
      addHandlerClicked(okbtn, function(h, ...) {
        newname <- svalue(name)
        if (newname == "") {
          gmessage("Please enter a name", icon = "error", parent = GUI$win)
        } else if (newname %in% GUI$dataNameWidget$nameLabel$get_items()) {
          gmessage("Oops... that name is used by another dataset. Try something else!")
        } else {
          GUI$getActiveDoc()$dataModel$setName(newname)
          dispose(GUI$modWin)
        }
      })

      cancelbtn <- gbutton("Cancel")
      addHandlerClicked(cancelbtn, function(h, ...) dispose(GUI$modWin))

      add(mainGroup, lbl)
      add(mainGroup, name)
      add(mainGroup, okbtn)
      add(mainGroup, cancelbtn)

      add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)

      visible(GUI$modWin) <<- TRUE
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
      helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#dtconvert")
      })
      titlelyt <- glayout(homegenous = FALSE)
      titlelyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- title
      titlelyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
      add(mainGroup, titlelyt)
      addSpace(mainGroup, 5)

      date_string <- glabel("Select variable to convert from", container = mainGroup, anchor = c(-1, 0))

      var1 = gcombobox(c("", names(GUI$getActiveData())), cont = mainGroup, handler = function(h, ...) {
        varname = svalue(var1)
        if (varname == "") {
          dfview$set_items("")
          convertedview$set_items("")
          svalue(newVarname) = ""
        } else {
          varx = GUI$getActiveData()[[varname]]
          svalue(newVarname) = makeNames(paste(varname, ".dt", sep = ""))
          dfview$set_items(data.frame(Original = varx, stringsAsFactors = TRUE))
          if (svalue(var2) == "") {
            return()
          } else {
            convname = svalue(var2)
            .dataset = GUI$getActiveData()
            name = svalue(newVarname)
            data = tryCatch(
              convertedview$set_items(
                data.frame(
                  Converted = iNZightTools::convert_to_datetime(.dataset, varname, convname, name)[[svalue(newVarname)]],
                  stringsAsFactors = TRUE
                  )
                ),
              warning = function(w) {
                if (w$message == "Failed to parse") {
                  convertedview$set_items(data.frame(
                    Converted = "Invalid format",
                    stringsAsFactors = TRUE
                  ))
                } else {
                  convertedview$set_items(data.frame(
                    Converted = w$message,
                    stringsAsFactors = TRUE
                  ))
                }
              })
          }
        }
      })

      factorsbox = gvbox(cont = mainGroup)
      factors = gtable(names(GUI$getActiveData()),multiple = TRUE, expand = TRUE, cont = factorsbox)
      names(factors) = "Variables"
      addHandlerSelectionChanged(factors, function(h, ...) {
        factorname = svalue(factors)
        varx = ""
        new_name = ""
        for (num in 1:length(factorname)) {
          name = factorname[num]
          varx = paste(varx, GUI$getActiveData()[[name]])
          new_name = paste(new_name, name, ".", sep = "")
        }
        dfview$set_items(data.frame(Original = varx, stringsAsFactors = TRUE))
        svalue(newVarname) = makeNames(paste(new_name, "dt", sep = ""))
        if (svalue(var2) == ""){
          return()
        } else{
          convname = svalue(var2)
          .dataset = GUI$getActiveData()
          name = svalue(newVarname)
          data = tryCatch(
            convertedview$set_items(
              data.frame(
                Converted = iNZightTools::convert_to_datetime(.dataset, factorname, convname, name)[[svalue(newVarname)]],
                stringsAsFactors = TRUE
              )
            ),
            warning = function(w) {
              if (w$message == "Failed to parse") {
                convertedview$set_items(
                  data.frame(Converted = "Invalid format", stringsAsFactors = TRUE)
                )
              } else {
                convertedview$set_items(
                  data.frame(Converted = w$message, stringsAsFactors = TRUE)
                )
              }
            })
        }
      })
      visible(factorsbox) = FALSE
      size(factorsbox) = c(-1, 250)

      checkbox = gcheckbox(text = "Click to use multiple variables", cont = mainGroup, handler = function(h, ...) {
        if (svalue(checkbox) == TRUE) {
          visible(factorsbox) = TRUE
          visible(var1) = FALSE
          date_string$set_value("Select variables to convert from \n(Use CNTRL to add/remove)")
        } else {
          visible(factorsbox) = FALSE
          visible(var1) = TRUE
          date_string$set_value("Select variable to convert from")
        }
      })

      addSpace(mainGroup, 5)

      name_string <- glabel("Name for the new variable", container = mainGroup, anchor = c(-1, 0))
      newVarname = gedit("", cont = mainGroup)

      dt.formats <- c("",
                      "year month date",
                      "year month date Hour Minute Second",
                      "year month date Hour Minute Second pm/am",
                      "day month year",
                      "day month year Hour Minute Second",
                      "day month year Hour Minute Second pm/am",
                      "Unix timestamp (secs from 1970)")

      for.var <- glabel("Select the order format of your data", container = mainGroup, anchor = c(-1, 0))

      var2 <- gcombobox(items = dt.formats, container = mainGroup, editable = TRUE, handler = function(h,...){
        if (svalue(checkbox) == TRUE) {
          factorname = svalue(factors)
        } else {
          factorname = svalue(var1)
        }
        convname = svalue(var2)
        if (length(factorname) != 0 & convname == "") {
          convertedview$set_items("")
        } else {
          .dataset = GUI$getActiveData()
          name = svalue(newVarname)
          data = tryCatch(
            convertedview$set_items(
              data.frame(
                Converted = iNZightTools::convert_to_datetime(.dataset, factorname, convname, name)[[svalue(newVarname)]],
                stringsAsFactors = TRUE
              )
            ),
            warning = function(w) {
              if (w$message == "Failed to parse") {
                convertedview$set_items(
                  data.frame(Converted = "Invalid format", stringsAsFactors = TRUE)
                )
              } else {
                convertedview$set_items(
                  data.frame(Converted = w$message, stringsAsFactors = TRUE)
                )
              }
            })
        }
      })

      g2 <- gexpandgroup(container = mainGroup, text = "Advanced selection")
      visible(g2) <- FALSE

      tbl = glayout(cont = g2, expand = TRUE)
      tbl[1,1] <- gbutton("year", handler = function(h,...){svalue(var2) = paste(svalue(var2), svalue(h$obj))})
      tbl[1,2] <- gbutton("month", handler = function(h,...){svalue(var2) = paste(svalue(var2), svalue(h$obj))})
      tbl[1,3] <- gbutton("day", handler = function(h,...){svalue(var2) = paste(svalue(var2), svalue(h$obj))})
      tbl[1,4] <- gbutton("pm/am", handler = function(h,...){svalue(var2) = paste(svalue(var2), svalue(h$obj))})
      tbl[2,1] <- gbutton("Hour", handler = function(h,...){svalue(var2) = paste(svalue(var2), svalue(h$obj))})
      tbl[2,2] <- gbutton("Minute", handler = function(h,...){svalue(var2) = paste(svalue(var2), svalue(h$obj))})
      tbl[2,3] <- gbutton("Second", handler = function(h,...){svalue(var2) = paste(svalue(var2), svalue(h$obj))})
      tbl[3,3] <- gbutton("delete", handler = function(h, ...){
        number_of_space = nchar(svalue(var2)) - nchar(gsub(" ", "", svalue(var2)))
        if (number_of_space == 1 | svalue(var2) == "") {
          svalue(var2) = ""
        } else {
          svalue(var2) = stringr::str_match(svalue(var2), "(^.+)\\s")[,2]
        }
      })
      tbl[3,4] <- gbutton("clear", handler = function(h,...){svalue(var2) = ""})

      g3 = ggroup(cont = mainGroup)
      dfview = gtable(data.frame(Original = "", stringsAsFactors = TRUE), cont = g3)
      size(dfview) = c(-1, 250)
      convertedview = gtable(data.frame(Converted = "", stringsAsFactors = TRUE), cont = g3)
      size(convertedview) = c(-1, 250)

      okbtn <- gbutton("Convert", container = mainGroup, handler = function(h,...) {
        if (svalue(checkbox) == TRUE) {
          factorname = svalue(factors)
        } else {
          factorname = svalue(var1)
        }
        convname = svalue(var2)
        name = svalue(newVarname)
        var.dt = iNZightTools::convert_to_datetime(GUI$getActiveData(), factorname, convname, name)
        updateData(var.dt)
        dispose(GUI$modWin)
      })

      scrolledWindow$addWithViewport(mainGroup$widget)
      add(GUI$modWin, scrolledWindow, expand = TRUE, fill = TRUE)
      size(GUI$modWin) <<- c(300, 600)
      visible(GUI$modWin) <<- TRUE
    })
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
      helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#dtextract")
      })
      titlelyt <- glayout(homegenous = FALSE)
      titlelyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- title
      titlelyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
      add(mainGroup, titlelyt)
      addSpace(mainGroup, 5)

      date_string <- glabel("Select variable to extract information from",
                            container = mainGroup, anchor = c(-1, 0))

      var1 <- gcombobox(items = c("", c(names(dplyr::select_if(GUI$getActiveData(), lubridate::is.Date)), names(dplyr::select_if(GUI$getActiveData(), lubridate::is.POSIXct)))), container = mainGroup, handler = function(h,...){
        varname <<- svalue(var1)
        if (varname == "") {
          dfview$set_items(data.frame(Original = "", stringsAsFactors = TRUE))
          extractedview$set_items(data.frame(Extracted = "", stringsAsFactors = TRUE))
        } else {
          varx = GUI$getActiveData()[[varname]]
          dfview$set_items(data.frame(Original = as.character(varx), stringsAsFactors = TRUE))
          if (component == "") {
            return()
          }
          updatePreview()
        }
      })

      for.var <- glabel("Select elements to extract \n(click + of lowest-level information for options)", container = mainGroup, anchor = c(-1, 0))

      offspring <- function(path=character(0), lst, ...) {
        if(length(path))
          obj <- lst[[path]]
        else
          obj <- lst
        nms <- names(obj)
        hasOffspring <- sapply(nms, function(i) {
          newobj <- obj[[i]]
          is.recursive(newobj) && !is.null(names(newobj))
        })
        data.frame(Name=nms, hasOffspring=hasOffspring, ## fred=nms,
                   stringsAsFactors=FALSE)
      }

      l <- list(Date = list("Date only" = "Date only",
                            Year = list("Year" = "Year", "Century" = "Century", "Decimal Year" = "Decimal Year"),
                            Quarter = list("Year Quarter" = "Year Quarter", "Quarter" = "Quarter"),
                            Month = list("Year Month" = "Year Month", "Month (full)" = "Month (full)", "Month (abbreviated)" = "Month (abbreviated)", "Month (number)" = "Month (number)"),
                            Week = list("Week of the year (Sunday as first day of the week)" = "Week of the year (Sunday as first day of the week)", "Week of the year (Monday as first day of the week)" = "Week of the year (Monday as first day of the week)"),
                            Day = list("Day of the year" = "Day of the year", "Day of the week (name)" = "Day of the week (name)", "Day of the week (abbreviated)" = "Day of the week (abbreviated)", "Day of the week (number, Monday as 1)" = "Day of the week (number, Monday as 1)", "Day of the week (number, Sunday as 0)" = "Day of the week (number, Sunday as 0)","Day" = "Day")),
                Time = list("Time only" = "Time only", "Hours (decimal)" = "Hours (decimal)", "Hour" = "Hour", "Minute" = "Minute", "Second" = "Second")
      )


      atree <- gtree(offspring=offspring, offspring.data=l, cont = mainGroup)

      component <<- ""
      addHandlerClicked(atree, function(h, ...) {
        component <<- svalue(atree)[length(svalue(atree))]
        svalue(newVarname) = makeNames(paste(varname, ".", switch(component, "Date only" = "Date",
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
                                                                  "Hours (decimal)" = "Hour.decimal", component), sep = ""))
        newname <<- svalue(newVarname)
        updatePreview()
      })

      date_string <- glabel("Name for new variable", container = mainGroup, anchor = c(-1, 0))
      newVarname = gedit("", cont = mainGroup)
      addHandlerKeystroke(newVarname, function(h, ...) {
        newname <<- ifelse(svalue(newVarname)=="", "Extracted", svalue(newVarname))
        updatePreview()
      })

      preview_string = glabel("Preview", cont = mainGroup, anchor = c(-1, 0))

      g2 = ggroup(cont = mainGroup)
      dfview = gtable(data.frame(Original = "", stringsAsFactors = TRUE), cont = g2)
      size(dfview) = c(-1, 250)
      extractedview <<- gtable(data.frame(Extracted = "", stringsAsFactors = TRUE), cont = g2)
      size(extractedview) <<- c(-1, 250)

      okbtn <- gbutton("Extract", container = mainGroup, handler = function(h,...) {
        .dataset = GUI$getActiveData()
        exp = iNZightTools::extract_part(.dataset, varname, component, newname)
        updateData(exp)
        dispose(GUI$modWin)
      })

      scrolledWindow$addWithViewport(mainGroup$widget)
      add(GUI$modWin, scrolledWindow, expand = TRUE, fill = TRUE)
      size(GUI$modWin) <<- c(300, 700)
      visible(GUI$modWin) <<- TRUE
    },
    updatePreview = function() {
      .dataset <- GUI$getActiveData()
      list <- list("Date only", "Year", "Century", "Decimal Year", "Year Quarter", "Quarter",
                   "Year Month", "Month (full)", "Month (abbreviated)", "Month (number)", "Year Week",
                   "Week of the year (Monday as first day of the week)", "Week of the year (Sunday as first day of the week)",
                   "Day of the year", "Day of the week (name)", "Day of the week (abbreviated)",
                   "Day of the week (number, Monday as 1)", "Day of the week (number, Sunday as 0)",
                   "Day", "Time only", "Hours (decimal)", "Hour", "Minute", "Second")
      if (component %in% list) {
        d <- iNZightTools::extract_part(.dataset, varname, component, newname)
        extractedview$set_items(
          data.frame(Extracted = as.character(d[[newname]]), stringsAsFactors = TRUE)
        )
      }
    }
  ))

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
      helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/variables/#dtaggregate")
      })

      titlelyt <- glayout(homegenous = FALSE)
      titlelyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- title
      titlelyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
      add(mainGroup, titlelyt)
      addSpace(mainGroup, 5)

      var1_string <- glabel("Select a column", container = mainGroup)

      col <<- ""
      var1 <- gcombobox(items = c("", names(GUI$getActiveData())), cont = mainGroup)
      addHandlerChanged(var1, function(h, ...) {
        col <<- svalue(var1)
        var <<- GUI$getActiveData()[[col]]
        newview$set_items("")
        if (lubridate::is.POSIXct(var) == TRUE | lubridate::is.Date(var) == TRUE) {
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
      })

      formatlist <- c("", "Weekly", "Monthly", "Quarterly", "Yearly")

      var2_string <- glabel("Choose format", cont = mainGroup)

      format <<- ""
      var2 <- gcombobox(items = formatlist, cont = mainGroup)
      addHandlerChanged(var2, function(h, ...) {
        format <<- svalue(var2)
        if (format == "") {
          newview$set_items()
        }
        if (format != "" & method != "" & col != "") {
          updateView()
        }
      })

      var3_string <- glabel("How to aggregate", cont = mainGroup)

      method <<- ""
      var3 <- gtable(c("Sum", "Mean", "Median"), cont = mainGroup)
      size(var3) <- c(-1, 150)
      addHandlerSelectionChanged(var3, function(h, ...) {
        method <<- svalue(var3)
        updateView()
      })

      name <<- "newcol"

      prevTbl <- glayout(homogeneous = FALSE, container = mainGroup)

      string1 <- glabel("Original dataset")
      originview <- gtable(data.frame(GUI$getActiveData(), stringsAsFactors = TRUE))
      prevTbl[1,1, expand = TRUE] <- string1
      prevTbl[2,1, expand = TRUE] <- originview
      size(originview) = c(-1, 250)

      string2 <- glabel("New dataset")
      newview <<- gtable(data.frame("", stringsAsFactors = TRUE))
      prevTbl[1,2, expand = TRUE] <- string2
      prevTbl[2,2, expand = TRUE] <- newview
      size(newview) <<- c(-1, 250)

      aggregatebtn <- gbutton("Aggregate", cont = mainGroup, handler = function(h,...) {
        .dataset <- GUI$getActiveData()
        data <- aggregate()
        attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "aggregated", sep = ".")
        attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
        GUI$setDocument(iNZDocument$new(data = data))
        dispose(GUI$modWin)
      })

      add(GUI$modWin, mainGroup, expand = TRUE)
      visible(GUI$modWin) <<- TRUE
    },
    aggregate = function() {
      .dataset = GUI$getActiveData()
      if (key == "dt" & format != "") {
        part = switch(format, "Weekly" = "Year Week",
                      "Monthly" = "Year Month",
                      "Quarterly" = "Year Quarter",
                      "Yearly" = "Year")
        df = iNZightTools::extract_part(.dataset, col, part, format)
        df = iNZightTools::aggregateData(df, format, method)
        colname = subset(colnames(df), grepl("[:.:]missing$",colnames(df)))
        for (i in 1:length(colname)) {
          if (all(df[[colname[i]]] == 0)) {
            df[, colname[i]] <- NULL
          }
        }
        return(df)
      } else if (key != "" & key != "dt" & format != "") {
        newdata <- iNZightTools::separate(.dataset, col, "left", "right", key, "Column")
        df = iNZightTools::aggregatedt(newdata, format, key, format)
        df = iNZightTools::aggregateData(df, format, method)
        colname = subset(colnames(df), grepl("[:.:]missing$",colnames(df)))
        for (i in 1:length(colname)) {
          if (all(df[[colname[i]]] == 0)) {
            df[, colname[i]] <- NULL
          }
        }
        return(df)
      }
    },
    updateView = function() {
      df = aggregate()
      if (length(df) != 0) {
        newview$set_items(df)
      }
    }
  ))
