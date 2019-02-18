## --------------------------------------------
## Class that handles the filtering of a dataset
## Upon initialization a window with different filter
## options is displayed. Upon choosing one, this
## window is closed and another window with specifics
## for that filter options is opened
## --------------------------------------------

iNZFilterWin <- setRefClass(
    "iNZFilterWin",
    fields = list(
        GUI = "ANY"
        ),
    methods = list(
        initialize = function(gui = NULL) {
            initFields(GUI = gui)
            usingMethods(opt1, opt2, opt3, opt4)
            if (!is.null(GUI)) {
                ## close any current mod windows
               try(dispose(GUI$modWin), silent = TRUE)
               GUI$modWin <<- gwindow("Filter Dataset...", parent = GUI$win,
                                      width = 300, height = 200,
                                      visible = FALSE)
               mainGrp <- ggroup(cont = GUI$modWin, horizontal = FALSE,
                                 expand = TRUE)
               mainGrp$set_borderwidth(15)
               lbl1 <- glabel("Filter data by:")
               font(lbl1) <- list(weight = "bold", style = "normal")
               filterOpt <- gradio(c("levels of a categorical variable",
                                     "numeric condition", "row number",
                                     "randomly"),
                                   horizontal = FALSE, selected = 1)
               add(mainGrp, lbl1)
               add(mainGrp, filterOpt)
               btnGrp <- ggroup(cont = mainGrp, horizontal = TRUE)
               addSpring(btnGrp)
               proceedButton <- gbutton(
                   "- Proceed -",
                   handler = function(h, ...) {
                       opt <-svalue(filterOpt, index = TRUE)
                       dispose(GUI$modWin)
                       do.call(paste("opt", opt, sep = ""),
                               args = list())
                   })
               add(btnGrp, proceedButton)
               visible(GUI$modWin) <<- TRUE
           }
        },
        ## Window for filtering by levels of a categorical variable
        opt1 = function() {
            GUI$modWin <<- gwindow("Filter data by level",
                                   parent = GUI$win, visible = FALSE,
                                   width = 300, height = 450)
            mainGrp <- ggroup(cont = GUI$modWin, horizontal = FALSE,
                              expand = TRUE)
            mainGrp$set_borderwidth(15)
            btnGrp <- ggroup(horizontal = TRUE)
            lbl1 = glabel("Filter data by :")
            font(lbl1) = list(weight = "bold", style = "normal")
            lbl2 = glabel("Select levels to include")
            font(lbl2) = list(weight = "bold", style = "normal")
            lbl3 = glabel("(Hold Ctrl to choose many)")
            ## choose a factor column from the dataset and display
            ## its levels together with their order
            factorIndices <- sapply(GUI$getActiveData(), is.factor)
            factorMenu <- gcombobox(names(GUI$getActiveData())[factorIndices],
                                    selected = 0)
            addHandlerChanged(factorMenu, handler = function(h, ...) {
                factorLvls[] <- levels(GUI$getActiveData()[svalue(factorMenu)][[1]])
            })
            factorLvls <- gtable("", multiple = TRUE, expand = TRUE)
            names(factorLvls) <- "Levels"
            filterButton <- gbutton(
                "-Filter Data-",
                handler = function(h, ...) {
                    if (length(svalue(factorLvls)) > 0) {
                      var <- svalue(factorMenu)
                      lvls <- svalue(factorLvls)
                      .dataset <- GUI$getActiveData()
                      data <- iNZightTools::filterLevels(.dataset, var, lvls)
                      attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "filtered", sep = ".")
                      ## .dataset %>% foo() becomes mydata.filtered %>% foo()
                      attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
                      GUI$setDocument(iNZDocument$new(data = data))
                      # GUI$getActiveDoc()$getModel()$updateData(data)
                      dispose(GUI$modWin)
                    }
                })
            tbl <- glayout()
            tbl[1, 1] <- lbl1
            tbl[1, 2] <- factorMenu
            tbl[2, 1:2, expand = TRUE, anchor = c(-1, -1)] <- lbl2
            tbl[3, 1:2, expand = TRUE, anchor = c(-1, -1)] <- lbl3
            add(mainGrp, tbl)
            add(mainGrp, factorLvls, expand = TRUE)
            add(mainGrp, btnGrp)
            addSpring(btnGrp)
            add(btnGrp, filterButton)
            visible(GUI$modWin) <<- TRUE
        },
        ## Window for filtering by numeric condition
        opt2 = function() {
            GUI$modWin <<- gwindow("Filter data by numeric condition",
                                   parent = GUI$win, visible = FALSE,
                                   width = 300, height = 300)
            mainGrp <- ggroup(cont = GUI$modWin, horizontal = FALSE,
                              expand = TRUE)
            mainGrp$set_borderwidth(15)
            btnGrp <- ggroup(horizontal = TRUE)
            operatorGrp <- ggroup(horizontal = TRUE)
            lessthan = gbutton("  <  ", cont = operatorGrp,
                handler = function(h,...) svalue(operator) <- "<")
            lessthan_equal = gbutton(" <= ", cont = operatorGrp,
                handler = function(h,...) svalue(operator) <- "<=")
            greaterthan = gbutton("  >  ", cont = operatorGrp,
                handler = function(h,...) svalue(operator) <- ">")
            greaterthan_equal = gbutton(" >= ",
                cont = operatorGrp,handler = function(h,...) svalue(operator) <- ">=")
            equal = gbutton(" == ",
                cont = operatorGrp,handler = function(h,...) svalue(operator) <- "==")
            not_equal = gbutton(" != ",
                cont = operatorGrp,handler = function(h,...) svalue(operator) <- "!=")
            addSpring(operatorGrp)
            lbl1 = glabel("Type in your subsetting expression")
            font(lbl1) = list(weight = "bold", style = "normal")
            lbl2 = glabel("eg: X >= 20")
            lbl3 = glabel("eg: X == 20")
            lbl4 = glabel("Choose observations in the dataset where :")
            font(lbl4) = list(weight = "bold", style = "normal")
            numIndices <- sapply(GUI$getActiveData(), function(x) !is.factor(x))
            numMenu <- gcombobox(names(GUI$getActiveData())[numIndices],
                                 selected = 0)
            operator <- gedit("", width = 2)
            expr <- gedit("") ## the expression specified by the user
            submitButton <- gbutton(
                "Submit",
                handler = function(h, ...) {
                  var <- svalue(numMenu)
                  op <- svalue(operator)
                  val <- svalue(expr)
                  .dataset <- GUI$getActiveData()
                  data <- try(iNZightTools::filterNumeric(.dataset, var, op, val), silent = TRUE)
                  if (inherits(data, 'try-error')) {
                    # err <- strsplit(data, "\n")[[1]]
                    # ew <- grepl('Evaluation error', err, fixed = TRUE)
                    # err <- ifelse(any(ew), gsub('Evaluation error:', '', err[ew]), '')
                    gmessage('Invalid numeric condition.',#paste(sep = "\n\n", 'Invalid condition:', err), 
                        icon = 'error', parent = GUI$modWin)
                    return()
                  }
                  attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "filtered", sep = ".")
                  attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
                  GUI$setDocument(iNZDocument$new(data = data))
                  dispose(GUI$modWin)
                })
            tbl <- glayout()
            tbl[1, 1:7, expand = TRUE, anchor = c(-1, 0)] <- lbl1
            tbl[2, 1:7, expand = TRUE, anchor = c(-1, 0)] <- lbl2
            tbl[3, 1:7, expand = TRUE, anchor = c(-1, 0)] <- lbl3
            tbl[4, 1:7, expand = TRUE, anchor = c(-1, 0)] <- lbl4
            tbl[5, 1:4] <- numMenu
            tbl[5, 5] <- operator
            tbl[5, 6:7, expand = TRUE] <- expr
            tbl[6, 1:7] <- operatorGrp
            add(mainGrp, tbl)
            add(mainGrp, btnGrp)
            addSpring(btnGrp)
            add(btnGrp, submitButton)
            visible(GUI$modWin) <<- TRUE
        },
        ## Window for filtering by row numbers
        opt3 = function() {
            GUI$modWin <<- gwindow("Filter data by specified row number",
                                   parent = GUI$win, visible = FALSE,
                                   width = 300, height = 300)
            mainGrp <- ggroup(cont = GUI$modWin, horizontal = FALSE,
                              expand = TRUE)
            mainGrp$set_borderwidth(15)
            btnGrp <- ggroup(horizontal = TRUE)
            lbl1 <- glabel("Type in the Row.names of observations\nthat need to be excluded")
            font(lbl1) <- list(weight = "bold", style = "normal")
            lbl2 <- glabel("(separate each value by a comma)")
            lbl3 <- glabel("EXAMPLE")
            font(lbl3) <- list(weight = "bold", style = "normal")
            lbl4 <- glabel("1,5,99,45,3")
            unwantedObs <- gedit("")
            submitButton <- gbutton(
                "Submit",
                handler = function(h, ...) {
                  rows <- sprintf("c(%s)", svalue(unwantedObs))
                  .dataset <- GUI$getActiveData()
                  data <- try(iNZightTools::filterRows(.dataset, rows), silent = TRUE)
                  if (inherits(data, 'try-error')) {
                    gmessage('Invalid row numbers.',
                        icon = 'error', parent = GUI$modWin)
                    return()
                  }
                  attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "filtered", sep = ".")
                  attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
                  GUI$setDocument(iNZDocument$new(data = data))
                  dispose(GUI$modWin)
                })
            add(mainGrp, lbl1)
            add(mainGrp, lbl2)
            add(mainGrp, lbl3)
            add(mainGrp, lbl4)
            add(mainGrp, unwantedObs, expand = TRUE)
            add(mainGrp, btnGrp)
            addSpring(btnGrp)
            add(btnGrp, submitButton)
            visible(GUI$modWin) <<- TRUE
        },
        ## Window for filtering by random sample
        opt4 = function() {
            GUI$modWin <<- gwindow("Filter data by random sample",
                                   parent = GUI$win, visible = FALSE,
                                   width = 200, height = 100)
            mainGrp <- ggroup(cont = GUI$modWin, horizontal = FALSE,
                              expand = TRUE)
            mainGrp$set_borderwidth(15)
            btnGrp <- ggroup(horizontal = TRUE)
            lbl1 <- glabel("Specify the size of your sample")
            font(lbl1) <- list(weight = "bold", style = "normal")
            numSample <- gspinbutton(from = 1, to = nrow(GUI$getActiveData()), by = 1)
            sampleSize <- gedit("", width =4)
            submitButton <- gbutton(
                "Submit",
                handler = function(h, ...) {
                  nsample <- svalue(numSample)
                  size <- svalue(sampleSize)
                  .dataset <- GUI$getActiveData()
                  N <- suppressWarnings(as.numeric(nsample))
                  if (is.na(N)) {
                    gmessage('Sample size must be a number.', icon = 'warning', parent = GUI$modWin)
                    return()
                  }
                  if (N * as.numeric(size) > nrow(.dataset)) {
                    gmessage(
                      paste(sep = "\n",
                        'Unable to sample that many rows.',
                        'Please make sure Sample Size x Number of Samples < Number of Rows'),
                      title = 'Sample size too big', parent = GUI$modWin, icon = 'warning')
                    return()
                  }
                  data <- iNZightTools::filterRandom(.dataset, nsample, size)
                  attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "filtered", sep = ".")
                  attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
                  GUI$setDocument(iNZDocument$new(data = data))
                  dispose(GUI$modWin)
                })
            tbl <- glayout()
            tbl[1,1:2] <- lbl1
            tbl[2, 1] <- "Total number of rows:"
            tbl[2, 2] <- glabel(nrow(GUI$getActiveData()))
            tbl[3, 1] <- "Sample Size:"
            tbl[3, 2] <- sampleSize
            tbl[4, 1] <- "Number of Samples"
            tbl[4, 2] <- numSample
            add(mainGrp, lbl1)
            add(mainGrp, tbl)
            add(mainGrp, btnGrp)
            addSpring(btnGrp)
            add(btnGrp, submitButton)
            visible(GUI$modWin) <<- TRUE
        },
        insertData = function(data, name, index, msg = NULL, closeAfter = TRUE) {
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

          GUI$getActiveDoc()$getModel()$updateData(newData)
          if (closeAfter)
            dispose(GUI$modWin)
        },
        updateData = function(newdata) {
            GUI$getActiveDoc()$getModel()$updateData(newdata)
        })
)



## --------------------------------------------
## Class that handles the reshaping of a dataset
## --------------------------------------------

iNZReshapeDataWin <- setRefClass(
  "iNZReshapeDataWin",
  fields = list(
    GUI = "ANY"
  ),
  methods = list(
    initialize = function(gui = NULL) {
      initFields(GUI = gui)
      if (!is.null(GUI)) {
        ## close any current mod windows
        try(dispose(GUI$modWin), silent = TRUE)
        GUI$modWin <<- gwindow("Filter data by numeric condition",
                                   parent = GUI$win, visible = FALSE)
            mainGrp <- ggroup(cont = GUI$modWin, horizontal = FALSE,
                              expand = TRUE)
            mainGrp$set_borderwidth(15)
            btnGrp <- ggroup(horizontal = TRUE)
            lbl1 <- glabel("Reshape your dataset so that groups\nas columns are transformed to cases by variables")
            conv.image <- gimage(system.file("images/groups-wide-to-tall.png",
                                             package = "iNZight"))
            reshapeButton <- gbutton(
                "Reshape",
                handler = function(h, ...) {
                    if (ncol(GUI$getActiveData()) <= 1)
                        gmessage("Unable to reshape datasets with a single column", "Error", icon = "error")
                    else {
                        .dataset <- GUI$getActiveData()
                        vars <- names(.dataset)
                        data <- iNZightTools::stackVars(.dataset, vars, 'variable', 'value')
                        attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "stacked", sep = ".")
                        attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
                        GUI$setDocument(iNZDocument$new(data = data))
                        dispose(GUI$modWin)
                    }

                })
            add(mainGrp, lbl1)
            add(mainGrp, conv.image)
            add(mainGrp, btnGrp)
            addSpring(btnGrp)
            add(btnGrp, reshapeButton)
            visible(GUI$modWin) <<- TRUE
            }
        })
)


## --------------------------------------------
## Class that handles the sortby of a dataset
## --------------------------------------------
iNZSortbyDataWin <- setRefClass(
  "iNZSortbyDataWin",
  fields = list(
    GUI = "ANY"
  ),
  methods = list(
    initialize = function(gui = NULL) {
      initFields(GUI = gui)
      if (!is.null(GUI)) {
        ## close any current mod windows
        try(dispose(GUI$modWin), silent = TRUE)
        GUI$modWin <<- gwindow("Sort data by variables",
                               parent = GUI$win, visible = FALSE)
        mainGrp <- ggroup(cont = GUI$modWin, horizontal = FALSE,
                          expand = TRUE)
        mainGrp$set_borderwidth(15)
        btnGrp <- ggroup(horizontal = TRUE)
        lbl1 <- glabel("Sort by")
        font(lbl1) <- list(weight = "bold", style = "normal")
        lbl2 <- glabel("Variable")
        nameList <- names(GUI$getActiveData())
        SortByButton <- gbutton(
          "Sort Now",
          handler = function(h, ...) {
            vars <- sapply(tbl[, 2], svalue)
            asc <- sapply(tbl[, 3], svalue, index = TRUE) == 1
            wi <- vars != ""
            
            .dataset <- GUI$getActiveData()
            data <- iNZightTools::sortVars(.dataset, vars[wi], asc[wi])
            attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "sorted", sep = ".")
            attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
            GUI$setDocument(iNZDocument$new(data = data))
            dispose(GUI$modWin)
          }
        )

        label_var1 <- glabel("1st")
        label_var2 <- glabel("2nd")
        label_var3 <- glabel("3rd")
        label_var4 <- glabel("4th")
        droplist_var1 <- gcombobox(c("",nameList), selected = 1)
        droplist_var2 <- gcombobox(c("",nameList), selected= 1)
        droplist_var3 <- gcombobox(c("",nameList), selected = 1)
        droplist_var4 <- gcombobox(c("",nameList), selected = 1)
        radio_var1 <- gradio(c("increasing","decreasing"), horizontal = TRUE)
        radio_var2 <- gradio(c("increasing","decreasing"), horizontal = TRUE)
        radio_var3 <- gradio(c("increasing","decreasing"), horizontal = TRUE)
        radio_var4 <- gradio(c("increasing","decreasing"), horizontal = TRUE)
        tbl <- glayout()
        tbl[1, 1, expand = TRUE, anchor = c(-1, -1)] <- label_var1
        tbl[1, 2, expand = TRUE, anchor = c(-1, -1)] <- droplist_var1
        tbl[1, 3, expand = TRUE, anchor = c(-1, -1)] <- radio_var1
        tbl[2, 1, expand = TRUE, anchor = c(-1, -1)] <- label_var2
        tbl[2, 2, expand = TRUE, anchor = c(-1, -1)] <- droplist_var2
        tbl[2, 3, expand = TRUE, anchor = c(-1, -1)] <- radio_var2
        tbl[3, 1, expand = TRUE, anchor = c(-1, -1)] <- label_var3
        tbl[3, 2, expand = TRUE, anchor = c(-1, -1)] <- droplist_var3
        tbl[3, 3, expand = TRUE, anchor = c(-1, -1)] <- radio_var3
        tbl[4, 1, expand = TRUE, anchor = c(-1, -1)] <- label_var4
        tbl[4, 2, expand = TRUE, anchor = c(-1, -1)] <- droplist_var4
        tbl[4, 3, expand = TRUE, anchor = c(-1, -1)] <- radio_var4
        add(mainGrp, lbl1, anchor = c(-1, -1))
        addSpring(mainGrp)
        add(mainGrp, lbl2, anchor = c(-1, -1))
        add(mainGrp, tbl)
        add(mainGrp, btnGrp)
        addSpring(btnGrp)
        add(btnGrp, SortByButton)
        visible(GUI$modWin) <<- TRUE
      }
    }
  )
)


## --------------------------------------------
## Class that handles aggregate the data set
## --------------------------------------------
iNZAgraDataWin <- setRefClass(
  "iNZAgraDataWin",
  fields = list(
    GUI = "ANY"
  ),
  methods = list(
    initialize = function(gui = NULL) {
      initFields(GUI = gui)
      if (!is.null(GUI)) {
        ## close any current mod windows
        try(dispose(GUI$modWin), silent = TRUE)
        GUI$modWin <<- gwindow("Aggregation to the data",
                               parent = GUI$win, visible = FALSE)
        mainGrp <- ggroup(cont = GUI$modWin, horizontal = FALSE,
                          expand = TRUE)
        mainGrp$set_borderwidth(15)
        btnGrp <- ggroup(horizontal = TRUE)
        nameList <- names(Filter(is.factor,GUI$getActiveData()))
        heading <- glabel("Aggregate over variables:")
        font(heading) <- list(weight = "bold", style = "normal")
        AgraButton <- gbutton(
          "Aggregate Now",
          handler = function(h, ...) {
            vars <- sapply(tbl[2:4, 2], svalue)
            vars <- vars[vars != ""]
            smrs <- svalue(func.table)
            
            .dataset <- GUI$getActiveData()
            data <- iNZightTools::aggregateData(.dataset, vars, smrs)
            attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "aggregated", sep = ".")
            attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
            GUI$setDocument(iNZDocument$new(data = data))
            dispose(GUI$modWin)
          })
        label_var1 <- glabel("1st")
        label_var2 <- glabel("2nd")
        label_var3 <- glabel("3rd")
        droplist_var1 <- gcombobox(c("",nameList), selected = 1)
        droplist_var2 <- gcombobox(c("",nameList), selected= 1)
        droplist_var3 <- gcombobox(c("",nameList), selected = 1)
        func.frame <- data.frame("Summaries:" = c("Mean", "Median", "Sum", "Sd", "IQR"),
                                 stringsAsFactors = FALSE)
        func.table <- gtable(func.frame, multiple=TRUE)
        func.table$remove_popup_menu() # remove the popup menu from gtable()
        tbl <- glayout()
        tbl[2, 1, expand = TRUE, anchor = c(-1, -1)] <- label_var1
        tbl[2, 2, expand = TRUE, anchor = c(-1, -1)] <- droplist_var1
        tbl[3, 1, expand = TRUE, anchor = c(-1, -1)] <- label_var2
        tbl[3, 2, expand = TRUE, anchor = c(-1, -1)] <- droplist_var2
        tbl[4, 1, expand = TRUE, anchor = c(-1, -1)] <- label_var3
        tbl[4, 2, expand = TRUE, anchor = c(-1, -1)] <- droplist_var3
        tbl[5:25, 1:2, expand =TRUE, anchor = c(-1, -1)] <- func.table
        add(mainGrp, heading)
        addSpring(mainGrp)
        add(mainGrp, tbl)
        add(mainGrp, btnGrp)
        #addSpring(btnGrp)
        add(btnGrp, AgraButton)
        visible(GUI$modWin) <<- TRUE
      }
    }
  )
)


iNZstackVarWin <- setRefClass(
  "iNZstackVarWin",
  fields = list(
    GUI = "ANY"
  ),
  methods = list(
    initialize = function(gui = NULL) {
      initFields(GUI = gui)
      if (!is.null(GUI)) {
        ## close any current mod windows
        try(dispose(GUI$modWin), silent = TRUE)
        GUI$modWin <<- gwindow("Stack data by Variables",
                               parent = GUI$win, visible = FALSE)
        mainGroup <- ggroup(expand = TRUE, horizontal = FALSE)
        ## instructions through glabels
        lbl1 <- glabel("Choose variables to stack")
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
        StackButton <- gbutton("Stack", handler = function(h, ...) {
          if (length(svalue(numVar)) > 0) {
            vars <- svalue(numVar)
            
            .dataset <- GUI$getActiveData()
            data <- iNZightTools::stackVars(.dataset, vars)
            attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "stacked", sep = ".")
            attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
            GUI$setDocument(iNZDocument$new(data = data))
            dispose(GUI$modWin)
          }
        })
        add(mainGroup, lbl1)
        add(mainGroup, lbl2)
        add(mainGroup, numVar, expand = TRUE)
        add(mainGroup, StackButton)
        add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)
        visible(GUI$modWin) <<- TRUE
      }
    })
)


iNZexpandTblWin <- setRefClass(
    "iNZexpandTblWin",
    fields = list(GUI = "ANY"),
    methods = list(
        initialize = function(gui = NULL) {
            initFields(GUI = gui)
            if (!is.null(GUI)) {
                try(dispose(GUI$modWin), silent = TRUE)

                conf <-
                    gconfirm(paste("This will expand the table to individial rows.",
                                   "Use Dataset > Restore dataset to go back to revert this change.",
                                   "Note: this is a temporary workaround for small tables until we integrate frequency tables.",
                                   sep = "\n\n"),
                             title = "Expand table?", icon = "question", parent = GUI$win)

                if (conf) {
                    dat <- GUI$getActiveData()
                    dat <- tryCatch({as.numeric(rownames(dat)); dat},
                                    warning = function(w) {
                                        ## cannot convert rownames to numeric - create column
                                        dat$Row <- rownames(dat)
                                        dat
                                    })
                    numIndices <- sapply(dat, function(x) is.numeric(x))
                    long <- reshape2:::melt.data.frame(
                        dat, measure.vars = colnames(dat)[numIndices],
                        variable.name = "Column", value.name = "Count", na.rm = TRUE)
                    out <- long[rep(rownames(long), long$Count), ]
                    rownames(out) <- 1:nrow(out)
                    ## for 1-way tables, don't need the "Count" column!
                    if (length(unique(out$Column)) == 1)
                        out$Column <- NULL
                    out$Count <- NULL
                    GUI$getActiveDoc()$getModel()$updateData(out)
                }
            }
        }
    )
)

iNZjoinDataWin <- setRefClass(
  "iNZjoinDataWin",
  fields = list(
    GUI = "ANY",
    newdata = "ANY",
    left_col = "ANY",
    right_col = "ANY",
    join_method = "ANY",
    left_name = "ANY",
    right_name = "ANY",
    joinview = "ANY"
  ),
  methods = list(
    initialize = function(gui = NULL) {
      initFields(GUI = gui)
      if (!is.null(GUI)) {
        ## close any current mod windows
        try(dispose(GUI$modWin), silent = TRUE)

        ## start my window
        GUI$modWin <<- gwindow("Join with another dataset by column values",
                                parent = GUI$win, visible = FALSE)
        mainGroup <- ggroup(cont = GUI$modWin, expand = TRUE)
        lyt <- glayout(cont = mainGroup)
         
        title_box = gvbox(cont = lyt)
        left = gvbox(cont = lyt)
        right = gvbox(cont = lyt)
        join = gvbox(cont = lyt)
        
        lyt[1, 1:2] = title_box
        lyt[2,1] <- left
        lyt[2,2] <- right
        lyt[3, 1:2] <- join
        
        ## Title
        title_string = glabel("Join Datasets", cont = title_box)
        font(title_string) = list(size = 14, weight = "bold")
        
        ## Left hand side
        original_string = glabel("Preview of the original dataset", cont = left, anchor = c(-1, 0))
        
        originview = gtable(data.frame(head(GUI$getActiveData(),10)), cont = left)
        
        join_string = glabel("Select join methods", cont = left, anchor = c(-1,0))
        
        join_method <<- "inner_join"
        var1 = gcombobox(items = c("Inner Join", "Left Join", "Full Join", "Semi Join", "Anti Join"), cont = left)
        addHandlerChanged(var1, function(h, ...) {
          join_method <<- switch(svalue(var1), "Inner Join" = "inner_join",
                                                      "Left Join" = "left_join",
                                                      "Full Join" = "full_join",
                                                      "Semi Join" = "semi_join",
                                                      "Anti Join" = "anti_join")
          updatePreview()
        })
        
        column_string = glabel("Select column from original dataset to match ", cont = left, anchor = c(-1, 0))
        
        left_col <<- ""
        var2 = gcombobox(items = c("", names(GUI$getActiveData())), cont = left)
        addHandlerChanged(var2, function(h, ...) {
          left_col <<- svalue(var2)
          updatePreview()
        })

        left_name_box = gvbox(cont = left)
        name_string = glabel("Duplicated cols: suffix for Original", cont = left_name_box, anchor = c(-1, 0))
        left_name <<- "Orig"
        left_name_string = gedit("Orig", cont = left_name_box)
        addHandlerChanged(left_name_string, function(h, ...) {
          left_name <<- svalue(left_name_string)
          updatePreview()
        })
        visible(left_name_box) = FALSE
        
        ## Right hand side
        Preview_string = glabel("Preview of the imported dataset", cont = right, anchor = c(-1, 0))
        data2view = gtable(data.frame(""), cont = right)
        size(data2view) = c(-1, 150)
        
        file_string = glabel("Import data", cont = right, anchor = c(-1,0))
        data_name = gfilebrowse(text = "Specify a file", initial.dir = file.path(".", "data"), cont = right, handler = function(h, ...) {
          newdata <<- read.csv(svalue(data_name))
          data2view$set_items(head(newdata, 10))
          var3$set_items(c("", names(newdata)))
          col = list()
          dup_cols = Reduce(intersect, list(colnames(GUI$getActiveData()), colnames(newdata)))
          if (length(dup_cols) == 0) {
            joinview$set_items("No common column name detected, please specify columns")
          } else {
            for (i in 1:length(dup_cols)) {
              col_name = dup_cols[i]
              left_dup = GUI$getActiveData()[[col_name]]
              right_dup = newdata[[col_name]]
              if (length(dplyr::intersect(left_dup, right_dup)) > 0) {
                col = append(col, dup_cols[i])
              }
            }
            
            # for (i in 1:length(dup_cols)) {
            #   col_name = dup_cols[i]
            #   left_dup = GUI$getActiveData()[[col_name]]
            #   right_dup = newdata[[col_name]]
            #   for (ii in 1:length(left_dup)) {
            #     if (left_dup[i] %in% right_dup) {
            #       col = append(col, dup_cols[i])
            #     } 
            #   }
            # }
            # common_col = unique(col)
            auto_join = iNZightTools::joindata(GUI$getActiveData(), newdata, col, col, join_method, left_name, right_name)
            joinview$set_items(head(auto_join, 10))
          }
        })
        
        right_col <<- ""
        column_string2 = glabel("Select matching column in new dataset", cont = right, anchor = c(-1, 0))
        var3 = gcombobox(items = "", cont = right)
        addHandlerChanged(var3, function(h, ...) {
          right_col <<- svalue(var3)
          updatePreview()
        })
        
        right_name_box = gvbox(cont = right)
        name_string = glabel("Duplicated cols: suffix for New", cont = right_name_box, anchor = c(-1, 0))
        right_name <<- "New"
        right_name_string = gedit("New", cont = right_name_box)
        addHandlerChanged(right_name_string, function(h, ...) {
          right_name <<- svalue(right_name_string)
          updatePreview()
        })
        visible(right_name_box) = FALSE
        
        
        ## Bottom join box
        preview_string2 = glabel("Preview", cont = join, anchor = c(-1, 0))
        joinview <<- gtable(data.frame(""), cont = join)
        size(joinview) = c(-1, 150)
        
        joinbtn = gbutton("Join", cont = join)
        addHandlerChanged(joinbtn, function(h, ...) {
          joined = iNZightTools::joindata(
            GUI$getActiveData(), 
            newdata, 
            left_col, 
            right_col, 
            join_method, 
            left_name, 
            right_name
          )
          GUI$setDocument(iNZDocument$new(data = joined))
          dispose(GUI$modWin)
        })
        
        helpbtn = gbutton("Help", cont = join, handler = function(h, ...) {
          helpwin = gwindow(title = "Help")
          win = gvbox(cont = helpwin)
          
          inner_join = glabel("Inner Join", cont = win)
          font(inner_join) = list(size = 12, weight = "bold")
          inner_join_help = glabel("Keep all the matched rows within both datasets", cont = win)
          addSpace(win, 5)
          
          left_join = glabel("Left Join", cont = win)
          font(left_join) = list(size = 12, weight = "bold")
          left_join_help = glabel("Keep every row in the original dataset and match them to the imported dataset", cont = win)
          addSpace(win, 5)
          
          full_join = glabel("Full Join", cont = win)
          font(full_join) = list(size = 12, weight = "bold")
          full_join_help = glabel("Keep all the rows in both datasets", cont = win)
          addSpace(win, 5)
          
          semi_join = glabel("Semi Join", cont = win)
          font(semi_join) = list(size = 12, weight = "bold")
          semi_join_help = glabel("Keep matched rows in the original dataset ONLY", cont = win)
          addSpace(win, 5)
          
          anti_join = glabel("Anti Join", cont = win)
          font(anti_join) = list(size = 12, weight = "bold")
          anti_join_help = glabel("Return all rows in the original dataset which do not have a match in the imported dataset", cont = win)
          addSpace(win, 5)
        })

        visible(GUI$modWin) <<- TRUE
      }
    },
    updatePreview = function() {
      "update the preview window"
      d = tryCatch(
        joinData(),
        error = function(e) {
          joinview$set_items(e$message)
        }
      )
      d = joinData()
      if (nrow(d) == 0) {
        joinview$set_items("Joined dataset has 0 row")
        
      } else {
        joinview$set_items(head(d, 10))
      }
      # update the svalue(preview)
    },
    joinData = function() {
      "to do the actual join"
      iNZightTools::joindata(
        GUI$getActiveData(), 
        newdata, 
        left_col, 
        right_col, 
        join_method, 
        left_name, 
        right_name
      )
    }
  )
)


iNZappendrowWin <- setRefClass(
  "iNZappendrowWin",
  fields = list(GUI = "ANY",
                newdata = "ANY"),
  methods = list(
    initialize = function(gui = NULL) {
      initFields(GUI = gui)
      if (!is.null(GUI)) {
        ## close any current mod windows
        try(dispose(GUI$modWin), silent = TRUE)
        
        ## start my window
        GUI$modWin <<- gwindow("Append rows",
                               parent = GUI$win, visible = FALSE)
        mainGroup <- ggroup(cont = GUI$modWin, expand = TRUE, horizontal = FALSE)

        title_string = glabel("Append rows", cont = mainGroup)
        font(title_string) = list(size = 14, weight = "bold")
        
        file_string = glabel("Import data", cont = mainGroup, anchor = c(-1,0))
        data_name = gfilebrowse(text = "Specify a file", initial.dir = file.path(".", "data"), cont = mainGroup, handler = function(h, ...) {
          newdata <<- read.csv(svalue(data_name))
        })
        
        check_box = gcheckbox("Tick if you want to attach a timestamp to the appended rows", cont = mainGroup)
        
        appendbtn = gbutton("Append", cont = mainGroup)
        addHandlerChanged(appendbtn, function(h, ...) {
          date = svalue(check_box)
          data = iNZightTools::appendrows(GUI$getActiveData(), newdata, date)
          GUI$setDocument(iNZDocument$new(data = data))
          dispose(GUI$modWin)
        })
        
        visible(GUI$modWin) <<- TRUE
      }
    }
  )
)
