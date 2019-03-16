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
    GUI = "ANY",
    colname = "ANY",
    key = "ANY",
    value = "ANY",
    newview = "ANY",
    col1 = "ANY",
    col2 = "ANY",
    type = "ANY"
  ),
  methods = list(
    initialize = function(gui = NULL) {
      initFields(GUI = gui)
      if (!is.null(GUI)) {
        ## close any current mod windows
        try(dispose(GUI$modWin), silent = TRUE)
        
        ## start my window
        GUI$modWin <<- gwindow("Reshape dataset", parent = GUI$win, visible = FALSE)
        mainGroup <- ggroup(cont = GUI$modWin, expand = TRUE, horizontal = FALSE)
        
        title_string = glabel("Reshape Dateset", cont = mainGroup)
        font(title_string) = list(size = 14, weight = "bold")
        
        format_string <- glabel("Select how you want to reshape your dataset", cont = mainGroup)
        format <- gcombobox(items = c("", "Wide to long", "Long to wide"), cont = mainGroup, handler = function(h, ...){
          type <<- svalue(format)
          visible(previewbox) <- TRUE
          visible(reshapebtn) <- TRUE
          if (type == "Wide to long"){
            visible(group1) = TRUE
            visible(group2) = FALSE
          } else if (type == "Long to wide") {
            visible(group2) = TRUE
            visible(group1) = FALSE
          } else{
            visible(group1) = FALSE
            visible(group2) = FALSE
            visible(previewbox) <- FALSE
            visible(reshapebtn) <- FALSE
          }
        })
        
        ## Wide to long
        group1 <- ggroup(cont = mainGroup, horizontal = FALSE)
        
        col_string = glabel("Select column to expand on", cont = group1)
        
        colname <<- ""
        var1 = gcombobox(c("", names(GUI$getActiveData())), cont = group1, handler = function(h, ...){
          colname <<- svalue(var1)
          updatePreview()
        })
        
        var2box = gvbox(cont = group1)
        var2 = gtable(names(GUI$getActiveData()),multiple = TRUE, expand = TRUE, cont = var2box)
        addHandlerSelectionChanged(var2, function(h, ...){
          colname <<- svalue(var2)
          updatePreview()
        })
        
        names(var2) = "Variables"
        visible(var2box) = FALSE
        size(var2box) = c(-1, 150)
        
        checkbox = gcheckbox(text = "Click to select multiple columns", cont = group1, handler = function(h, ...) {
          if (svalue(checkbox) == TRUE) {
            visible(var2box) = TRUE
            visible(var1) = FALSE
            colname <<- svalue(var2)
            col_string$set_value("Select columns to expand on \n(Use CNTRL to add/remove)")
            updatePreview()
          } else {
            visible(var2box) = FALSE
            visible(var1) = TRUE
            colname <<- svalue(var1)
            col_string$set_value("Select column to expand on")
            updatePreview()
          }
        })

        key <<- "key"
        key_string <- glabel("Name the new column for old column names", cont = group1)
        keybox <- gedit("key", cont = group1, handler = function(h,...) {
          key <<- svalue(keybox)
          updatePreview()
        })
        
        value <<- "value"
        value_string <- glabel("Name the new column for old column value", cont = group1)
        valuebox <- gedit("value", cont = group1, handler = function(h,...) {
          value <<- svalue(valuebox)
          updatePreview()
        })
        
        visible(group1) = FALSE
        
        ## Long to wide
        group2 <- ggroup(cont = mainGroup, horizontal = FALSE)
        
        label1 <- glabel("Select the column to gather on", cont = group2)
        col1box <- gcombobox(items = c("", names(GUI$getActiveData())), cont = group2, handler = function(h, ...) {
          col1 <<- svalue(col1box)
          updatePreview()
        })
        
        label2 <- glabel("Select the column with values corresponding to the above column", cont = group2)
        col2box <- gcombobox(items = c("", names(GUI$getActiveData())), cont = group2, handler = function(h,...) {
          col2 <<- svalue(col2box)
          updatePreview()
        })
        
        visible(group2) = FALSE
        
        ## Preview window
        previewbox <- ggroup(cont = mainGroup, horizontal = TRUE, fill = TRUE)
        left <- ggroup(horizontal = FALSE, cont = previewbox)
        right <- ggroup(horizontal = FALSE, cont = previewbox)
        
        string1 <- glabel("Original dataset", cont = left)
        originview = gtable(data.frame(GUI$getActiveData()), cont = left)
        size(originview) = c(-1, 250)
        
        string2 <- glabel("New dataset", cont = right)
        newview <<- gtable(data.frame(""), cont = right)
        size(newview) <<- c(-1, 250)
        
        reshapebtn <- gbutton("Reshape", cont = mainGroup, handler = function(h, ...) {
          df = reshape()
          GUI$setDocument(iNZDocument$new(data = df))
          dispose(GUI$modWin)
        })
        
        visible(previewbox) <- FALSE
        visible(reshapebtn) <- FALSE
        
        visible(GUI$modWin) <<- TRUE
      }
    },
    updatePreview = function() {
      d = reshape()
      newview$set_items(d)
    },
    reshape = function() {
      if (type == "Wide to long"){
        df = iNZightTools::reshape_data_wide_to_long(GUI$getActiveData(), colname, key, value)
      } else if (type == "Long to wide") {
        df = iNZightTools::reshape_data_long_to_wide(GUI$getActiveData(), col1, col2)
      }
    }
  )
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
