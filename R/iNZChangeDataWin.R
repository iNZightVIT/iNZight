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
        
        helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
          browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/data_options/#filter")
        })
        lbl1 <- glabel("Filter data by:")
        font(lbl1) <- list(weight = "bold", style = "normal")
        
        helplyt <- glayout(homegenous = FALSE)
        helplyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- lbl1
        helplyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
        
        filterOpt <- gradio(c("levels of a categorical variable",
                              "numeric condition", "row number",
                              "randomly"),
                            horizontal = FALSE, selected = 1)
        add(mainGrp, helplyt)
        # add(mainGrp, lbl1)
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
      factorIndices <- sapply(GUI$getActiveData(), is_cat)
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
      numIndices <- sapply(GUI$getActiveData(), function(x) !is_cat(x))
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
        helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
          browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/data_options/#sort")
        })
        lbl1 <- glabel("Sort by")
        font(lbl1) <- list(weight = "bold", style = "normal")
        helplyt <- glayout(homegenous = FALSE)
        helplyt[1, 1:19, expand = TRUE] <- lbl1
        helplyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
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
        add(mainGrp, helplyt)
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
        nameList <- names(Filter(is_cat,GUI$getActiveData()))
        helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
          browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/data_options/#aggregate")
        })
        heading <- glabel("Aggregate over variables:")
        font(heading) <- list(weight = "bold", style = "normal")
        helplyt <- glayout(homegenous = FALSE)
        helplyt[1, 5:19, expand = TRUE, anchor = c(0,0)] <- heading
        helplyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
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
        add(mainGrp, helplyt)
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
        mainGroup$set_borderwidth(15)
        helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
          browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/data_options/#stack")
        })
        ## instructions through glabels
        lbl1 <- glabel("Choose variables to stack")
        font(lbl1) <- list(weight = "bold",
                           family = "normal")
        helplyt <- glayout(homegenous = FALSE)
        helplyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- lbl1
        helplyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
        lbl2 <- glabel("(Hold Ctrl to choose many)")
        font(lbl2) <- list(weight = "bold",
                           family = "normal")
        ## display only numeric variables
        numIndices <- sapply(GUI$getActiveData(), function(x) !is_cat(x))
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
        add(mainGroup, helplyt)
        # add(mainGroup, lbl1)
        add(mainGroup, lbl2)
        add(mainGroup, numVar, expand = TRUE)
        add(mainGroup, StackButton)
        add(GUI$modWin, mainGroup, expand = TRUE, fill = TRUE)
        visible(GUI$modWin) <<- TRUE
      }
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
    type = "ANY",
    check = "ANY"
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
        mainGroup$set_borderwidth(15)
        helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
          browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/data_options/#reshape")
        })
        title_string <- glabel("Reshape Dateset")
        font(title_string) <- list(size = 14, weight = "bold")
        helplyt <- glayout(homegenous = FALSE)
        helplyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- title_string
        helplyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
        add(mainGroup, helplyt)
        
        format_string <- glabel("Select reshape mode", cont = mainGroup)
        format <- gcombobox(items = c("", "Wide to long", "Long to wide"), cont = mainGroup, handler = function(h, ...){
          type <<- svalue(format)
          newview$set_items("")
          visible(previewbox) <- TRUE
          visible(reshapebtn) <- TRUE
          if (type == "Wide to long"){
            visible(group1) <- TRUE
            visible(group2) <- FALSE
            check <<- "wide"
          } else if (type == "Long to wide") {
            visible(group2) <- TRUE
            visible(group1) <- FALSE
            check <<- "long"
          } else{
            visible(group1) <- FALSE
            visible(group2) <- FALSE
            visible(previewbox) <- FALSE
            visible(reshapebtn) <- FALSE
          }
        })
        
        ## Wide to long
        group1 <- ggroup(cont = mainGroup, horizontal = FALSE)
        
        col_string <- glabel("Select column(s) to gather together", cont = group1)
        
        colname <<- ""
        var1 <- gcombobox(c("", names(GUI$getActiveData())), cont = group1, handler = function(h, ...){
          colname <<- svalue(var1)
          if (colname == "") {
            newview$set_items("")
          } else {
            updatePreview()
          }
        })
        
        var2box <- gvbox(cont = group1)
        var2 <- gtable(names(GUI$getActiveData()),multiple = TRUE, expand = TRUE, cont = var2box)
        addHandlerSelectionChanged(var2, function(h, ...){
          colname <<- svalue(var2)
          updatePreview()
        })
        
        names(var2) <- "Variables"
        visible(var2box) <- FALSE
        size(var2box) <- c(-1, 150)
        
        checkbox <- gcheckbox(text = "Click to select multiple columns", cont = group1, handler = function(h, ...) {
          if (svalue(checkbox) == TRUE) {
            visible(var2box) <- TRUE
            visible(var1) <- FALSE
            colname <<- svalue(var2)
            newview$set_items("")
          } else {
            visible(var2box) <- FALSE
            visible(var1) <- TRUE
            colname <<- svalue(var1)
            newview$set_items("")
          }
        })
        
        key <<- "key"
        key_string <- glabel("Name the new column containing the old column names", cont = group1)
        keybox <- gedit("key", cont = group1)
        addHandlerKeystroke(keybox, function(h, ...) {
          key <<- ifelse(svalue(keybox) == "", "key", svalue(keybox))
          updatePreview()
        })
        
        value <<- "value"
        value_string <- glabel("Name the new column containing the old column values", cont = group1)
        valuebox <- gedit("value", cont = group1)
        addHandlerKeystroke(valuebox, function(h,...) {
          value <<- ifelse(svalue(valuebox) == "", "value", svalue(valuebox))
          updatePreview()
        })
        
        visible(group1) <- FALSE
        
        ## Long to wide
        group2 <- ggroup(cont = mainGroup, horizontal = FALSE)
        
        col1 <<- ""
        label1 <- glabel("Select the column to spread out to multiple columns", cont = group2)
        col1box <- gcombobox(items = c("", names(GUI$getActiveData())), cont = group2, handler = function(h, ...) {
          col1 <<- svalue(col1box)
          if (col1 != "" & col2 != "") {
            updatePreview()
          } else {
            newview$set_items("")
          }
        })
        
        col2 <<- ""
        label2 <- glabel("Select the column with the values to be put in these column", cont = group2)
        col2box <- gcombobox(items = c("", names(GUI$getActiveData())), cont = group2, handler = function(h,...) {
          col2 <<- svalue(col2box)
          if (col1 != "" & col2 != "") {
            updatePreview()
          } else {
            newview$set_items("")
          }
        })
        
        visible(group2) <- FALSE
        
        ## Preview window
        previewbox <- gvbox(cont = mainGroup)#, horizontal = TRUE, fill = TRUE)
        prevTbl <- glayout(homogeneous = FALSE, container = previewbox)
        
        string1 <- glabel("Original dataset")
        originview <- gtable(data.frame(GUI$getActiveData()))
        prevTbl[1,1, expand = TRUE] <- string1
        prevTbl[2,1, expand = TRUE] <- originview
        size(originview) = c(-1, 250)
        
        string2 <- glabel("New dataset")
        newview <<- gtable(data.frame(""))
        prevTbl[1,2, expand = TRUE] <- string2
        prevTbl[2,2, expand = TRUE] <- newview
        size(newview) <<- c(-1, 250)
        
        reshapebtn <- gbutton("Reshape", cont = mainGroup, handler = function(h, ...) {
          .dataset <- GUI$getActiveData()
          data <- reshape()
          attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "reshaped", sep = ".")
          attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
          GUI$setDocument(iNZDocument$new(data = data))
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
      .dataset <- GUI$getActiveData()
      df = iNZightTools::reshape_data(.dataset, col1, col2, colname, key, value, check)
    }
  )
)


## --------------------------------------------
## Class that handles the separating of a dataset
## --------------------------------------------
iNZSeparateDataWin <- setRefClass(
  "iNZSeparateDataWin",
  fields = list(
    GUI = "ANY",
    type = "ANY",
    col = "ANY",
    left = "ANY",
    right = "ANY",
    sep = "ANY",
    check = "ANY",
    newview = "ANY",
    varx = "ANY",
    num = "ANY",
    g2 = "ANY",
    box = "ANY",
    dtpreview = "ANY",
    scrollbox = "ANY",
    namelist = "ANY"
  ),
  methods = list(
    initialize = function(gui = NULL) {
      initFields(GUI = gui)
      if (!is.null(GUI)) {
        ## close any current mod windows
        try(dispose(GUI$modWin), silent = TRUE)
        
        ## start my window
        GUI$modWin <<- gwindow("Separate columns", parent = GUI$win, visible = FALSE)
        mainGroup <- ggroup(cont = GUI$modWin, expand = TRUE, horizontal = FALSE)
        mainGroup$set_borderwidth(15)
        helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
          browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/data_options/#separate")
        })

        title_string = glabel("Separate columns")
        font(title_string) = list(size = 14, weight = "bold")
        
        helplyt <- glayout(homegenous = FALSE)
        helplyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- title_string
        helplyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
        add(mainGroup, helplyt)
        
        format_string <- glabel("Select separate mode", cont = mainGroup)
        format <- gcombobox(items = c("", "Separate a column into several columns", "Separate a column to make several rows"), cont = mainGroup, handler = function(h, ...){
          col <<- ""
          sep <<- ""
          left <<- "col1"
          right <<- "col2"
          num <<- 2
          var1$set_value(" ")
          var2$set_value("")
          newview$set_items("")
          type <<- svalue(format)
          if (type == "Separate a column into several columns"){
            visible(namebox) <- TRUE
            check <<- "Column"
          } else if (type == "Separate a column to make several rows") {
            visible(namebox) <- FALSE
            check <<- "Row"
          } else{
            visible(namebox) <- FALSE
          }
        })
        
        col_string <- glabel("Select column to separate out", cont = mainGroup)
        
        var1 <- gcombobox(c(" ", names(GUI$getActiveData())), cont = mainGroup, handler = function(h, ...){
          col <<- svalue(var1)
          varx <<- GUI$getActiveData()[[col]]
          updateView()
        })
        
        sep_string <- glabel("Enter the separator between values", cont = mainGroup)
        var2 <- gedit("", cont = mainGroup)
        addHandlerKeystroke(var2, function(h ,...){
          sep <<- svalue(var2)
          updateView()
        })
        
        namelist <<- list()
        namebox <- ggroup(cont = mainGroup, horizontal = FALSE)
        g2 <<- gexpandgroup(container = namebox, horizontal = FALSE, text = "Change column names (Push enter to refresh the preview)")
        scrollbox <<- ggroup(cont = g2, horizontal = FALSE, use.scrollwindow = TRUE)
        box <<- glayout()
        scrollbox$add_child(box, fill = TRUE)
        visible(g2) <<- FALSE
        visible(namebox) <- FALSE
        
        prevTbl <- glayout(homogeneous = FALSE, container = mainGroup)
        
        string1 <- glabel("Original dataset")
        originview = gtable(data.frame(GUI$getActiveData()))
        prevTbl[1,1, expand = TRUE] <- string1
        prevTbl[2,1, expand = TRUE] <- originview
        size(originview) = c(-1, 250)
        
        string2 <- glabel("New dataset")
        newview <<- gtable(data.frame(""))
        prevTbl[1,2, expand = TRUE] <- string2
        prevTbl[2,2, expand = TRUE] <- newview
        size(newview) <<- c(-1, 250)
        
        seperatebtn <- gbutton("Separate", cont = mainGroup, handler = function(h, ...) {
          .dataset <- GUI$getActiveData()
          data <- separatedt()
          i <- 1
          while (i < length(namelist)) {
            old <- namelist[i]
            new <- namelist[i+1]
            colnames(data)[which(names(data) == old)] <- new
            i <- i + 2
          }
          attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "separated", sep = ".")
          attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
          GUI$setDocument(iNZDocument$new(data = data))
          dispose(GUI$modWin)
        })
        
        visible(GUI$modWin) <<- TRUE
      }
    },
    separatedt = function() {
      data <- GUI$getActiveData() %>% dplyr::select(col, dplyr::everything())
      if (check == "Column") {
        while (TRUE %in% grepl(sep, varx)) {
          data <- iNZightTools::separate(data, col, left, right, sep, check)
          col <- paste0("col", num)
          varx <- eval(parse(text = paste0("data$", col)))
          left <- paste0("col", num)
          right <- paste0("col", num + 1)
          num <- num + 1
        }
      } else if (check == "Row") {
        data <- iNZightTools::separate(data, col, left, right, sep, check)
      }
      return(data)
    },
    updateView = function(){
      if (col != " " & sep != "") {
        dtpreview <- separatedt()
        newview$set_items(dtpreview)
        scrollbox$remove_child(box)
        
        numcol = sum(grepl("^col[1-9]+$", names(dtpreview)))
        if (length(numcol) != 0) {
          box <<- glayout()
          scrollbox$add_child(box, fill = TRUE)
          
          update_name <- function(i, name) {
            force(i)
            force(name)
            
            function(h, ...) {
              new.name <- ifelse(svalue(box[i, 2]) == "", paste0("col", i), svalue(box[i, 2]))
              colnames(dtpreview)[which(names(dtpreview) == name)] <<- new.name
              newview$set_items(dtpreview)
              namelist <<- append(namelist, c(paste0("col", i), new.name))
            }
          }
          
          for (i in 1:numcol) {
            name <- paste0("col", i)
            box[i, 1] <<- glabel(paste0("Column ", i))
            box[i, 2] <<-gedit("", handler = update_name(i, name))
          }
        }
        
      } else {
        newview$set_items("")
      }
    }
  )
)



## --------------------------------------------
## Class that handles the uniting of a dataset
## --------------------------------------------
iNZUniteDataWin <- setRefClass(
  "iNZUniteDataWin",
  fields = list(
    GUI = "ANY",
    sep = "ANY",
    col = "ANY",
    name = "ANY",
    newview = "ANY"
  ),
  methods = list(
    initialize = function(gui = NULL) {
      initFields(GUI = gui)
      if (!is.null(GUI)) {
        ## close any current mod windows
        try(dispose(GUI$modWin), silent = TRUE)
        
        ## start my window
        GUI$modWin <<- gwindow("Unite columns", parent = GUI$win, visible = FALSE)
        mainGroup <- ggroup(cont = GUI$modWin, expand = TRUE, horizontal = FALSE)
        mainGroup$set_borderwidth(15)
        helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
          browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/data_options/#unite")
        })
        title_string <- glabel("Unite columns")
        font(title_string) <- list(size = 14, weight = "bold")
        helplyt <- glayout(homegenous = FALSE)
        helplyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- title_string
        helplyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
        add(mainGroup, helplyt)
        
        col_string <- glabel("Select columns to unite", cont = mainGroup)
        
        var1 <- gtable(names(GUI$getActiveData()),multiple = TRUE, expand = TRUE, cont = mainGroup)
        addHandlerSelectionChanged(var1, function(h, ...){
          col <<- svalue(var1)
          name <<- ""
          for (i in 1:length(col)) {
            n <- col[i]
            name <<- paste(name, n, sep = ".")
          }
          svalue(var2) <- substring(name, 2)
          updateView()
        })
        size(var1) <- c(-1, 150)
        
        var2 <- gedit("", cont = mainGroup)
        addHandlerKeystroke(var2, function(h, ...) {
          name <<- ifelse(svalue(var2) == "", "newcol", svalue(var2))
          updateView()
        })
        
        sep <<- "_"
        sep_string <- glabel("Enter the separator to use between values", cont = mainGroup)
        var3 <- gedit("_", cont = mainGroup)
        addHandlerKeystroke(var3, function(h, ...) {
          sep <<- svalue(var3)
          updateView()
        })
        
        prevTbl <- glayout(homogeneous = FALSE, container = mainGroup)
        
        string1 <- glabel("Original dataset")
        originview = gtable(data.frame(GUI$getActiveData()))
        prevTbl[1,1, expand = TRUE] <- string1
        prevTbl[2,1, expand = TRUE] <- originview
        size(originview) = c(-1, 250)
        
        string2 <- glabel("New dataset")
        newview <<- gtable(data.frame(""))
        prevTbl[1,2, expand = TRUE] <- string2
        prevTbl[2,2, expand = TRUE] <- newview
        size(newview) <<- c(-1, 250)
        
        unitebtn <- gbutton("Unite", cont = mainGroup, handler = function(h, ...) {
          .dataset <- GUI$getActiveData()
          data <- iNZightTools::unite(.dataset, name, col, sep)
          attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "united", sep = ".")
          attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
          GUI$setDocument(iNZDocument$new(data = data))
          dispose(GUI$modWin)
        })
        
        visible(GUI$modWin) <<- TRUE
      }
    },
    updateView = function() {
      data <- GUI$getActiveData()
      df <- iNZightTools::unite(data, name, col, sep)
      newview$set_items(df)
    }
  )
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
          numIndices <- sapply(dat, function(x) is_num(x))
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


## --------------------------------------------
## Class that handles the joining of the original dataset with a new dataset
## --------------------------------------------
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
    joinview = "ANY",
    coltbl = "ANY",
    middle = "ANY"
  ),
  methods = list(
    initialize = function(gui = NULL) {
      initFields(GUI = gui)
      if (!is.null(GUI)) {
        ## close any current mod windows
        try(dispose(GUI$modWin), silent = TRUE)
        
        ## start my window
        GUI$modWin <<- gwindow("Join with another dataset by column values",
                               parent = GUI$win, width = 550, visible = FALSE)
        mainGroup <- ggroup(cont = GUI$modWin, expand = TRUE, horizontal = FALSE)
        mainGroup$set_borderwidth(15)
        
        helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
          browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/data_options/#join")
        })
        title_string <- glabel("Join Datasets")
        font(title_string) <- list(size = 14, weight = "bold")
        helplyt <- glayout(homegenous = FALSE)
        helplyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- title_string
        helplyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
        add(mainGroup, helplyt)
        
        prevTbl <- glayout(homogeneous = FALSE, cont = mainGroup)
        
        string1 <- glabel("Preview of the original dataset")
        originview <- gtable(data.frame(head(GUI$getActiveData(), 10)))
        string2 <- glabel("Select join methods")
        jointypes <- list("Inner Join" = "inner_join", "Left Join" = "left_join", "Full Join" = "full_join", "Semi Join" = "semi_join", "Anti Join" = "anti_join")
        var1 <- gcombobox(
          items = names(jointypes), 
          selected = 2,
          handler = function(h, ...) {
            join_method <<- jointypes[[svalue(var1)]]
            updatePreview()
          })
	      
        join_method <<- "left_join"

        left_name_box <- gvbox()
        name_string <- glabel("Duplicated cols: suffix for Original", cont = left_name_box, anchor = c(-1, 0))
        left_name <<- "Orig"
        left_name_string <- gedit("Orig", cont = left_name_box)
        addHandlerKeystroke(left_name_string, function(h, ...) {
          left_name <<- svalue(left_name_string)
          updatePreview()
        })
        
        prevTbl[1,1, expand = TRUE] <- string1
        prevTbl[2,1, expand = TRUE] <- originview
        prevTbl[3,1, expand = TRUE] <- string2
        prevTbl[4,1, expand = TRUE] <- var1
        prevTbl[5,1, expand = TRUE] <- left_name_box
        size(originview) <- c(-1, 200)

        
        string3 <- glabel("Preview of the imported dataset")
        impview <- gtable(data.frame(""))
        string4 <- glabel("Import data")
        data_name <- gfilebrowse(text = "Specify a file", initial.dir = file.path(".", "data"), handler = function(h, ...) {
          newdata <<- iNZightTools::smart_read(svalue(data_name))
          impview$set_items(head(newdata, 10))

          left_col <<- ""
          right_col <<- ""

          d1 = tryCatch(
            joinData(),
            error = function(e) {
              if (e$message == "`by` required, because the data sources have no common variables") {
                a = tibble::tibble()
                attr(a, "join_cols") = ""
              }
            }
          )
          attr = attr(d1, "join_cols")
          left_col <<- as.character(attr)
          right_col <<- left_col
          
          create_join_table()
          updatePreview()
        })
        
        right_name_box = gvbox()
        name_string = glabel("Duplicated cols: suffix for New", cont = right_name_box, anchor = c(-1, 0))
        right_name <<- "New"
        right_name_string = gedit("New", cont = right_name_box)
        addHandlerKeystroke(right_name_string, function(h, ...) {
          right_name <<- svalue(right_name_string)
          updatePreview()
        })
        
        prevTbl[1,2, expand = TRUE] <- string3
        prevTbl[2,2, expand = TRUE] <- impview
        prevTbl[3,2, expand = TRUE] <- string4
        prevTbl[4,2, expand = TRUE] <- data_name
        prevTbl[5,2, expand = TRUE] <- right_name_box
        size(impview) <- c(-1, 200)
        
        ## Middle box
        middle <<- ggroup(cont = mainGroup, horizontal = FALSE)
        coltbl <<- glayout(cont = middle)
        coltbl[1, 1:4] <<- glabel("Please specify columns to match on from two datasets")
        
        ## Bottom box
        bottom = ggroup(cont = mainGroup, horizontal = FALSE)
        preview_string2 = glabel("Preview", cont = bottom, anchor = c(-1, 0))
        joinview <<- gtable(data.frame(""), cont = bottom)
        size(joinview) <<- c(-1, 150)
        
        joinbtn = gbutton("Join", cont = bottom)
        addHandlerChanged(joinbtn, function(h, ...) {
          .dataset <- GUI$getActiveData()
          data = joinData()
          if (length(data) == 0 | nrow(data) == 0) {data = GUI$getActiveData()}
          attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "joined", sep = ".")
          attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
          GUI$setDocument(iNZDocument$new(data = data))
          dispose(GUI$modWin)
        })
        
        helpbtn = gbutton("Help", cont = bottom, handler = function(h, ...) {
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
      d = joinData()
      if (length(d) == 0) return()
      if (nrow(d) == 0) {
        joinview$set_items("Joined dataset has 0 row")
      } else {
        d[is.na(d)] <- "NA"
        joinview$set_items(head(d, 10))
      }
    },
    joinData = function() {
      data <- GUI$getActiveData()
      if (length(left_col) != 0 & length(left_col) == length(right_col)) {
        ## checking for column types
        list <- list()
        for (i in 1:length(left_col)) {
          orig_type <- class(GUI$getActiveData()[[left_col[i]]])
          new_type <- class(newdata[[right_col[i]]])
          if (orig_type == new_type|orig_type == "character" & new_type == "factor"|orig_type == "factor" & new_type == "character") {
            list <- append(list, TRUE)
          } else {list <- append(list, FALSE)}
        }
        ## Now left_col contains some column namese and the mataching columns from two datasets are in the same class so JOIN
        if (all(list == TRUE)) {
          iNZightTools::joindata(
            GUI$getActiveData(),
            newdata,
            left_col,
            right_col,
            join_method,
            left_name,
            right_name
          )
        } else {
          joinview$set_items("Selected columns are of different types")
          return()
        }
      } else {
        joinview$set_items("Please specify columns to match on from two datasets")
        return()
      }
    },
    ## Create join table
    create_join_table = function() {
      if (length(coltbl$children) > 1) {
        middle$remove_child(coltbl)
        coltbl <<- glayout()
        coltbl[1, 1:4] <<- glabel("Please specify columns to match on from two datasets")
        middle$add_child(coltbl, fill = TRUE)
      }
      if (length(left_col) == 0) {
        add_joinby_row(coltbl, 1)
        joinview$set_items("Please specify columns to match on from two datasets")
        return()
      } else {
        for (i in 1:length(left_col)) {
          add_joinby_row(coltbl, i)
          number = i + 1
          coltbl[number, 1]$set_items(left_col[i])
          svalue(coltbl[number, 1]) <<- left_col[i]
          coltbl[number, 2]$set_items(right_col[i])
          svalue(coltbl[number, 2]) <<- right_col[i]
        }
      }
    },
    # Add joinby row
    add_joinby_row = function(coltbl, number) {
      n = number + 1
      coltbl[n, 1] <<- gcombobox(c("", setdiff(names(GUI$getActiveData()), left_col)), handler = function(h, ...) {
        new_col = svalue(coltbl[n,1])
        left_col[number] <<- new_col
        updatePreview()
      })
      coltbl[n, 2] <<- gcombobox(c("", setdiff(names(newdata), right_col)), handler = function(h, ...) {
        new_col = svalue(coltbl[n,2])
        right_col[number] <<- new_col
        updatePreview()
      })
      coltbl[n, 3] <<- gbutton('delete', handler = function(h, ...) {
        remove_joinby_row(coltbl, n, left_col)
      })
      coltbl[n, 4] <<- gbutton('add', handler = function(h, ...) {
        add_joinby_row(coltbl, length(left_col) + 1)
      })
    },
    ## Remove joinby row
    remove_joinby_row = function(coltbl, pos, left) {
      pos = pos - 1
      if (length(left_col) > 0) {
        left_col <<- left[-pos]
        right_col <<- right_col[-pos]
      }
      create_join_table()
    }
  )
)


## --------------------------------------------
## Class that handles appending new row to the dataset
## --------------------------------------------
iNZappendrowWin <- setRefClass(
  "iNZappendrowWin",
  fields = list(GUI = "ANY",
                newdata = "ANY",
                date = "ANY"),
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
        mainGroup$set_borderwidth(15)
        helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
          browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/data_options/#append")
        })
        title_string = glabel("Append rows")
        font(title_string) = list(size = 14, weight = "bold")
        helplyt <- glayout(homegenous = FALSE)
        helplyt[1, 4:19, expand = TRUE, anchor = c(0,0)] <- title_string
        helplyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn
        add(mainGroup, helplyt)
        file_string = glabel("Import data", cont = mainGroup, anchor = c(-1,0))
        data_name = gfilebrowse(text = "Specify a file", initial.dir = file.path(".", "data"), cont = mainGroup, handler = function(h, ...) {
          newdata <<- iNZightTools::smart_read(svalue(data_name))
        })
        
        date <<- FALSE
        check_box = gcheckbox("Tick if you want to attach a timestamp to the appended rows", cont = mainGroup, handler = function(h, ...) {
          date <<- svalue(check_box)
        })
        
        appendbtn = gbutton("Append", cont = mainGroup)
        addHandlerChanged(appendbtn, function(h, ...) {
          .dataset <- GUI$getActiveData()
          data = appendrow()
          attr(data, "name") <- paste(attr(.dataset, "name", exact = TRUE), "appended", sep = ".")
          attr(data, "code") <- gsub(".dataset", attr(.dataset, "name", exact = TRUE), attr(data, "code"))
          GUI$setDocument(iNZDocument$new(data = data))
          dispose(GUI$modWin)
        })
        
        visible(GUI$modWin) <<- TRUE
      }
    },
    appendrow = function() {
      data = GUI$getActiveData()
      oldcols = names(data)
      newcols = names(newdata)
      common = intersect(oldcols, newcols)
      if (length(common) != 0) {
        for (i in 1:length(common)) {
          colname = common[i]
          if (class(data[[colname]]) != class(newdata[[colname]])) {
            colnames(data)[which(names(data) == colname)] <- paste0(colname, class(data[[colname]]))
            colnames(newdata)[which(names(newdata) == colname)] <<- paste0(colname, class(newdata[[colname]]))
          }
        }
      }
      iNZightTools::appendrows(data, newdata, date)
    }
  ))

