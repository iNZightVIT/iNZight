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
                      originalD <- GUI$getActiveDoc()$getModel()$origDataSet
                      ActiveData <- GUI$getActiveData()
#                      attr(ActiveData, "tag") <- "filtered"
                      idx <- GUI$getActiveData()[[svalue(factorMenu)]] %in%
                        svalue(factorLvls)
                      GUI$setDocument(iNZDocument$new(data = originalD))
                      GUI$getActiveDoc()$getModel()$updateData(
                        droplevels(ActiveData[idx,]))
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
                  originalD <- GUI$getActiveDoc()$getModel()$origDataSet
                  ActiveData <- GUI$getActiveData()
                  subsetExpression <- paste(svalue(numMenu),
                                            svalue(operator),
                                            gsub(pattern = '\\n+', "",
                                                 svalue(expr),
                                                 perl = TRUE))
                  subsetData <- try(
                    droplevels(subset(GUI$getActiveData(),
                           eval(parse(text = eval(subsetExpression)))))
                  )
                  if(class(subsetData)[1] == "try-error"){
                    gmessage(title = "ERROR",
                             msg = "Error in expression!",
                             icon = "error",
                             parent = GUI$modWin)
                  } else {
                    
                    GUI$setDocument(iNZDocument$new(data = originalD))
                    GUI$getActiveDoc()$getModel()$updateData(
                      subsetData)
                    dispose(GUI$modWin)
                    }
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
                  originalD <- GUI$getActiveDoc()$getModel()$origDataSet
                  ActiveData <- GUI$getActiveData()
                  rowNumbers <- try(
                    strsplit(gsub(pattern = '\\s+',
                                  replacement = "",
                                  svalue(unwantedObs),
                                  perl = TRUE),
                             ",", fixed = TRUE)[[1]]
                  )
                  if (inherits(rowNumbers,"try-error") ||
                        is.na(rowNumbers)) {
                    gmessage(title = "ERROR",
                             msg = "Error in typed values.\nCheck for missing commas or non-existing Row.names",
                             icon = "error",
                             parent = GUI$modWin)
                  } else {
                    ranges <- grep(":", rowNumbers)
                    if (length(ranges) > 0) {
                      rowRanges <- rowNumbers[ranges]
                      rowNumbers <- as.numeric(rowNumbers[-ranges])
                      rowRanges <- as.vector(sapply(
                        rowRanges, function(m) eval(parse(text=m))))
                      rowNumbers <- unique(c(rowNumbers, rowRanges))
                    }
                    if(!all(rowNumbers %in%
                              as.numeric(row.names(GUI$getActiveData()))))
                      gmessage(title = "ERROR",
                               msg = "You have entered one or more non-existing Row.names",
                               icon = "error",
                               parent = GUI$modWin)
                    else {
                      GUI$setDocument(iNZDocument$new(data = originalD))
                      idx <- !rownames(ActiveData) %in% rowNumbers
                      ## please notice iNZdataViewWidget/createDfView to show small number of row even when the original data is large
                      GUI$getActiveDoc()$getModel()$updateData(
                        droplevels(ActiveData[idx, ]))
                      # so after the above step, we may update the gdf panel in iNZdataViewWidget
                      dispose(GUI$modWin)
                    }
                  }
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
            numSample <- gspinbutton(from = 1, to = 99999, by = 1)
            sampleSize <- gedit("", width =4)
            submitButton <- gbutton(
                "Submit",
                handler = function(h, ...) {
                  originalD <- GUI$getActiveDoc()$getModel()$origDataSet
                  ActiveData <- GUI$getActiveData()
                  sSize <- as.numeric(svalue(sampleSize))
                  if (svalue(numSample) == 1){ 
                    if (is.na(sSize) || sSize > nrow(GUI$getActiveData()))
                      gmessage(title = "ERROR",
                               msg = "Number of Samples X Sample Size cannot exceed Total 
                                 number of rows",
                               icon = "error",
                               parent = GUI$modWin)
                    else {
                      rdmSample <- sample(1:nrow(GUI$getActiveData()),
                                          size = sSize)
                      GUI$setDocument(iNZDocument$new(data = originalD))
                      GUI$getActiveDoc()$getModel()$updateData(
                        droplevels(ActiveData[rdmSample, ]))
                      dispose(GUI$modWin)
                    }
                  }
                  else{
                    if (is.na(sSize) || sSize > nrow(GUI$getActiveData()))
                      gmessage(title = "ERROR",
                               msg = "T",
                               icon = "error",
                               parent = GUI$modWin)
                    else {
                      rdmSample <- numeric(0)
                      
                      
                      if (sSize*svalue(numSample) < nrow(GUI$getActiveData()))
                        rdmSample = sample(1:nrow(GUI$getActiveData()), 
                                           size = sSize*svalue(numSample))
                      else 
                        return(gmessage(title = "ERROR",
                                        msg = "The total sample number is greater than the sample size",
                                        icon = "error",
                                        parent = GUI$modWin))
                      
                      GUI$setDocument(iNZDocument$new(data = originalD))
                      GUI$getActiveDoc()$getModel()$updateData(
                        droplevels(ActiveData[rdmSample, ]))
                      
                      newVar <- as.character(rep(1:svalue(numSample) , each = sSize))
                      
                      newNames <- "Sample Number"
                      
                      insertData(data = newVar,
                                 name = newNames,
                                 index = ncol(GUI$getActiveData()),
                                 msg = list(
                                   msg = "The new variables are added to the end of the dataset",
                                   icon = "info"
                                 ),
                                 closeAfter = TRUE)
                      
                      
                      
                      #dispose(GUI$modWin)
                    }
                  }
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
                        GUI$getActiveDoc()$getModel()$updateData(
                            wide.to.tall(GUI$getActiveData()))
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
        },
        wide.to.tall = function(data) {
            varnames <- names(data)
            values <- unlist(data)
            names(values) <- NULL
            group.labels <- factor(rep(varnames, each = nrow(data)), levels = names(data))
            data <- data.frame(group.labels, values)
            names(data) <- c("group", "value")
            data
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
            dataSet <- GUI$getActiveData()
            argList <- list()
            for (i in 1:4) {
              con <- eval(parse(text=paste(c("svalue(droplist_var",i, ")"), collapse="")))
              con2 <- eval(parse(text=paste(c("svalue(radio_var",i, ")"), collapse="")))
              if (con != "") {
                datai <- dataSet[, con]
                if (inherits(datai, "character") | inherits(datai, "factor"))
                  datai <- xtfrm(datai)
                if (con2 != "increasing")
                  datai <- -datai
                argList[[length(argList) + 1]] <- datai
              }
            }
            
            if (identical(argList,list()))
              return(gmessage("Select at leat one variable!",
                              parent = GUI$win))
            idx <- do.call("order", argList)
            GUI$getActiveDoc()$getModel()$updateData(
              GUI$getActiveData()[idx, ])
            dispose(GUI$modWin)
            
          })
        
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
            originalD <- GUI$getActiveDoc()$getModel()$origDataSet
            dataSet <- GUI$getActiveData()
            con <- c(svalue(droplist_var1), svalue(droplist_var2), svalue(droplist_var3))
            if (all(con %in% ""))
              return(gmessage("Select at leat one variable!",
                              parent = GUI$win))
            con <- con[which(con!="")]
            con2 <- c("mean","median","sum","sd","IQR","length")[svalue(func.table, index=TRUE)]
            if(length(con2) == 0)
              return(gmessage("Select at leat one function!",
                              parent = GUI$win))
            
          
            out <- ddply(na.omit(dataSet), as.quoted(con), numcolwise(con2[1]))
            nameVec <- c(con, paste0(names(Filter(is.numeric, dataSet)), ".", con2[1]))
            names(out) <- nameVec 
            
            if (length(con2) > 1) {
              for (i in 2:length(con2)){
                out1 <- ddply(na.omit(dataSet), as.quoted(con), numcolwise(con2[i]))  
                nameVec <- c(con, paste0(names(Filter(is.numeric, dataSet)), ".", con2[i]))
                names(out1) <- nameVec 
                out = join(out,out1, by=con)
              }
            }
            id = sort(names(out)[-(1:length(con))])
            GUI$setDocument(iNZDocument$new(data = originalD))
            GUI$getActiveDoc()$getModel()$updateData(out[, c(con, id)])
            
            
            dispose(GUI$modWin)
              
            
          })
        label_var1 <- glabel("1st")
        label_var2 <- glabel("2nd")
        label_var3 <- glabel("3rd")
        droplist_var1 <- gcombobox(c("",nameList), selected = 1)
        droplist_var2 <- gcombobox(c("",nameList), selected= 1)
        droplist_var3 <- gcombobox(c("",nameList), selected = 1)
        func.frame <- data.frame(c("Mean", "Median", "Sum", "Sd", "IQR", "Count"))
        names(func.frame) <- "Summeries: "
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
            measure.vars <- svalue(numVar)
            dat <- GUI$getActiveData()
            out <- reshape2:::melt.data.frame(dat, measure.vars = measure.vars, 
                                              variable.name = "stack.variable",
                                              value.name = "stack.value")
            GUI$getActiveDoc()$getModel()$updateData(out)
            
            
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
