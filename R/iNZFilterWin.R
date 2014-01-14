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
               GUI$modWin <<- gwindow("Filter Dataset", parent = GUI$win,
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
                        idx <- GUI$getActiveData()[[svalue(factorMenu)]] %in%
                        svalue(factorLvls)
                        GUI$getActiveDoc()$getModel()$updateData(
                            GUI$getActiveData()[idx, ])
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
                    subsetExpression <- paste(svalue(numMenu),
                                              svalue(operator),
                                              gsub(pattern = '\\n+', "",
                                                   svalue(expr),
                                                   perl = TRUE))
                    subsetData <- try(
                        subset(GUI$getActiveData(),
                               eval(parse(text = eval(subsetExpression))))
                        )
                    if(class(subsetData)[1] == "try-error"){
                        gmessage(title = "ERROR",
                                 msg = "Error in expression!",
                                 icon = "error",
                                 parent = GUI$modWin)
                    } else {
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
                    rowNumbers <- try(
                        as.numeric(strsplit(gsub(pattern = '\\s+',
                                                 replacement = "",
                                                 svalue(unwantedObs),
                                                 perl = TRUE),
                                            ",", fixed = TRUE)[[1]])
                        )
                    if (inherits(rowNumbers,"try-error") ||
                        is.na(rowNumbers)) {
                        gmessage(title = "ERROR",
                                 msg = "Error in typed values.\nCheck for missing commas or non-existing Row.names",
                                 icon = "error",
                                 parent = GUI$modWin)
                    } else {
                        if(!all(rowNumbers %in%
                                as.numeric(row.names(GUI$getActiveData()))))
                            gmessage(title = "ERROR",
                                     msg = "You have entered one or more non-existing Row.names",
                                     icon = "error",
                                     parent = GUI$modWin)
                        else {
                            GUI$getActiveDoc()$getModel()$updateData(
                                GUI$getActiveData()[-rowNumbers, ])
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
            sampleSize <- gedit("", width = 4)
            submitButton <- gbutton(
                "Submit",
                handler = function(h, ...) {
                    sSize <- as.numeric(svalue(sampleSize))
                    if (is.na(sSize) || sSize > nrow(GUI$getActiveData()))
                        gmessage(title = "ERROR",
                                 msg = "Please specify a valid number for the sample size",
                                 icon = "error",
                                 parent = GUI$modWin)
                    else {
                        rdmSample <- sample(1:nrow(GUI$getActiveData()),
                                            size = sSize)
                        GUI$getActiveDoc()$getModel()$updateData(
                            GUI$getActiveData()[rdmSample, ])
                        dispose(GUI$modWin)                              
                    }
                })            
            tbl <- glayout()
            tbl[1,1:2] <- lbl1
            tbl[2, 1] <- "Total number of rows:"
            tbl[2, 2] <- glabel(nrow(GUI$getActiveData()))
            tbl[3, 1] <- "Sample Size:"
            tbl[3, 2] <- sampleSize
            add(mainGrp, lbl1)
            add(mainGrp, tbl)
            add(mainGrp, btnGrp)
            addSpring(btnGrp)
            add(btnGrp, submitButton)
            visible(GUI$modWin) <<- TRUE
        })
    )
