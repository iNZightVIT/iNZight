## --------------------------------------------
## The super class for the data modification window
## When a new data modification window is opened,
## a current one is closed if it exists
## --------------------------------------------

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
                    names(newData)[(index+1):ncol(newData)] <- c(
                        name,
                        names(GUI$getActiveData()[, (index+1):ncol(GUI$getActiveData())])
                        )
                } else {
                    newData <- data.frame(GUI$getActiveData(), data)
                    names(newData)[ncol(newData)] <- name
                }

                if (!is.null(msg)) 
                    do.call(gmessage, msg)

                GUI$getActiveDoc()$getModel()$updateData(newData)
                if (closeAfter)
                    dispose(GUI$modWin)            
        })
    )


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
                                  svalue(name.txt) <- paste(h$dropdata, ".g", sep = "")
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
                out <- as.factor(varData)
                name <- gsub('\\n+', "", name, perl = TRUE)
                index <- which(names(GUI$getActiveData()) == orgVar)

                msg <- list(title = "INFO",
                            msg = paste("The new variable", name,
                                "will be inserted next to",
                                names(GUI$getActiveData()[index]),
                                "in the dataset"),
                            icon = "info",
                            parent = GUI$modWin)
                insertData(out, name, index, msg)
            }
        })
    )

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
        ## type: the function used for transformation
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
