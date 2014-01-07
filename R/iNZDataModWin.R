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
            if (!is.null(GUI)) {
               try(dispose(GUI$modWin), silent = TRUE) ## close any current mod windows
               GUI$modWin <<- gwindow(parent = GUI$win,
                                     visible = FALSE)
            }
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
                ## insert the new variable in the column after the old variable
                ## or at the end if the old variable is the last column in the
                ## data
                if (index != length(names(GUI$getActiveData()))) {
                    newData <- data.frame(
                        GUI$getActiveData()[, 1:index],
                        out,
                        GUI$getActiveData()[, (index+1):ncol(GUI$getActiveData())]
                        )
                    names(newData)[(index+1):ncol(newData)] <- c(
                        name,
                        names(GUI$getActiveData()[, (index+1):ncol(GUI$getActiveData())])
                        )
                } else {
                    newData <- data.frame(GUI$getActiveData(), out)
                    names(newData)[ncol(newData)] <- name
                }

                gmessage(title = "INFO",
                         msg = paste("The new variable", name,
                             "will be inserted next to",
                             names(GUI$getActiveData()[index]),
                             "in the dataset"),
                         icon = "info",
                         parent = GUI$modWin)

                GUI$getActiveDoc()$getModel()$updateData(newData)
                dispose(GUI$modWin)
            }
        })
    )
