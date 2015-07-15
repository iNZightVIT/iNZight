iNZControlWidget <- setRefClass(
    "iNZControlWidget",
    fields = list(
        GUI = "ANY",
        ctrlGp = "ANY",
        V1box = "ANY",
        V2box = "ANY"
        ),
    methods = list(
        initialize = function(gui) {
            ctrlGp <<- ggroup(horizontal = FALSE)
            initFields(GUI = gui)
            ## set up glayout
            tbl <- glayout(expand = FALSE, cont = ctrlGp)


            V1box <<- gcombobox(c("Select or Drag/drop Variable 1", colnames(GUI$getActiveData())))
            V2box <<- gcombobox(c("Select or Drag/drop Variable 2", colnames(GUI$getActiveData())))
            
            tbl[3,1:5, anchor = c(0,0), expand = TRUE] <- V1box
            tbl[5,1:5, anchor = c(0,0), expand = TRUE] <- V2box

            #tbl[3,1, anchor = c(0,0)] <- glabel(" Variable 1 :")
            #tbl[5,1, anchor = c(0,0)] <- glabel(" Variable 2 :")
            tbl[7,1, anchor = c(0,0)] <- glabel(" subset by  :")
            tbl[9,1, anchor = c(0,0)] <- glabel(" subset by  :")
            #tbl[3,3, anchor = c(0,0)] <- (xlbl <- glabel("Drop name here"))
            #tbl[5,3, anchor = c(0,0)] <- (ylbl <- glabel("Drop name here"))
            tbl[7,3, anchor = c(0,0)] <- (g1lbl <- glabel("Drop name here"))
            tbl[9,3, anchor = c(0,0)] <- (g2lbl <- glabel("Drop name here"))

            ### CLEAR BUTTONS

            ## -- Variable 1
            V1clearbtn <- gbutton("",
                                  handler = function(h,...) {
                                      svalue(V1box, index = TRUE) <<- 1
                                      changePlotSettings(list(x = NULL))
                                  })
            V1clearbtn$set_icon("Cancel")#, size = "menu")
            tbl[3,7, anchor = c(0,0)] <- V1clearbtn

            ## -- Variable 2
            V2clearbtn <- gbutton("",
                                  handler = function(h,...) {
                                      svalue(V2box, index = TRUE) <<- 1
                                      changePlotSettings(list(y = NULL,
                                                              varnames = list(
                                                                  y = NULL)),
                                                         reset = { GUI$plotType != "dot" })
                                  })
            V2clearbtn$set_icon("Cancel")#, size = "menu")
            tbl[5,7, anchor = c(0,0)] <- V2clearbtn

            ## -- Grouping Variable 1
            tbl[7,7, anchor = c(0,0)] <- gbutton("clear",
                         handler=function(h,...) {
                             deleteSlider(8) # delete a slider in row 8 of the glayout
                             svalue(g1lbl) <- "Drop name here"
                             changePlotSettings(list(g1 = NULL,
                                                     g1.level = NULL,
                                                     varnames = list(
                                                         g1 = NULL)
                                                     ), reset = TRUE)
                         })
            tbl[9,7, anchor = c(0,0)] <- gbutton("clear",
                         handler=function(h,...) {
                             deleteSlider(10) # delete a slider in row 10 of the glayout
                             svalue(g2lbl) <- "Drop name here"
                             changePlotSettings(list(g2 = NULL,
                                                     g2.level = NULL,
                                                     varnames = list(
                                                         g2 = NULL)
                                                     ), reset = TRUE)
                         })
            ## change the font
            #font(xlbl) <- list(weight="bold", family = "normal")
            #font(ylbl) <- list(weight="bold", family = "normal")
            #font(g1lbl) <- list(weight="bold", family = "normal")
            #font(g2lbl) <- list(weight="bold", family = "normal")
            
            ### add drop functionality to the fields

            ## -- Variable 1
            addDropTarget(
                V1box,
                handler = function(h, ...) {
                    svalue(h$obj) <- h$dropdata
                })
            addHandlerChanged(
                V1box,
                handler = function(h, ...) {
                    if (svalue(h$obj, index = TRUE) == 1) {
                        newX <- NULL
                        newXname <- NULL
                    } else {
                        newX <- GUI$getActiveDoc()$getData()[svalue(h$obj)][[1]]
                        newXname <- colnames(GUI$getActiveDoc()$getData()[svalue(h$obj)])
                    }
                    
                    changePlotSettings(list(
                        x = newX,
                        xlab = NULL,
                        main = NULL,
                        varnames = list(
                            x = newXname)
                        ), reset = TRUE)
                })

            ## -- Variable 2
            addDropTarget(
                V2box,
                handler = function(h, ...) {
                    svalue(h$obj) <- h$dropdata
                })
            addHandlerChanged(
                V2box,
                handler = function(h, ...) {
                    do.reset <- TRUE
                    
                    if (svalue(h$obj, index = TRUE) == 1) {
                        newY <- NULL
                        newYname <- NULL
                        do.reset <- TRUE
                    } else {
                        newY <- GUI$getActiveDoc()$getData()[svalue(h$obj)][[1]]
                        newYname <- colnames(GUI$getActiveDoc()$getData()[svalue(h$obj)])
                        
                        if (GUI$plotType == "dot") 
                            if (is.factor(newY))
                                do.reset <- FALSE
                    }                           
                    
                    changePlotSettings(list(
                        y = newY,
                        ylab = NULL,
                        main = NULL,
                        varnames = list(y = newYname)
                        ), reset = do.reset)
                })
            
            ## slider 1
            addDropTarget(
                g1lbl,
                handler = function(h, ...) {
                    if (h$dropdata == svalue(g2lbl))
                        gmessage("STOP! You are trying to use the same variable in both subsetting slots",
                                 parent = GUI$win)
                    else {
                        deleteSlider(pos = 8)
                        svalue(h$obj) <- h$dropdata
                        createSlider(pos = 8, h$dropdata)
                        changePlotSettings(list(
                            g1 = iNZightPlots:::convert.to.factor(
                                GUI$getActiveDoc()$getData()[h$dropdata][[1]]
                                ),
                            g1.level = "_MULTI",
                            main = NULL,
                            varnames = list(
                                g1 = colnames(GUI$getActiveDoc()$getData()[h$dropdata]))
                            ))
                    }
                })
            ## slider 2
            addDropTarget(
                g2lbl,
                handler = function(h, ...) {
                    if (h$dropdata == svalue(g1lbl))
                        gmessage("STOP! You are trying to use the same variable in both subsetting slots",
                                 parent = GUI$win)
                    else {
                        deleteSlider(pos = 10)
                        svalue(h$obj) <- h$dropdata
                        createSlider(pos = 10, h$dropdata)
                        changePlotSettings(list(
                            g2 = iNZightPlots:::convert.to.factor(
                                GUI$getActiveDoc()$getData()[h$dropdata][[1]]
                                ),
                            g2.level = "_ALL",
                            main = NULL,
                            varnames = list(
                                g2 = colnames(GUI$getActiveDoc()$getData()[h$dropdata]))
                            ))
                    }
                })
        },
        ## change the plotSettings
        changePlotSettings = function(setList, reset = FALSE) {
            GUI$getActiveDoc()$setSettings(setList, reset)
        },
        updateVariables = function() {
            V1box$set_items(c("Select or Drag/drop Variable 1", colnames(GUI$getActiveData())))
            V1box$set_value(GUI$ctrlWidget$V1box$get_items()[1])
            
            V2box$set_items(c("Select or Drag/drop Variable 2", colnames(GUI$getActiveData())))
            V2box$set_value(GUI$ctrlWidget$V2box$get_items()[1])
        },
        createSlider = function(pos, dropdata) {
            ## make sure there is no slider at the pos
            deleteSlider(pos)

            ## ##################################
            ## ## This is a workaround for the current bug in
            ## ## gWidgets2RGtk2. Remove this code once the bug
            ## ## is fixed! Comes in 2 parts
            ## ##################################
            ## if (pos == 8) {
            ##     childPos <- which(sapply(ctrlGp$children[[1]]$child_positions,
            ##                          function(x) x$x == 10))
            ##     if (length(childPos) > 0) {
            ##         g2Data <- svalue(ctrlGp$children[[1]][9, 3])
            ##         deleteSlider(10)
            ##     }
            ## }
            ## ##################################
            ## ## End of woraround part1
            ## ##################################

            ## create a ggroup for the slider at the specified
            ## pos in the glayout
            tbl <- ctrlGp$children[[1]]
            tbl[pos, 1:7, expand = TRUE] <- (hzGrp <- ggroup(fill = "x"))

            sliderGrp <- ggroup(horizontal = FALSE)
            
            ## build the level names that are used for the slider
            grpData <- GUI$getActiveData()[dropdata][[1]]
            grpData <- iNZightPlots:::convert.to.factor(grpData)
            if (pos == 8)
                lev <- c("_MULTI", levels(grpData))
            else
                lev <- c("_ALL", levels(grpData), "_MULTI")
            lev <- factor(lev, levels = lev)
            slider <- gslider(from = lev,
                              value = 1)
            add(sliderGrp, slider, expand = FALSE)
            if (pos == 8)
                grp = "g1"
            else
                grp = "g2"
            ## update the plot settings whenever the slider changes
            addHandlerChanged(slider, handler = function(h, ...) {
                              changePlotSettings(
                                  structure(list(
                                      as.character(svalue(h$obj))),
                                            .Names = paste(
                                                grp,
                                                "level",
                                                sep = ".")
                                            )
                                  )
                          })
            lbl <- levels(grpData)
            ## if the level names are too long, replace them with nr
            if (sum(nchar(lbl)) > 42)
                lbl <- 1:length(lbl)
            ## add * or _ to beginning of labels
            if (pos == 8)
                lbl <- c("_MULTI", lbl)
            else
                lbl <- c("_ALL", lbl, "_MULTI")
            ## only add label if it is short enough
            if (sum(nchar(lbl)) + 3 * length(lbl) < 50)
                add(sliderGrp, glabel(paste(lbl, collapse = "   ")))

            ## Play button
            playBtn <- gbutton("play", expand = FALSE,
                            handler = function(h, ...) {
                                oldSet <- GUI$getActiveDoc()$getSettings()
                                for (i in 1:length(levels(grpData))) {
                                    changePlotSettings(
                                        structure(list(i),
                                                  .Names = paste(
                                                      grp,
                                                      "level",
                                                      sep = ".")
                                                  )
                                        )
                                  # This effectively freezes the R session,
                                  # and therefore iNZight --- so increase with
                                  # discression!!!!!
                                    Sys.sleep(0.6)
                                }
                                changePlotSettings(oldSet)
                            })
            add(hzGrp, sliderGrp, expand = TRUE)
            add(hzGrp, playBtn, expand = FALSE, anchor = c(0, 0))
                                         

            ## ##################################
            ## ## start of workaround part2
            ## ##################################
            ## if (exists("g2Data")) {
            ##     createSlider(10, g2Data)
            ## }
            ## ##################################
            ## ## end of workaround part2
            ## ##################################
        },
        deleteSlider = function(pos) {
            ## get the child that is at the specified positions
            childPos <- which(sapply(ctrlGp$children[[1]]$child_positions,
                                     function(x) x$x == pos))
            if(length(childPos) > 0) {
                ##childPos <- names(ctrlGp$children[[1]]$child_positions)[[childPos]]
                ## delete all the current children of sliderGrp
                try(
                    ctrlGp$children[[1]]$remove_child(
                        ctrlGp$children[[1]]$child_positions[[childPos]]$child),
                    silent = TRUE)
            }
        },
        ## reset the widget to its original state
        ## (same as triggering all 4 clear buttons)
        resetWidget = function() {
            invisible(sapply(c(3,5,7,9), function(x) {
                ctrlGp$children[[1]][x, 7]$invoke_change_handler()
            }))
        })
    )
