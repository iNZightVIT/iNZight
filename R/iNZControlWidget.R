iNZControlWidget <- setRefClass(
    "iNZControlWidget",
    fields = list(
        GUI = "ANY",
        ctrlGp = "ANY",
        V1box = "ANY",
        V2box = "ANY",
        G1box = "ANY",
        G2box = "ANY"
        ),
    methods = list(
        initialize = function(gui) {
            ctrlGp <<- ggroup(horizontal = FALSE)
            initFields(GUI = gui)
            ## set up glayout
            tbl <- glayout(expand = TRUE, homogeneous = FALSE, cont = ctrlGp, spacing = 5)

            ### DRAG/DROP MENUS

            V1box <<- gcombobox(c("Select/Drag-drop Variable 1", colnames(GUI$getActiveData())))
            V2box <<- gcombobox(c("Select/Drag-drop Variable 2", colnames(GUI$getActiveData())))
            G1box <<- gcombobox(c("Select/Drag-drop Subset Variable 1", colnames(GUI$getActiveData())))
            G2box <<- gcombobox(c("Select/Drag-drop Subset Variable 2", colnames(GUI$getActiveData())))

            tbl[1,1:5, anchor = c(0,0), expand = TRUE] <- V1box
            tbl[3,1:5, anchor = c(0,0), expand = TRUE] <- V2box
            tbl[5,1:5, anchor = c(0,0), expand = TRUE] <- G1box
            tbl[7,1:5, anchor = c(0,0), expand = TRUE] <- G2box


            ### CLEAR BUTTONS

            ## -- Variable 1
            V1clearbtn <- gimagebutton(stock.id = "cancel",
                                  handler = function(h,...) {
                                      svalue(V1box, index = TRUE) <<- 1
                                      changePlotSettings(list(x = NULL))
                                  })
            ## V1clearbtn$set_icon("Cancel")
            tbl[1,7, anchor = c(0,0)] <- V1clearbtn

            ## old_cursor <- getToolkitWidget(GUI$win)$getWindow()$getCursor()
            ## cross <- gdkCursorNew("GDK_HAND1")
            ## addHandler(switchV12, "enter-notify-event", handler=function(h,...) {
            ##                getToolkitWidget(switchV12)$getWindow()$setCursor(cross)
            ##                TRUE
            ##            })
            ## addHandler(switchV12, "leave-notify-event", handler=function(h,...) {
            ##                getToolkitWidget(switchV12)$getWindow()$setCursor(old_cursor)
            ##                TRUE
            ##            })

            ## -- Variable 2
            V2clearbtn <- gimagebutton(stock.id = "cancel",
                                  handler = function(h,...) {
                                      svalue(V2box, index = TRUE) <<- 1
                                      changePlotSettings(list(y = NULL,
                                                              varnames = list(
                                                                  y = NULL)),
                                                         reset = { GUI$plotType != "dot" })
                                  })
            ## V2clearbtn$set_icon("Cancel")
            tbl[3,7, anchor = c(0,0)] <- V2clearbtn

            ## -- Grouping Variable 1
            G1clearbtn <- gimagebutton(stock.id = "cancel",
                                  handler = function(h,...) {
                                      svalue(G1box, index = TRUE) <<- 1
                                      ## change handler will handle the rest
                                  })
            tbl[5,7, anchor = c(0,0)] <- G1clearbtn

            ## -- Grouping Variable 2
            G2clearbtn <- gimagebutton(stock.id = "cancel",
                                  handler = function(h,...) {
                                      svalue(G2box, index = TRUE) <<- 1
                                  })
            tbl[7,7, anchor = c(0,0)] <- G2clearbtn


            ## "SWITCH" buttons:
            switchV12 <- gimagebutton("go-down")
            addHandlerClicked(switchV12, function(h, ...) {
                                  if (svalue(V1box, TRUE) == 1 || svalue(V2box, TRUE) == 1) {
                                      gmessage("Need variable selected in both boxes", icon = "error")
                                      return()
                                  }
                                  
                                  V1 <- svalue(V1box)
                                  V2 <- svalue(V2box)
                                  
                                  blockHandlers(V1box)
                                  blockHandlers(V2box)
                                  
                                  svalue(V1box) <<- V2
                                  svalue(V2box) <<- V1
                                  
                                  valX <- svalue(V1box)
                                  newX <- GUI$getActiveDoc()$getData()[valX][[1]]
                                  newXname <- valX
                                  
                                  valY <- svalue(V2box)
                                  newY <- GUI$getActiveDoc()$getData()[valY][[1]]
                                  newYname <- valY
                                  
                                  changePlotSettings(list(
                                      x = newX, y = newY,
                                      xlab = NULL, ylab = NULL,
                                      main = NULL,
                                      varnames = list(x = newXname, y = newYname)
                                      ), reset = TRUE)
                                  
                                  unblockHandlers(V1box)
                                  unblockHandlers(V2box)
                              })
            switchV23 <- gimagebutton("go-down")

            tbl[1, 6] <- switchV12
            tbl[3, 6] <- switchV23

            ## add drop functionality to the fields

            ## -- Variable 1
            addDropTarget(
                V1box,
                handler = function(h, ...) {
                    svalue(h$obj) <- h$dropdata
                })
            addHandlerChanged(
                V1box,
                handler = function(h, ...) {
                    if (svalue(V1box, TRUE) == 1) {
                        newX <- NULL
                        newXname <- NULL
                    } else {
                        val <- svalue(V1box)
                        newX <- GUI$getActiveDoc()$getData()[val][[1]]
                        newXname <- val
                    }
                    
                    changePlotSettings(list(
                        x = newX,
                        xlab = NULL,
                        main = NULL,
                        varnames = list(x = newXname)
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
                    if (svalue(V2box, TRUE) == 1) {
                        newY <- NULL
                        newYname <- NULL
                    } else {
                        val <- svalue(V2box)
                        newY <- GUI$getActiveDoc()$getData()[val][[1]]
                        newYname <- val
                    }
                    
                    changePlotSettings(list(
                        y = newY,
                        ylab = NULL,
                        main = NULL,
                        varnames = list(y = newYname)
                    ), reset = TRUE)
                })
            ## slider 1
            addDropTarget(
                G1box,
                handler = function(h, ...) {
                    svalue(h$obj) <- h$dropdata
                })
            addHandlerChanged(
                G1box,
                handler = function(h, ...) {
                    if (svalue(G1box) == svalue(G2box)) {
                        svalue(G1box, index = TRUE) <<- 1
                        gmessage("You cannot use the same variable in both subsetting slots.",
                                 parent = GUI$win)
                    } else {
                        deleteSlider(pos = 6)
                        if (svalue(G1box, index = TRUE) > 1) {
                            val <- svalue(G1box)
                            createSlider(pos = 6, val)
                            changePlotSettings(list(
                                g1 = iNZightPlots:::convert.to.factor(
                                    GUI$getActiveDoc()$getData()[val][[1]]
                                    ),
                                g1.level = "_MULTI",
                                main = NULL,
                                varnames = list(
                                    g1 = val)
                                ))
                        } else {
                            changePlotSettings(list(g1 = NULL,
                                                    g1.level = NULL,
                                                    varnames = list(
                                                        g1 = NULL)
                                                    ), reset = TRUE)
                        }
                    }
                })

            ## slider 2
            addDropTarget(
                G2box,
                handler = function(h, ...) {
                    svalue(h$obj) <- h$dropdata
                })
            addHandlerChanged(
                G2box,
                handler = function(h, ...) {
                    if (svalue(G2box) == svalue(G1box)) {
                        svalue(G2box, index = TRUE) <<- 1
                        gmessage("You cannot use the same variable in both subsetting slots.",
                                 parent = GUI$win)
                    } else {
                        deleteSlider(pos = 8)
                        if (svalue(G2box, index = TRUE) > 1) {
                            val <- svalue(G2box)
                            createSlider(pos = 8, val)
                            changePlotSettings(list(
                                g2 = iNZightPlots:::convert.to.factor(
                                    GUI$getActiveDoc()$getData()[val][[1]]
                                    ),
                                g2.level = "_ALL",
                                main = NULL,
                                varnames = list(
                                    g2 = val)
                                ))
                        } else {
                            changePlotSettings(list(g2 = NULL,
                                                    g2.level = NULL,
                                                    varnames = list(
                                                        g2 = NULL)
                                                    ), reset = TRUE)
                        }
                    }
                })
        },
        ## change the plotSettings
        changePlotSettings = function(setList, reset = FALSE) {
            GUI$getActiveDoc()$setSettings(setList, reset)
        },
        updateVariables = function() {
            datavars <- colnames(GUI$getActiveData())

            v1 <- if (svalue(V1box) %in% datavars) which(datavars == svalue(V1box)) + 1 else 1
            V1box$set_items(c(V1box$get_items()[1], datavars))
            V1box$set_value(GUI$ctrlWidget$V1box$get_items()[v1])

            v2 <- if (svalue(V2box) %in% datavars) which(datavars == svalue(V2box)) + 1 else 1
            V2box$set_items(c(V2box$get_items()[1], datavars))
            V2box$set_value(GUI$ctrlWidget$V2box$get_items()[v2])

            g1 <- if (svalue(G1box) %in% datavars) which(datavars == svalue(G1box)) + 1 else 1
            G1box$set_items(c(G1box$get_items()[1], datavars))
            G1box$set_value(GUI$ctrlWidget$G1box$get_items()[g1])

            g2 <- if (svalue(G2box) %in% datavars) which(datavars == svalue(G2box)) + 1 else 1
            G2box$set_items(c(G2box$get_items()[1], datavars))
            G2box$set_value(GUI$ctrlWidget$G2box$get_items()[g2])
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
            tbl[pos, 1:4, expand = TRUE] <- (hzGrp <- ggroup(fill = "x"))

            sliderGrp <- ggroup(horizontal = FALSE)

            ## build the level names that are used for the slider
            grpData <- GUI$getActiveData()[dropdata][[1]]
            grpData <- iNZightPlots:::convert.to.factor(grpData)
            if (pos == 6)
                lev <- c("_MULTI", levels(grpData))
            else
                lev <- c("_ALL", levels(grpData), "_MULTI")
            lev <- factor(lev, levels = lev)
            slider <- gslider(from = lev,
                              value = 1)
            add(sliderGrp, slider, expand = FALSE)
            if (pos == 6)
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
            if (pos == 6)
                lbl <- c("_MULTI", lbl)
            else
                lbl <- c("_ALL", lbl, "_MULTI")
            ## only add label if it is short enough
            if (sum(nchar(lbl)) + 3 * length(lbl) < 50)
                add(sliderGrp, glabel(paste(lbl, collapse = "   ")))

            ## Play button
            playBtn <- gbutton("Play", expand = FALSE,
                               handler = function(h, ...) {
                                   h$obj$set_value("Stop")
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
                                   h$obj$set_value("Play")
                                changePlotSettings(oldSet)
                            })
            #playBtn$set_icon("go")
            add(hzGrp, sliderGrp, expand = TRUE)
            #add(hzGrp, playBtn, expand = FALSE, anchor = c(0, 0))
            tbl[pos, 6:7, anchor = c(0, 0), expand = FALSE] <- playBtn


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
            while(length(childPos) > 0) {
                ##childPos <- names(ctrlGp$children[[1]]$child_positions)[[childPos]]
                ## delete all the current children of sliderGrp
                try({
                    ctrlGp$children[[1]]$remove_child(
                        ctrlGp$children[[1]]$child_positions[[childPos[1]]]$child)
                    childPos <- which(sapply(ctrlGp$children[[1]]$child_positions,
                                             function(x) x$x == pos))
                }, silent = TRUE)
            }
        },
        ## reset the widget to its original state
        ## (same as triggering all 4 clear buttons)
        resetWidget = function() {
            invisible(sapply(c(1,3,5,7), function(x) {
                ctrlGp$children[[1]][x, 7]$invoke_change_handler()
            }))
        })
    )
