iNZControlWidget <- setRefClass(
    "iNZControlWidget",
    fields = list(
        GUI = "ANY",
        ctrlGp = "ANY",
        V1box = "ANY",
        V2box = "ANY",
        G1box = "ANY",
        G2box = "ANY",
        playButton = "list",
        playdelay = "numeric"
    ),
    methods = list(
        initialize = function(gui) {
            ctrlGp <<- ggroup(horizontal = FALSE)
            initFields(GUI = gui, playdelay = 0.6)
            ## set up glayout
            tbl <- glayout(expand = TRUE, homogeneous = FALSE, cont = ctrlGp, spacing = 5)

            clear_icon <- "clear"

            ### DRAG/DROP MENUS
            V1box <<- gcombobox(
                c("Select/Drag-drop Variable 1", colnames(GUI$getActiveData()))
            )
            V2box <<- gcombobox(
                c("Select/Drag-drop Variable 2", colnames(GUI$getActiveData()))
            )
            G1box <<- gcombobox(
                c("Select/Drag-drop Variable 3 (subset)", colnames(GUI$getActiveData()))
            )
            G2box <<- gcombobox(
                c("Select/Drag-drop Variable 4 (subset)", colnames(GUI$getActiveData()))
            )

            tbl[1L, 1:5, anchor = c(0,0), expand = TRUE] <- V1box
            tbl[3L, 1:5, anchor = c(0,0), expand = TRUE] <- V2box
            tbl[5L, 1:5, anchor = c(0,0), expand = TRUE] <- G1box
            tbl[7L, 1:5, anchor = c(0,0), expand = TRUE] <- G2box


            ### CLEAR BUTTONS

            ## -- Variable 1
            V1clearbtn <- gimagebutton(
                stock.id = "clear",
                tooltip = "Clear Variable",
                handler = function(h,...) {
                    svalue(V1box, index = TRUE) <<- 1L
                    changePlotSettings(list(x = NULL))
                }
            )
            ## V1clearbtn$set_icon("Cancel")
            tbl[1L, 7L, anchor = c(0, 0)] <- V1clearbtn

            ## -- Variable 2
            V2clearbtn <- gimagebutton(
                stock.id = "clear",
                tooltip = "Clear Variable",
                handler = function(h,...) {
                    svalue(V2box, index = TRUE) <<- 1L
                    changePlotSettings(
                        list(
                            y = NULL,
                            varnames = list(
                                y = NULL
                            )
                        ),
                        reset = { GUI$plotType != "dot" })
                }
            )
            tbl[3L, 7L, anchor = c(0, 0)] <- V2clearbtn

            ## -- Grouping Variable 1
            G1clearbtn <- gimagebutton(
                stock.id = "clear",
                tooltip = "Clear Variable",
                handler = function(h,...) {
                    svalue(G1box, index = TRUE) <<- 1L
                    ## change handler will handle the rest
                }
            )
            tbl[5L, 7L, anchor = c(0, 0)] <- G1clearbtn

            ## -- Grouping Variable 2
            G2clearbtn <- gimagebutton(
                stock.id = "clear",
                tooltip = "Clear Variable",
                handler = function(h,...) {
                    svalue(G2box, index = TRUE) <<- 1L
                }
            )
            tbl[7L, 7L, anchor = c(0, 0)] <- G2clearbtn


            ## "SWITCH" buttons:
            switchV12 <- gimagebutton("go-down",
                tooltip = "Switch with Variable 2")
            addHandlerClicked(switchV12,
                function(h, ...) {
                    if (svalue(V1box, TRUE) == 1L || svalue(V2box, TRUE) == 1L)
                        return()

                    V1 <- svalue(V1box)
                    V2 <- svalue(V2box)

                    blockHandlers(V1box)
                    blockHandlers(V2box)

                    svalue(V1box) <<- V2
                    svalue(V2box) <<- V1

                    valX <- svalue(V1box)
                    newX <- as.name(valX)
                    newXname <- valX

                    valY <- svalue(V2box)
                    newY <- as.name(valY)
                    newYname <- valY

                    changePlotSettings(
                        list(
                            x = newX,
                            y = newY,
                            xlab = NULL,
                            ylab = NULL,
                            main = NULL,
                            varnames = list(x = newXname, y = newYname)
                        ),
                        reset = TRUE
                    )
                    unblockHandlers(V1box)

                    unblockHandlers(V2box)
                }
            )
            switchV23 <- gimagebutton("go-down",
                tooltip = "Switch with Variable 3")
            addHandlerClicked(switchV23,
                function(h, ...) {
                    if (svalue(V2box, TRUE) == 1L && svalue(G1box, TRUE) == 1L)
                        return()

                    V2 <- svalue(V2box, TRUE)
                    G1 <- svalue(G1box, TRUE)

                    blockHandlers(V2box)
                    blockHandlers(G1box)

                    deleteSlider(pos = 6L)
                    svalue(V2box, TRUE) <<- G1
                    svalue(G1box, TRUE) <<- V2

                    if (svalue(V2box, TRUE) == 1L) {
                        valY <- NULL
                        newY <- NULL
                    } else {
                        valY <- svalue(V2box)
                        newY <- as.name(valY)
                    }
                    newYname <- valY

                    if (svalue(G1box, TRUE) == 1L) {
                        changePlotSettings(
                            list(
                                y = newY,
                                ylab = NULL,
                                xlab = NULL,
                                main = NULL,
                                g1 = NULL,
                                g1.level = NULL,
                                varnames = list(
                                    y = newYname,
                                    g1 = NULL
                                )
                            ),
                            reset = TRUE
                        )
                    } else {
                        valG1 <- svalue(G1box)
                        newG1 <- as.name(valG1)
                        newG1name <- valG1

                        createSlider(pos = 6L, valG1)
                        changePlotSettings(
                            list(
                                y = newY,
                                ylab = NULL,
                                xlab = NULL,
                                main = NULL,
                                g1 = newG1,
                                g1.level = "_MULTI",
                                varnames = list(
                                    y = newYname,
                                    g1 = newG1name
                                )
                            )
                        )
                    }

                    unblockHandlers(V2box)
                    unblockHandlers(G1box)
                }
            )
            switchV34 <- gimagebutton("go-down", tooltip = "Switch with Variable 4")
            addHandlerClicked(switchV34,
                function(h, ...) {
                    if (svalue(G1box, TRUE) == 1L && svalue(G2box, TRUE) == 1L)
                        return()

                    G1 <- svalue(G1box, TRUE)
                    G2 <- svalue(G2box, TRUE)

                    blockHandlers(G1box)
                    blockHandlers(G2box)

                    deleteSlider(pos = 6L)
                    deleteSlider(pos = 8L)
                    svalue(G2box, TRUE) <<- G1
                    svalue(G1box, TRUE) <<- G2

                    if (svalue(G1box, TRUE) == 1L) {
                        varG1 <- NULL
                        newG1 <- NULL
                        newG1name <- NULL
                        newG1level <- NULL
                    } else {
                        valG1 <- svalue(G1box)
                        newG1 <- as.name(valG1)
                        newG1name <- valG1
                        newG1level <- "_MULTI"
                        createSlider(pos = 6L, valG1)
                    }
                    if (svalue(G2box, TRUE) == 1L) {
                        varG2 <- NULL
                        newG2 <- NULL
                        newG2name <- NULL
                        newG2level <- NULL
                    } else {
                        valG2 <- svalue(G2box)
                        newG2 <- as.name(valG2)
                        newG2name <- valG2
                        newG2level <- "_ALL"
                        createSlider(pos = 8L, valG2)
                    }

                    changePlotSettings(
                        list(
                            main = NULL,
                            g1 = newG1,
                            g2 = newG2,
                            g1.level = newG1level,
                            g2.level = newG2level,
                            varnames = list(
                                g1 = newG1name,
                                g2 = newG2name
                            )
                        )
                    )

                    unblockHandlers(G1box)
                    unblockHandlers(G2box)
                }
            )

            tbl[1L, 6L] <- switchV12
            tbl[3L, 6L] <- switchV23
            tbl[5L, 6L] <- switchV34

            ## add drop functionality to the fields

            ## -- Variable 1
            addDropTarget(V1box,
                handler = function(h, ...) svalue(h$obj) <- h$dropdata
            )
            addHandlerChanged(V1box,
                handler = function(h, ...) {
                    if (svalue(V1box, TRUE) == 1L) {
                        newX <- NULL
                        newXname <- NULL
                    } else {
                        val <- svalue(V1box)
                        newX <- as.name(val)
                        newXname <- val
                    }

                    changePlotSettings(
                        list(
                            x = newX,
                            xlab = NULL,
                            main = NULL,
                            varnames = list(x = newXname)
                        ),
                        reset = TRUE
                    )
                }
            )
            ## -- Variable 2
            addDropTarget(V2box,
                handler = function(h, ...) svalue(h$obj) <- h$dropdata
            )
            addHandlerChanged(V2box,
                handler = function(h, ...) {
                    if (svalue(V2box, TRUE) == 1L) {
                        newY <- NULL
                        newYname <- NULL
                    } else {
                        val <- svalue(V2box)
                        newYname <- val
                        newY <- as.name(val)
                    }

                    changePlotSettings(list(
                        y = newY,
                        ylab = NULL,
                        main = NULL,
                        varnames = list(y = newYname)
                    ), reset = TRUE)
                }
            )
            ## slider 1
            addDropTarget(G1box,
                handler = function(h, ...) svalue(h$obj) <- h$dropdata
            )
            addHandlerChanged(
                G1box,
                handler = function(h, ...) {
                    if (svalue(G1box) == svalue(G2box)) {
                        svalue(G1box, index = TRUE) <<- 1L
                        gmessage(
                            "You cannot use the same variable in both subsetting slots.",
                            parent = GUI$win
                        )
                    } else {
                        deleteSlider(pos = 6L)
                        if (svalue(G1box, index = TRUE) > 1L) {
                            val <- svalue(G1box)
                            createSlider(pos = 6L, val)
                            changePlotSettings(
                                list(
                                    g1 = as.name(val),
                                    g1.level = "_MULTI",
                                    main = NULL,
                                    varnames = list(
                                        g1 = val
                                    )
                                )
                            )
                        } else {
                            changePlotSettings(
                                list(
                                    g1 = NULL,
                                    g1.level = NULL,
                                    varnames = list(
                                        g1 = NULL
                                    )
                                ),
                                reset = TRUE
                            )
                        }
                    }
                }
            )

            ## slider 2
            addDropTarget(G2box,
                handler = function(h, ...) svalue(h$obj) <- h$dropdata
            )
            addHandlerChanged(G2box,
                handler = function(h, ...) {
                    if (svalue(G2box) == svalue(G1box)) {
                        svalue(G2box, index = TRUE) <<- 1L
                        gmessage(
                            "You cannot use the same variable in both subsetting slots.",
                            parent = GUI$win
                        )
                    } else {
                        deleteSlider(pos = 8L)
                        if (svalue(G2box, index = TRUE) > 1L) {
                            val <- svalue(G2box)
                            createSlider(pos = 8L, val)
                            changePlotSettings(
                                list(
                                    g2 = as.name(val),
                                    g2.level = "_ALL",
                                    main = NULL,
                                    varnames = list(
                                        g2 = val
                                    )
                                )
                            )
                        } else {
                            changePlotSettings(
                                list(
                                    g2 = NULL,
                                    g2.level = NULL,
                                    varnames = list(
                                        g2 = NULL
                                    )
                                ),
                                reset = TRUE
                            )
                        }
                    }
                }
            )
        },
        ## change the plotSettings
        changePlotSettings = function(setList, reset = FALSE) {
            GUI$getActiveDoc()$setSettings(setList, reset)
        },
        updateVariables = function() {
            datavars <- colnames(GUI$getActiveData())

            v1 <- if (svalue(V1box) %in% datavars)
                which(datavars == svalue(V1box)) + 1L else 1L
            V1box$set_items(c(V1box$get_items()[1L], datavars))
            V1box$set_value(GUI$ctrlWidget$V1box$get_items()[v1])

            v2 <- if (svalue(V2box) %in% datavars)
                which(datavars == svalue(V2box)) + 1L else 1L
            V2box$set_items(c(V2box$get_items()[1L], datavars))
            V2box$set_value(GUI$ctrlWidget$V2box$get_items()[v2])

            g1 <- if (svalue(G1box) %in% datavars)
                which(datavars == svalue(G1box)) + 1L else 1L
            G1box$set_items(c(G1box$get_items()[1L], datavars))
            G1box$set_value(GUI$ctrlWidget$G1box$get_items()[g1])

            g2 <- if (svalue(G2box) %in% datavars)
                which(datavars == svalue(G2box)) + 1L else 1L
            G2box$set_items(c(G2box$get_items()[1L], datavars))
            G2box$set_value(GUI$ctrlWidget$G2box$get_items()[g2])
        },
        createSlider = function(pos, dropdata, index = 1L) {
            ## make sure there is no slider at the pos
            deleteSlider(pos)

            ## create a ggroup for the slider at the specified
            ## pos in the glayout
            tbl <- ctrlGp$children[[1L]]

            ## build the level names that are used for the slider
            grpData <- GUI$getActiveData()[dropdata][[1L]]
            grpData <- iNZightPlots:::convert.to.factor(grpData)
            if (pos == 6L)
                lev <- c("_MULTI", levels(grpData))
            else
                lev <- c("_ALL", levels(grpData), "_MULTI")
            lev <- factor(lev, levels = lev)
            slider <- gslider(from = lev, value = index)

            #add(sliderGrp, slider, expand = FALSE)
            if (pos == 6L)
                grp = "g1"
            else
                grp = "g2"
            ## update the plot settings whenever the slider changes
            addHandlerChanged(slider,
                handler = function(h, ...) {
                    changePlotSettings(
                        structure(
                            list(as.character(svalue(h$obj))),
                            .Names = paste(grp, "level", sep = ".")
                        )
                    )
                }
            )

            ## Play button
            PLAY <- function(data) {
                playButton$levi <<- playButton$levi + 1L
                if (playButton$levi > playButton$Nlev) {
                    playButton$playtimer$stop_timer()
                    playBtn$set_value(img.playicon)
                    playButton$playtimer <<- NULL
                } else {
                    changePlotSettings(
                        structure(list(playButton$levi),
                            .Names = paste(grp, "level", sep = ".")
                        )
                    )
                    ri <- playButton$row
                    tb <- ctrlGp$children[[1L]][ri, 1L]
                    blockHandlers(tb)
                    ## This line creates "IA__gtk_table_attach: assertion 'child->parent == NULL' failed" error.
                    svalue(tb, index = TRUE) <- playButton$levi + 1L
                    unblockHandlers(tb)
                }
            }
            clickPlay <- function(h, ...) {
                if (!is.null(playButton$playtimer)) {
                    ## time is running - so stop the animation
                    playButton$playtimer$stop_timer()
                    playBtn$set_value(img.playicon)
                    playButton$playtimer <<- NULL
                    return()
                }
                oldSet <- GUI$getActiveDoc()$getSettings()
                playBtn$set_value(img.stopicon)
                pr <- h$obj$parent
                wc <- which(sapply(pr$child_positions,
                    function(x) identical(h$obj, x$child)))
                playButton <<- list(
                    playtimer = NULL,
                    row = pr$child_positions[[wc]]$x,
                    Nlev = length(levels(grpData)),
                    levi = 0L,
                    oldSet = oldSet
                )
                PLAY(oldSet)
                playButton$playtimer <<- gtimer(playdelay * 1000, PLAY,
                    data = oldSet,
                    one.shot = FALSE
                )
            }
            img.playicon <- system.file("images/icon-play.png",
                package = "iNZight")
            img.stopicon <- system.file("images/icon-stop.png",
                package = "iNZight")
            playBtn <- gimagebutton(
                filename = img.playicon,
                size = "button",
                handler = clickPlay,
                tooltip = "Play through levels"
            )

            ## Play time delay - time in milliseconds
            img.clockicon <- system.file("images/icon-clock.png",
                package = "iNZight")
            delayBtn <- gimagebutton(
                filename = img.clockicon,
                size = "button",
                tooltip = "Set play timing options",
                handler = function(h, ...) {
                    w <- gwindow(
                        title = "Play Settings",
                        width = 200,
                        height = 80,
                        parent = GUI$win
                    )
                    g <- gvbox(spacing = 10, container = w)
                    g$set_borderwidth(10)

                    g1 <- ggroup(container = g)
                    glabel("Time delay between plots :", container = g1)
                    spin <- gspinbutton(from = 0.1, to = 3, by = 0.1,
                        value = playdelay,
                        container = g1
                    )
                    glabel("(seconds)", container = g1)

                    g2 <- ggroup(container = g)
                    addSpring(g2)
                    gbutton("OK",
                        container = g,
                        handler = function(h, ...) {
                            playdelay <<- svalue(spin)
                            dispose(w)
                        }
                    )
                }
            )
            delaySpin <- gspinbutton(from = 0.1, to = 3, by = 0.1,
                value = playdelay,
                handler = function(h, ...)
                    playdelay <<- svalue(h$obj)
            )

            ## Add things to layout:
            tbl[pos, 1:5, expand = TRUE] <- slider
            tbl[pos, 6L, anchor = c(0, 0), expand = FALSE] <- delayBtn
            tbl[pos, 7L, anchor = c(0, 0), expand = FALSE] <- playBtn
        },
        deleteSlider = function(pos) {
            ## get the child that is at the specified positions
            childPos <- which(
                sapply(
                    ctrlGp$children[[1L]]$child_positions,
                    function(x) x$x == pos
                )
            )
            while(length(childPos) > 0L) {
                ## delete all the current children of sliderGrp
                try(
                    {
                        ctrlGp$children[[1L]]$remove_child(
                            ctrlGp$children[[1L]]$child_positions[[childPos[1L]]]$child
                        )
                        childPos <- which(
                            sapply(ctrlGp$children[[1L]]$child_positions,
                            function(x) x$x == pos)
                        )
                    },
                    silent = TRUE)
            }
        },
        ## reset the widget to its original state
        ## (same as triggering all 4 clear buttons)
        resetWidget = function() {
            invisible(
                sapply(c(1L, 3L, 5L, 7L),
                    function(x) ctrlGp$children[[1L]][x, 7L]$invoke_change_handler()
                )
            )
        },
        setState = function(set) {
            updateVariables()

            data <- GUI$getActiveData()
            vars <- names(data)

            if (!is.null(set$x) && as.character(set$x) %in% vars) {
                ## set variable 1 to whatever it's supposed to be
                blockHandlers(V1box)
                svalue(V1box) <<- as.character(set$x)
                unblockHandlers(V1box)
                set$x <- set$x
            } else {
                ## remove variable 1
                set$x <- NULL
            }
            if (!is.null(set$y) && as.character(set$y) %in% vars) {
                ## set variable 2 to whatever it's supposed to be
                blockHandlers(V2box)
                svalue(V2box) <<- as.character(set$y)
                unblockHandlers(V2box)
                set$y <- set$y
            } else {
                ## remove variable 1
                set$y <- NULL
            }
            if (!is.null(set$g1) && as.character(set$g1) %in% vars) {
                ## set variable 3 to whatever it's supposed to be
                svalue(G1box) <<- as.character(set$g1)
                set$g1 <- set$g1
                g1level <- set$g1.level
                set$g1.level <- NULL
                sld1 <- ctrlGp$children[[1L]][6L, 1L]
                if (!is.null(g1level) &&
                    g1level %in% levels(svalue(sld1))) {
                    svalue(sld1) <- g1level
                    set$g1.level <- g1level
                }
            } else {
                ## remove variable 3
                set$g1 <- NULL
                set$g1.level <- NULL
            }
            if (!is.null(set$g2) && as.character(set$g2) %in% vars) {
                ## set variable 3 to whatever it's supposed to be
                svalue(G2box) <<- as.character(set$g2)
                set$g2 <- set$g2
                g2level <- set$g2.level
                set$g2.level <- NULL
                sld2 <- ctrlGp$children[[1L]][8L, 1L]
                if (!is.null(g2level) &&
                    g2level %in% levels(svalue(sld2))) {
                    svalue(sld2) <- g2level
                    set$g2.level <- g2level
                }
            } else {
                ## remove variable 3
                set$g2 <- NULL
                set$g2.level <- NULL
            }

            ## other things ...
            if (!is.null(set$sizeby) && as.character(set$sizeby) %in% vars) {
                set$sizeby <- set$sizeby
            } else {
                set$sizeby <- NULL
            }
            if (!is.null(set$colby) && as.character(set$colby) %in% vars) {
                set$colby <- set$colby
            } else {
                set$colby <- NULL
            }
            if (!is.null(set$symbolby) && as.character(set$symbolby) %in% vars) {
                set$symbolby <- set$symbolby
            } else {
                set$symbolby <- NULL
            }

            GUI$getActiveDoc()$setSettings(set)
            GUI$updatePlot()
        }
    ) ## methods
) ## setRefClass
