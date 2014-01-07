iNZControlWidget <- setRefClass(
    "iNZControlWidget",
    fields = list(
        GUI = "ANY",
        ctrlGp = "ANY",
        sliderGp1 = "ANY",
        sliderGp2 = "ANY"
        ),
    methods = list(
        initialize = function(gui) {
            ctrlGp <<- ggroup(horizontal = FALSE)
            initFields(GUI = gui)
            ## The format is: glayout -> ggroup -> glayout -> ggroup
            ## buttons in glayouts, sliders in ggroups
            ## set up first glayout
            tbl <- glayout(expand = FALSE, cont = ctrlGp)
            tbl[3,1, anchor = c(0,0)] <- glabel(" Variable 1 :")
            tbl[5,1, anchor = c(0,0)] <- glabel(" Variable 2 :")
            tbl[7,1, anchor = c(0,0)] <- glabel(" subset by  :")
            tbl[9,1, anchor = c(0,0)] <- glabel(" subset by  :")
            tbl[3,3, anchor = c(0,0)] <- glabel("Drop name here")
            tbl[5,3, anchor = c(0,0)] <- glabel("Drop name here")
            tbl[7,3, anchor = c(0,0)] <- glabel("Drop name here")
            tbl[9,3, anchor = c(0,0)] <- glabel("Drop name here")
            tbl[3,7, anchor = c(0,0)] <- gbutton("clear",
                         handler = function(h,...) {
                             svalue(tbl[3,3]) <- "Drop name here"
                             changePlotSettings(list(x = NULL))
                         })
            tbl[5,7, anchor = c(0,0)] <- gbutton("clear",
                         handler=function(h,...) {
                             svalue(tbl[5,3]) <- "Drop name here"
                             changePlotSettings(list(y = NULL))
                         })
            tbl[7,7, anchor = c(0,0)] <- gbutton("clear",
                         handler=function(h,...) {
                             deleteSlider(8) # delete a slider in row 8 of the glayout
                             svalue(tbl[7,3]) <- "Drop name here"
                             changePlotSettings(list(g1 = NULL,
                                                     g1.level = NULL,
                                                     varnames = list(
                                                         g1 = NULL)
                                                     ))
                         })
            tbl[9,7, anchor = c(0,0)] <- gbutton("clear",
                         handler=function(h,...) {
                             deleteSlider(10) # delete a slider in row 10 of the glayout
                             svalue(tbl[9,3]) <- "Drop name here"
                             changePlotSettings(list(g2 = NULL,
                                                     g2.level = NULL,
                                                     varnames = list(
                                                         g2 = NULL)
                                                     ))
                         })
            ## change the font
            font(tbl[3,3]) <- list(weight="bold", family = "normal")
            font(tbl[5,3]) <- list(weight="bold", family = "normal")
            font(tbl[7,3]) <- list(weight="bold", family = "normal")
            font(tbl[9,3]) <- list(weight="bold", family = "normal")
            ## set up first slider
            sliderGp1 <<- ggroup(horizontal = FALSE, cont = ctrlGp)
            visible(sliderGp1) <<- FALSE
            ## add drop functionality to the fields
            addDropTarget(
                tbl[3,3],
                handler = function(h, ...) {
                    svalue(h$obj) <- h$dropdata
                    changePlotSettings(list(
                        x = GUI$getActiveDoc()$getData()[h$dropdata][[1]],
                        varnames = list(
                            x = colnames(GUI$getActiveDoc()$getData()[h$dropdata]))
                        ))
                })
            addDropTarget(
                tbl[5,3],
                handler = function(h, ...) {
                    svalue(h$obj) <- h$dropdata
                    changePlotSettings(list(
                        y = GUI$getActiveDoc()$getData()[h$dropdata][[1]],
                        varnames = list(
                            y = colnames(GUI$getActiveDoc()$getData()[h$dropdata]))
                        ))
                })
            ## slider 1
            addDropTarget(
                tbl[7,3],
                handler = function(h, ...) {
                    if (h$dropdata == svalue(tbl[9, 3]))
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
                            varnames = list(
                                g1 = colnames(GUI$getActiveDoc()$getData()[h$dropdata]))
                            ))
                    }
                })
            ## slider 2
            addDropTarget(
                tbl[9,3],
                handler = function(h, ...) {
                    if (h$dropdata == svalue(tbl[7, 3]))
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
                            varnames = list(
                                g2 = colnames(GUI$getActiveDoc()$getData()[h$dropdata]))
                            ))
                    }
                })
        },
        ## change the plotSettings
        changePlotSettings = function(setList) {
            GUI$getActiveDoc()$setSettings(setList)
        },
        createSlider = function(pos, dropdata) {
            ## create a ggroup for the slider at the specified
            ## pos in the glayout
            tbl <- ctrlGp$children[[1]]
            tbl[pos, 1:7, expand = TRUE] <- (sliderGrp <- ggroup(horizontal = FALSE))
            ## build the level names that are used for the slider
            grpData <- GUI$getActiveData()[dropdata][[1]]
            grpData <- iNZightPlots:::convert.to.factor(grpData)
            if (pos == 8) 
                lev <- c("_MULTI", levels(grpData))
            else
                lev <- c("_ALL", levels(grpData))
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
                lbl <- c("_ALL", lbl)
            add(sliderGrp, glabel(paste(lbl, collapse = "   ")))
        },
        deleteSlider = function(pos) {
            ## delete all the current children of sliderGrp
            if(ctrlGp$children[[1]]$get_dim()[1] >= pos &&
               class(ctrlGp$children[[1]][pos, 1]) == "GGroup")
                try(delete(ctrlGp$children[[1]],
                           ctrlGp$children[[1]][pos, 1]),
                    silent = TRUE)
        })
    )
