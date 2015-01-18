iNZMapModWin <- setRefClass(
    "iNZMapModWin",
    fields = list(
        GUI = "ANY",
        optGrp = "ANY",
        mainGrp = "ANY",
        activeData = "data.frame",
        lonVar = "character",
        latVar = "character",
        varNames = "character"
    ),
    methods = list(
        initialize = function(GUI) {
            initFields(GUI = GUI)
            
            source("../iNZightMaps/R/isGeoData.R")
            checkGeoData()
            
            activeData <<- GUI$getActiveData()
            lonVar <<- getLon(activeData)
            latVar <<- getLat(activeData)
            varNames <<- names(activeData)
            
            ## NOTE: 
            ##   ggmap uses [0, 360] for longitudes instead of [-180,180],
            ##   so add 360 to negative longitudes to convert them.
            activeData[activeData[[lonVar]] < 0, lonVar] <<-
                activeData[activeData[[lonVar]] < 0, lonVar] + 360
            
            ## MORE FIELDS
            labFont = list(weight = "bold", family = "normal",size = 10)
            frameFont = list(weight = "bold")
            
            mainGrp <<- gvbox(spacing = 10, container = GUI$moduleWindow,
                              expand = TRUE)
            # size(mainGrp) = c(300, 600)
            # w = gwindow(width = 300, height = 600)
            # mainGrp = gvbox(spacing = 10, use.scrollwindow = TRUE,
            #               container = w)
            mainGrp$set_borderwidth(15)
            
            ## go back button
            goBackButtonAction = gaction(
                "Back",
                tooltip = "Click to go back to the data view screen",
                # icon = "gtk-go-back",
                handler = function(h,...) {
                    visible(GUI$moduleWindow) <<- FALSE
                    visible(GUI$gp1) <<- TRUE
                }
            )
            goBackButton = gbutton(action = goBackButtonAction)
            # size(goBackButton) = c(250, 28)
            # add(mainGrp, goBackButton, expand = TRUE)
            
            #################################
            ###  set up frame containers  ###
            #################################
            g1 = gframe("Longitude & Latitude", pos = 0.5, 
                        horizontal = FALSE, container = mainGrp)
            g2 = gframe("Colour by", pos = 0.5,
                        horizontal = FALSE, container = mainGrp)
            g3 = gframe("Size by", pos = 0.5,
                        horizontal = FALSE, container = mainGrp)
            g4 = gexpandgroup("Advanced settings", horizontal = FALSE,
                              container = mainGrp)
            
            label1 = getToolkitWidget(mainGrp)$
                getChildren()[[1]]$getChildren()[[2]]
            label2 = getToolkitWidget(mainGrp)$
                getChildren()[[2]]$getChildren()[[2]]
            label3 = getToolkitWidget(mainGrp)$
                getChildren()[[3]]$getChildren()[[2]]
            label4 = getToolkitWidget(mainGrp)$
                getChildren()[[4]]$getChildren()[[2]]
            
            mainGrp$set_rgtk2_font(label1, frameFont)
            mainGrp$set_rgtk2_font(label2, frameFont)
            mainGrp$set_rgtk2_font(label3, frameFont)
            mainGrp$set_rgtk2_font(label4, frameFont)
            
            g1$set_borderwidth(10)
            g2$set_borderwidth(10)
            g3$set_borderwidth(10)
            g4$set_borderwidth(3)
            
            ############
            ###  g1  ###
            ############
            g1Layout = glayout(container = g1)#, spacing = 20)
            
            ###  labels  ###
            g1_lab1 = glabel("from")
            g1_lab2 = glabel("to")
            g1_lab3 = glabel("fom")
            g1_lab4 = glabel("to")
            
            font(g1_lab1) = labFont
            font(g1_lab2) = labFont
            font(g1_lab3) = labFont
            font(g1_lab4) = labFont
            
            ###  options  ###
            if (hasLon(activeData) & hasLat(activeData)) {
                lon = which(varNames == lonVar) + 1
                lat = which(varNames == latVar) + 1
                
                datLon = activeData[is.finite(activeData[, lonVar]), lonVar]
                datLat = activeData[is.finite(activeData[, latVar]), latVar]
                
                lonFrom = min(datLon, na.rm = TRUE)
                
                lonMax = max(datLon, na.rm = TRUE)
                lonTo = ifelse(lonMax > 180, 180, lonMax)
                
                latFrom = min(datLat, na.rm = TRUE)
                latTo = max(datLat, na.rm = TRUE)
            } else {
                lon = 1
                lat = 1
                lonFrom = latFrom = 0
                lonTo = latTo = 1
            }
            
            ## option 1: longitude
            g1_opt1a = gcombobox(c("", varNames), selected = lon)
            g1_opt1b = gspinbutton(from = lonFrom, to = lonTo, 
                                     by = 0.01, value = lonFrom)
            g1_opt1c = gspinbutton(from = lonFrom, to = lonTo, 
                                   by = 0.01, value = lonTo)
            
            ## option 2: latitude
            g1_opt2a = gcombobox(c("", varNames), selected = lat)
            g1_opt2b = gspinbutton(from = latFrom, to = latTo, 
                                     by = 0.01, value = latFrom)
            g1_opt2c = gspinbutton(from = latFrom, to = latTo, 
                                   by = 0.01, value = latTo)
            
            g1Layout[2, 1, expand = TRUE, anchor = c(1, 0)] = g1_lab1
            g1Layout[2, 3, expand = TRUE, anchor = c(1, 0)] = g1_lab2
            g1Layout[4, 1, expand = TRUE, anchor = c(1, 0)] = g1_lab3
            g1Layout[4, 3, expand = TRUE, anchor = c(1, 0)] = g1_lab4
            
            g1Layout[1, 1:4, expand = TRUE] = g1_opt1a
            g1Layout[3, 1:4, expand = TRUE] = g1_opt2a
            g1Layout[2, 2, expand = TRUE] = g1_opt1b
            g1Layout[2, 4, expand = TRUE] = g1_opt1c
            g1Layout[4, 2, expand = TRUE] = g1_opt2b
            g1Layout[4, 4, expand = TRUE] = g1_opt2c
            
            ## NOTE: they need to be deleted and created again because
            ##       their ranges (initialised) may be different to new ones.
            ##       (ie, cannot choose 200 from [0, 100] range)
            addHandlerChanged(g1_opt1a, handler = function(h,...) {
                var = svalue(h$obj)
                if (!(var %in% varNames)) { return() }
                
                ## delete current display
                replicate(2, delete(g1Layout, g1Layout$children[[7]]))
                ## reset indices
                for (i in length(g1Layout$children):7) {
                    g1Layout$children[[i + 2]] = g1Layout$children[[i]]
                }
                
                if (is.numeric(activeData[, var])) {
                    dat = activeData[is.finite(activeData[[var]]), var]
                    lonMin = min(dat, na.rm = TRUE)
                    lonMax = max(dat, na.rm = TRUE)
                    
                    g1_opt1b = gspinbutton(from = lonMin, to = lonMax,
                                           by = 0.01, value = lonMin)
                    g1_opt1c = gspinbutton(from = lonMin, to = lonMax,
                                           by = 0.01, value = lonMax)
                } else {
                    g1_opt1b = gspinbutton(
                        from = -180, to = 180, by = 0.01, value = -180)
                    g1_opt1c = gspinbutton(
                        from = -180, to = 180, by = 0.01, value = 180)
                }
                
                ## add
                g1Layout[2, 2, expand = TRUE] = g1_opt1b
                g1Layout[2, 4, expand = TRUE] = g1_opt1c
                
                ## update
                g1Layout$children[[8]] = g1_opt1b
                g1Layout$children[[7]] = g1_opt1c
            })
            
            addHandlerChanged(g1_opt2a, handler = function(h,...) {
                var = svalue(h$obj)
                if (!(var %in% varNames)) { return() }
                
                ## delete current display
                replicate(2, delete(g1Layout, g1Layout$children[[9]]))
                ## reset indices
                for (i in length(g1Layout$children):9) {
                    g1Layout$children[[i + 2]] = g1Layout$children[[i]]
                }
                
                if (is.numeric(activeData[, var])) {
                    dat = activeData[is.finite(activeData[[var]]), var]
                    latMin = min(dat, na.rm = TRUE)
                    latMax = max(dat, na.rm = TRUE)
                    
                    g1_opt2b = gspinbutton(from = latMin, to = latMax,
                                           by = 0.01, value = latMin)
                    g1_opt2c = gspinbutton(from = latMin, to = latMax,
                                           by = 0.01, value = latMax)
                } else {
                    g1_opt2b = gspinbutton(
                        from = -180, to = 180, by = 0.01, value = -180)
                    g1_opt2c = gspinbutton(
                        from = -180, to = 180, by = 0.01, value = 180)
                }
                
                ## add
                g1Layout[4, 2, expand = TRUE] = g1_opt2b
                g1Layout[4, 4, expand = TRUE] = g1_opt2c
                
                ## update
                g1Layout$children[[10]] = g1_opt2b
                g1Layout$children[[9]] = g1_opt2c
            })
            
            ############
            ###  g2  ###
            ############
            g2Layout = glayout(container = g2)#, spacing = 20)
            
            ###  labels  ###
            g2_lab1 = glabel("from")
            g2_lab2 = glabel("to")
            
            font(g2_lab1) = labFont
            font(g2_lab2) = labFont
            
            ###  options  ###
            g2_opt1a = gcombobox(c("", varNames))
            g2_opt1b = gspinbutton(from = 0, to = 1, 
                                   length.out = 10, value = 0)
            g2_opt1c = gspinbutton(from = 0, to = 1, 
                                   length.out = 10, value = 0)
            
            
            g2Layout[2, 1, expand = TRUE, anchor = c(1, 0)] = g2_lab1
            g2Layout[2, 3, expand = TRUE, anchor = c(1, 0)] = g2_lab2
            
            g2Layout[1, 1:4, expand = TRUE] = g2_opt1a
            g2Layout[2, 2, expand = TRUE] = g2_opt1b
            g2Layout[2, 4, expand = TRUE] = g2_opt1c
            
            addHandlerChanged(g2_opt1a, handler = function(h,...) {
                var = svalue(h$obj)
                if (!(var %in% varNames)) { return() }
                
                ## delete current display
                replicate(2, delete(g2Layout, g2Layout$children[[4]]))
                ## reset indices
                for (i in length(g2Layout$children):4) {
                    g2Layout$children[[i + 2]] = g2Layout$children[[i]]
                }
                
                if (is.numeric(activeData[, var])) {
                    dat = activeData[is.finite(activeData[[var]]), var]
                    varMin = min(dat, na.rm = TRUE)
                    varMax = max(dat, na.rm = TRUE)
                    
                    g2_opt1b = gspinbutton(from = varMin, to = varMax,
                                           by = 0.01, value = varMin)
                    g2_opt1c = gspinbutton(from = varMin, to = varMax,
                                           by = 0.01, value = varMax)
                } else {
                    g2_opt1b = gspinbutton(
                        from = -180, to = 180, by = 0.01, value = -180)
                    g2_opt1c = gspinbutton(
                        from = -180, to = 180, by = 0.01, value = 180)
                }
                
                ## add
                g2Layout[2, 2, expand = TRUE] = g2_opt1b
                g2Layout[2, 4, expand = TRUE] = g2_opt1c
                
                ## update
                g2Layout$children[[5]] = g2_opt1b
                g2Layout$children[[4]] = g2_opt1c
            })
            
            
            
            ############
            ###  g3  ###
            ############
            g3Layout = glayout(container = g3)#, spacing = 20)
            
            ###  labels  ###
            g3_lab1 = glabel("from")
            g3_lab2 = glabel("to")
            
            font(g3_lab1) = labFont
            font(g3_lab2) = labFont
            
            ###  options  ###
            g3_opt1a = gcombobox(c("", varNames))
            g3_opt1b = gspinbutton(from = 0, to = 1, 
                                   length.out = 10, value = 0)
            g3_opt1c = gspinbutton(from = 0, to = 1, 
                                   length.out = 10, value = 0)
            
            
            g3Layout[2, 1, expand = TRUE, anchor = c(1, 0)] = g3_lab1
            g3Layout[2, 3, expand = TRUE, anchor = c(1, 0)] = g3_lab2
            
            g3Layout[1, 1:4, expand = TRUE] = g3_opt1a
            g3Layout[2, 2, expand = TRUE] = g3_opt1b
            g3Layout[2, 4, expand = TRUE] = g3_opt1c
            
            addHandlerChanged(g3_opt1a, handler = function(h,...) {
                var = svalue(h$obj)
                if (!(var %in% varNames)) { return() }
                
                ## delete current display
                replicate(2, delete(g3Layout, g3Layout$children[[4]]))
                ## reset indices
                for (i in length(g3Layout$children):4) {
                    g3Layout$children[[i + 2]] = g3Layout$children[[i]]
                }
                
                if (is.numeric(activeData[, var])) {
                    dat = activeData[is.finite(activeData[[var]]), var]
                    varMin = min(dat, na.rm = TRUE)
                    varMax = max(dat, na.rm = TRUE)
                    
                    g3_opt1b = gspinbutton(from = varMin, to = varMax,
                                           by = 0.01, value = varMin)
                    g3_opt1c = gspinbutton(from = varMin, to = varMax,
                                           by = 0.01, value = varMax)
                } else {
                    g3_opt1b = gspinbutton(
                        from = -180, to = 180, by = 0.01, value = -180)
                    g3_opt1c = gspinbutton(
                        from = -180, to = 180, by = 0.01, value = 180)
                }
                
                ## add
                g3Layout[2, 2, expand = TRUE] = g3_opt1b
                g3Layout[2, 4, expand = TRUE] = g3_opt1c
                
                ## update
                g3Layout$children[[5]] = g3_opt1b
                g3Layout$children[[4]] = g3_opt1c
            })
            
            
            ############
            ###  g4  ###
            ############
            g4NB = gnotebook(container = g4)
            
            ###########
            ## tab 1 ##
            ###########
            tab1 = glayout(spacing = 5, container = g4NB, label = "map")
            
            ## labels
            tab1_lab1 = glabel("map type")
            tab1_lab2 = glabel("colour")
            
            ## options
            tab1_opt1 = gcombobox(c("terrain", "satellite", "roadmap",
                                  "hybrid", "toner", "watercolour"),
                                selected = 1)
            tab1_opt2 = gcombobox(c("black-and-white", "colour"), selected = 1)
            
            tab1[2, 2, expand = TRUE, anchor = c(-1, 0)] = tab1_lab1
            tab1[3, 2, expand = TRUE, anchor = c(-1, 0)] = tab1_lab2
            
            tab1[2, 3, expand = TRUE] = tab1_opt1
            tab1[3, 3, expand = TRUE] = tab1_opt2
            
            tab1[1, 1:4] = gseparator(horizontal = TRUE)
            tab1[4, 1:4] = gseparator(horizontal = TRUE)
            tab1[1:4, 1] = gseparator(horizontal = FALSE)
            tab1[1:4, 4] = gseparator(horizontal = FALSE)
            
            ###########
            ## tab 2 ##
            ###########
            tab2 = glayout(spacing = 7, container = g4NB, label = "aesthetics")
            
            ## labels
            tab2_lab1 = glabel("mode")
            tab2_lab2 = glabel("zoom")
            
            ## options
            tab2_opt1a = gcheckbox("colour", checked = TRUE)
            tab2_opt1b = gcheckbox("size", checked = TRUE)
            tab2_opt1c = gcheckbox("alpha", checked = TRUE)
            
            zoom = computeZoom() - 2
            zoomLevels = c(paste(3, " (continent level)"), 4:9,
                           paste(10, "(city level)"), 11:22,
                           paste(21, "(building level)"))
            tab2_opt2 = gcombobox(zoomLevels, selected = zoom)
            
            tab2[2, 2, expand = TRUE, anchor = c(-1, 0)] = tab2_lab1
            tab2[3, 2, expand = TRUE, anchor = c(-1, 0)] = tab2_lab2
            
            tab2[2, 3, expand = TRUE] = tab2_opt1a
            tab2[2, 4, expand = TRUE] = tab2_opt1b
            tab2[2, 5, expand = TRUE] = tab2_opt1c
            tab2[3, 3:5, expand = TRUE] = tab2_opt2
            
            ## box
            tab2[1, 1:6] = gseparator(horizontal = TRUE)
            tab2[4, 1:6] = gseparator(horizontal = TRUE)
            tab2[1:4, 1] = gseparator(horizontal = FALSE)
            tab2[1:4, 6] = gseparator(horizontal = FALSE)
            
            addHandlerChanged(g1_opt1b, handler = function(h,...) {
                zoom = computeZoom() - 2
                svalue(tab2_opt2, index = TRUE) = zoom
            })
            addHandlerChanged(g1_opt1c, handler = function(h,...) {
                zoom = computeZoom() - 2
                svalue(tab2_opt2, index = TRUE) = zoom
            })
            addHandlerChanged(g1_opt2b, handler = function(h,...) {
                zoom = computeZoom() - 2
                svalue(tab2_opt2, index = TRUE) = zoom
            })
            addHandlerChanged(g1_opt2c, handler = function(h,...) {
                zoom = computeZoom() - 2
                svalue(tab2_opt2, index = TRUE) = zoom
            })
            
            ###########
            ## tab 3 ##
            ###########
            tab3 = gvbox(container = g4NB, label = "colour", expand = TRUE)
            tab3A = glayout(spacing = 11, container = tab3)
            tab3B = glayout(spacing = 11, container = tab3)
            visible(tab3B) = FALSE
            
            ## labels
            tab3_lab1 = glabel("gradient colour")
            tab3_lab2 = glabel("linear colour")
            
            ## options
            tab3_opt1a = gcombobox(c("low", "white", "green", 
                                     "yellow", "orange"), 
                                   selected = 1, editable = TRUE)
            tab3_opt1b = gcombobox(c("high", "red", "black", "navy", "blue"), 
                                   selected = 1, editable = TRUE)
            tab3_opt2 = gcombobox("default", selected = 1, editable = TRUE)
            
            size(tab3_opt1a) = c(80, 21)
            size(tab3_opt1b) = c(80, 21)
            size(tab3_opt2) = c(200, 21)
            
            addHandlerChanged(tab2_opt1a, handler = function(h,...) {
                if (svalue(h$obj)) {
                    visible(tab3A) = TRUE
                    visible(tab3B) = FALSE
                } else {
                    visible(tab3A) = FALSE
                    visible(tab3B) = TRUE
                }
            })
            
            ## tab3A
            tab3A[2, 2:3, expand = TRUE] = tab3_lab1
            tab3A[3, 2, expand = TRUE] = tab3_opt1a
            tab3A[3, 3, expand = TRUE] = tab3_opt1b
            
            tab3A[1, 1:4] = gseparator(horizontal = TRUE)
            tab3A[4, 1:4] = gseparator(horizontal = TRUE)
            tab3A[1:4, 1] = gseparator(horizontal = FALSE)
            tab3A[1:4, 4] = gseparator(horizontal = FALSE)
            
            ## tab3B
            tab3B[2, 2, expand = TRUE] = tab3_lab2
            tab3B[3, 2, expand = TRUE] = tab3_opt2
            
            tab3B[1, 1:3] = gseparator(horizontal = TRUE)
            tab3B[4, 1:3] = gseparator(horizontal = TRUE)
            tab3B[1:4, 1] = gseparator(horizontal = TRUE)
            tab3B[1:4, 3] = gseparator(horizontal = TRUE)
            
            ###########
            ## tab 4 ##
            ###########
            tab4 = gvbox(container = g4NB, label = "size", expand = TRUE)
            tab4A = glayout(spacing = 11, container = tab4)
            tab4B = glayout(spacing = 11, container = tab4)
            visible(tab4B) = FALSE
            
            ## labels
            tab4_lab1 = glabel("gradient scale")
            tab4_lab2 = glabel("linear scale")
            
            ## options
            tab4_opt1a = gcombobox(c("low", seq(0.1, 2, by = 0.1)),
                                   selected = 1, editable = TRUE)
            tab4_opt1b = gcombobox(c("high", seq(0.1, 2, by = 0.1)),
                                   selected = 1, editable = TRUE)
            tab4_opt2 = gcombobox(c("default", seq(0.1, 2, by = 0.1)),
                                  selected = 1, editable = TRUE)
            
            size(tab4_opt1a) = c(80, 21)
            size(tab4_opt1b) = c(80, 21)
            size(tab4_opt2) = c(200, 21)
            
            addHandlerChanged(tab2_opt1b, handler = function(h,...) {
                if (svalue(h$obj)) {
                    visible(tab4A) = TRUE
                    visible(tab4B) = FALSE
                } else {
                    visible(tab4A) = FALSE
                    visible(tab4B) = TRUE
                }
            })
            
            ## tab4A
            tab4A[2, 2:3, expand = TRUE] = tab4_lab1
            tab4A[3, 2, expand = TRUE] = tab4_opt1a
            tab4A[3, 3, expand = TRUE] = tab4_opt1b
            
            tab4A[1, 1:4] = gseparator(horizontal = TRUE)
            tab4A[4, 1:4] = gseparator(horizontal = TRUE)
            tab4A[1:4, 1] = gseparator(horizontal = FALSE)
            tab4A[1:4, 4] = gseparator(horizontal = FALSE)
            
            ## tab4B
            tab4B[2, 2, expand = TRUE] = tab4_lab2
            tab4B[3, 2, expand = TRUE] = tab4_opt2
            
            tab4B[1, 1:3] = gseparator(horizontal = TRUE)
            tab4B[4, 1:3] = gseparator(horizontal = TRUE)
            tab4B[1:4, 1] = gseparator(horizontal = TRUE)
            tab4B[1:4, 3] = gseparator(horizontal = TRUE)
            
            
            ###########
            ## tab 5 ##
            ###########
            tab5 = glayout(spacing = 7, container = g4NB, label = "contour*")
            
            ## labels
            tab5_lab1 = glabel("mode")
            tab5_lab2 = glabel("type")
            
            ## options
            tab5_opt1 = gcheckbox("fill", checked = FALSE)
            tab5_opt2 = gcombobox(c("filled", "line"), selected = 1)
            
            tab5[2, 2, expand = TRUE, anchor = c(-1, 0)] = tab5_lab1
            tab5[3, 2, expand = TRUE, anchor = c(-1, 0)] = tab5_lab2
            
            tab5[2, 3, expand = TRUE] = tab5_opt1
            tab5[3, 3, expand = TRUE] = tab5_opt2
            
            ## box
            tab5[1, 1:4] = gseparator(horizontal = TRUE)
            tab5[4, 1:4] = gseparator(horizontal = TRUE)
            tab5[1:4, 1] = gseparator(horizontal = FALSE)
            tab5[1:4, 4] = gseparator(horizontal = FALSE)
            
            
            ## set the first tab as default
            svalue(g4NB) = 1
            ## collapse the expandgroup as default
            visible(g4) = FALSE
            
            
            #############
            ###  g5   ###
            #############
            addSpring(mainGrp)
            g5 = ggroup(container = mainGrp)
            pButton = gbutton("points", handler = function(h,...) {
                arg = .self$argGenerator(buttonPressed = "points")
                .self$plotButtonHandler(arg)
            }, container = g5, expand = TRUE)
            
            cButton = gbutton("contour", handler = function(h,...) {
                arg = .self$argGenerator(buttonPressed = "contour")
                .self$plotButtonHandler(arg)
            }, container = g5, expand = TRUE)
            
            
            cancelButton = gbutton("cancel", handler = function(h,...) {
                ## delete the module window
                sapply(GUI$moduleWindow$children, 
                       function(x) delete(GUI$moduleWindow, x))
                ## display the default view (data, variable, etc.)
                visible(GUI$gp1) <<- TRUE
                
                ## delete preModBtn
                delete(GUI$gp1$children[[1]],
                       GUI$gp1$children[[1]]$children[[1]])
            }, container = mainGrp)
            
{
#             ############################
#             ###  BOTTOM MAIN WINDOW  ###
#             ############################
#             botWin = ggroup(horizontal = FALSE, spacing = 10, 
#                             container = mainGrp, expand = TRUE)
#             
#             botLayout = glayout(spacing = 10, container = botWin)
#             
#             botLab1 = glabel("Longitude")
#             botLab1a = glabel("from")
#             botLab1b = glabel("to")
#             botLab2 = glabel("Latitude")
#             botLab2a = glabel("from")
#             botLab2b = glabel("to")
#             botLab3 = glabel("Colour by")
#             botLab4 = glabel("from")
#             botLab5 = glabel("to")
#             botLab6 = glabel("Type of plot")
#             
#             tooltip(botLab1) = "select variable of longitude"
#             tooltip(botLab2) = "select variable of latitude"
#             tooltip(botLab3) = "select variable of interest"
#             tooltip(botLab4) = "choose lower boundary (minimum by default)"
#             tooltip(botLab5) = "choose upper boundary (maximum by default)"
#             tooltip(botLab6) = "is the plot drawn as points or contours?"
#             
#             font(botLab1) = labFont
#             font(botLab1a) = labFont
#             font(botLab1b) = labFont
#             font(botLab2) = labFont
#             font(botLab2a) = labFont
#             font(botLab2b) = labFont
#             font(botLab3) = labFont
#             font(botLab4) = labFont
#             font(botLab5) = labFont
#             font(botLab6) = labFont
#             
#             if (hasLon(activeData) & hasLat(activeData)) {
#                 
#                 lon = which(varNames == lonVar) + 1
#                 lat = which(varNames == latVar) + 1
#                 
#                 datLon = activeData[is.finite(activeData[, lonVar]), lonVar]
#                 datLat = activeData[is.finite(activeData[, latVar]), latVar]
#                 
#                 lonFrom = min(datLon, na.rm = TRUE)
#                 
#                 lonMax = max(datLon, na.rm = TRUE)
#                 lonTo = ifelse(lonMax > 180, 180, lonMax)
#                 
#                 latFrom = min(datLat, na.rm = TRUE)
#                 latTo = max(datLat, na.rm = TRUE)
#             } else {
#                 lon = 1
#                 lat = 1
#                 lonFrom = latFrom = 0
#                 lonTo = latTo = 1
#             }
#             
#             longitude = gcombobox(
#                 c("", varNames), selected = lon, container = botLayout
#             )
#             lonFromOpt = gspinbutton(
#                 from = lonFrom, to = lonTo, by = 0.01, value = lonFrom
#             )
#             lonToOpt = gspinbutton(
#                 from = lonFrom, to = lonTo, by = 0.01, value = lonTo
#             )
#             
#             latitude = gcombobox(
#                 c("", varNames), selected = lat, container = botLayout
#             )
#             latFromOpt = gspinbutton(
#                 from = latFrom, to = latTo, by = 0.01, value = latFrom
#             )
#             latToOpt = gspinbutton(
#                 from = latFrom, to = latTo, by = 0.01, value = latTo
#             )
#             
#             variable = gcombobox(c("", varNames), container = botLayout)
#             from = gspinbutton(from = 0, to = 1, length.out = 10, 
#                                value = 0, container = botLayout)
#             to = gspinbutton(from = 0, to = 1, length.out = 10,
#                              value = 0, container = botLayout)
#             type = gcheckboxgroup(
#                 c("point","contour"),
#                 checked = FALSE, horizontal = TRUE, 
#                 handler = function(h,...)
#                     changeOpts(svalue(h$obj, index = TRUE)),
#                 container = botLayout
#             )
#             
#             tooltip(longitude) = "select variable of longitude"
#             tooltip(latitude) = "select variable of latitude"
#             tooltip(variable) = "select variable of interest"
#             tooltip(from) = "choose lower boundary (minimum by default)"
#             tooltip(to) = "choose upper boundary (maximum by default)"
#             
#             size(longitude) = c(170, 25)
#             size(latitude) = c(170, 25)
#             size(variable) = c(170, 25)
#             # size(from) = size(lonFromOpt) = size(latFromOpt) = c(70, 20)
#             # size(to) = size(lonToOpt) = size(latToOpt) = c(70, 20)
#             
#             addHandlerChanged(
#                 longitude,
#                 handler = function(h,...) {
#                     var = svalue(h$obj)
#                     if (!(var %in% varNames))
#                         return()
#                     
#                     ## delete current
#                     replicate(2, delete(botLayout, botLayout$children[[12]]))
#                     ## reset indices
#                     for (i in length(botLayout$children):12) {
#                         botLayout$children[[i + 2]] = botLayout$children[[i]]
#                     }
#                     
#                     if (is.numeric(activeData[, var])) {
#                         lonMin = min(
#                             activeData[is.finite(activeData[, var]), var], 
#                             na.rm = TRUE
#                         )
#                         lonMax = max(
#                             activeData[is.finite(activeData[, var]), var], 
#                             na.rm = TRUE
#                         )
#                         
#                         lonFromOpt1 = gspinbutton(
#                             from = lonMin, to = lonMax, by = 0.01, 
#                             value = lonMin
#                         )
#                         lonToOpt1 = gspinbutton(
#                             from = lonMin, to = lonMax, by = 0.01, 
#                             value = lonMax
#                         )
#                     } else {
#                         ## TO-DO for factors
#                         lonFromOpt1 = gspinbutton(
#                             from = 0, to = 1, by = 0.01, value = 0
#                         )
#                         lonToOpt1 = gspinbutton(
#                             from = 0, to = 1, by = 0.01, value = 1
#                         )
#                     }
#                     
#                     ## Add
#                     botLayout[2, 4:5, expand = TRUE] = lonFromOpt1
#                     botLayout[3, 4:5, expand = TRUE] = lonToOpt1
#                     
#                     ## Update
#                     botLayout$children[[13]] = lonFromOpt1
#                     botLayout$children[[12]] = lonToOpt1
#                 }
#             )
#             
#             addHandlerChanged(
#                 latitude,
#                 handler = function(h,...) {
#                     var = svalue(h$obj)
#                     if (!(var %in% varNames))
#                         return()
#                     
#                     ## delete current
#                     replicate(2, delete(botLayout, botLayout$children[[15]]))
#                     ## reset indices
#                     for (i in length(botLayout$children):15) {
#                         botLayout$children[[i + 2]] = botLayout$children[[i]]
#                     }
#                     
#                     if (is.numeric(activeData[, var])) {
#                         latMin = min(
#                             activeData[is.finite(activeData[, var]), var], 
#                             na.rm = TRUE
#                         )
#                         latMax = max(
#                             activeData[is.finite(activeData[, var]), var], 
#                             na.rm = TRUE
#                         )
#                         
#                         latFromOpt = gspinbutton(
#                             from = latMin, to = latMax, by = 0.01, 
#                             value = latMin
#                         )
#                         latToOpt = gspinbutton(
#                             from = latMin, to = latMax, by = 0.01, 
#                             value = latMax
#                         )
#                     } else {
#                         ## TO-DO for factors
#                         latFromtOpt = gspinbutton(
#                             from = 0, to = 1, by = 0.01,
#                             value = 0
#                         )
#                         latToOpt = gspinbutton(
#                             from = 0, to = 1, by = 0.01,
#                             value = 0
#                         )
#                     }
#                     
#                     botLayout[5, 4:5, expand = TRUE] = latFromOpt
#                     botLayout[6, 4:5, expand = TRUE] = latToOpt
#                     
#                     ## update the indices
#                     botLayout$children[[16]] = latToOpt
#                     botLayout$children[[15]] = latFromOpt
#                 }
#             )
#             
#             addHandlerChanged(
#                 variable,
#                 handler = function(h,...) {
#                     var = svalue(h$obj)
#                     if (!(var %in% varNames))
#                         return()
#                     
#                     ## Delete the spin buttons
#                     replicate(2, delete(botLayout, botLayout$children[[18]]))
#                     ## reset indices
#                     for (i in length(botLayout$children):18) {
#                         botLayout$children[[i + 2]] = botLayout$children[[i]]
#                     }
#                     
#                     if (is.numeric(activeData[, var])) {
#                         ## Find the min & max of the selected variable
#                         varMin = min(
#                             activeData[is.finite(activeData[, var]), var],
#                             na.rm = TRUE
#                         )
#                         varMax = max(
#                             activeData[is.finite(activeData[, var]), var],
#                             na.rm = TRUE
#                         )
#                         
#                         ## Create the spin buttons
#                         from = gspinbutton(
#                             from = varMin, to = varMax, length.out = 100, 
#                             value = varMin, container = botLayout)
#                         
#                         to = gspinbutton(
#                             from = varMin, to = varMax, length.out = 100, 
#                             value = varMax, container = botLayout)
#                     } else {
#                         ## TO-DO for factors
#                         from = gspinbutton(
#                             from = 0, to = 1, by = 0.01,
#                             value = 0
#                         )
#                         to = gspinbutton(
#                             from = 0, to = 1, by = 0.01,
#                             value = 0
#                         )
#                     }
#                     
#                     # size(from) = c(70, 20)
#                     # size(to) = c(70, 20)
#                     
#                     ## Add them
#                     botLayout[8, 4:5, expand = TRUE] = from
#                     botLayout[9, 4:5, expand = TRUE] = to
#                     
#                     ## Update the indices
#                     botLayout$children[[19]] = to
#                     botLayout$children[[18]] = from
#                 }
#             )
#             
#             usingMethods(opt, opt1, opt2, opt3)
#             
#             botLayout[1, 1:2, expand = TRUE, anchor = c(-1, 0)] = botLab1
#             botLayout[2, 3, expand = TRUE, anchor = c(1, 0)] = botLab1a
#             botLayout[3, 3, expand = TRUE, anchor = c(1, 0)] = botLab1b
#             botLayout[4, 1:2, expand = TRUE, anchor = c(-1, 0)] = botLab2
#             botLayout[5, 3, expand = TRUE, anchor = c(1, 0)] = botLab2a
#             botLayout[6, 3, expand = TRUE, anchor = c(1, 0)] = botLab2b
#             botLayout[7, 1:2, expand = TRUE, anchor = c(-1, 0)] = botLab3
#             botLayout[8, 3, expand = TRUE, anchor = c(1, 0)] = botLab4
#             botLayout[9, 3, expand = TRUE, anchor = c(1, 0)] = botLab5
#             botLayout[10, 1:2, expand = TRUE, anchor = c(-1, 0)] = botLab6
#             
#             botLayout[1, 3:5, expand = TRUE] = longitude
#             botLayout[2, 4:5, expand = TRUE] = lonFromOpt
#             botLayout[3, 4:5, expand = TRUE] = lonToOpt
#             
#             botLayout[4, 3:5, expand = TRUE] = latitude
#             botLayout[5, 4:5, expand = TRUE] = latFromOpt
#             botLayout[6, 4:5, exapnd = TRUE] = latToOpt
#             
#             botLayout[7, 3:5, expand = TRUE] = variable
#             botLayout[8, 4:5, expand = TRUE] = from
#             botLayout[9, 4:5, expand = TRUE] = to
#             botLayout[10, 3:5, expand = TRUE] = type
#            optGrp <<- ggroup(expand = TRUE)
            # size(optGrp) <<- c(300, 150)
#            add(mainGrp, optGrp, expand = TRUE)
###############################################################################
#             ## plot button is made and assigned after adding optGrp to mainGrp
#             ## so that the options in optGrp can be accessed.
#             plotButtonAction = gaction(
#                 "Plot",
#                 tooltip = "Click to plot with current settings",
#                 icon = "gtk-apply",
#                 handler = function(h, ...) {
#                     ## checks
#                     vars = c(svalue(longitude), 
#                              svalue(latitude), 
#                              svalue(variable))
#                     type = svalue(type)
#                     if (any(vars == "") | length(type == 0)) {
#                         return(gmessage("All settings must be specified",
#                                         title = "Error", icon = "error"))
#                     }
#                     
#                     ## create string paths to access options
#                     path = "optGrp$children[[1]]$children[[1]]"
#                     pathToTab1 = paste0(path, "$children[[1]]")
#                     pathToTab2 = paste0(path, "$children[[2]]")
#                     pathToTab3 = paste0(path, "$children[[3]]")
#                     
#                     ## tab 1
#                     pathToTab1Opts = paste0(
#                         pathToTab1, "$children[[", 2:4, "]]"
#                     )
#                     whichMode = sapply(
#                         pathToTab1Opts, 
#                         function(x) svalue(evalText(x))
#                     )
#                     mode = paste0(
#                         "c(", 
#                         paste(c("\"colour\"",
#                                 "\"size\"",
#                                 "\"alpha\"")[whichMode], collapse = ", "),
#                         ")"
#                     )
#                     
#                     ## tab 2
#                     pathToTab2Opts = paste0(
#                         pathToTab2, "$children[[", 1:4, "]]"
#                     )
#                     whichColour = svalue(
#                         evalText(pathToTab2Opts[1]),
#                         index = TRUE
#                     )
#                     if (whichColour == 1) {
#                         col = NULL
#                         low = svalue(evalText(pathToTab2Opts[2]))
#                         if (low == "low" | low == "") { low = NULL }
#                         high = svalue(evalText(pathToTab2Opts[3]))
#                         if (high == "high" | high == "") { high = NULL }
#                     } else if (whichColour == 2) {
#                         col = svalue(evalText(pathToTab2Opts[4]))
#                         if (col == "default" | col == "") { col = NULL }
#                         low = NULL
#                         high = NULL
#                     }
#                     
#                     ## tab 3
#                     pathToTab3Opts = paste0(
#                         pathToTab3, "$children[[", 2, "]]"
#                     )
#                     size = as.numeric(
#                         svalue(evalText(pathToTab3Opts))
#                     )
#                     if (size == 4) { size = NULL }
#                     
#                     from = svalue(
#                         mainGrp$children[[3]]$children[[1]]$children[[10]]
#                     )
#                     to = svalue(
#                         mainGrp$children[[3]]$children[[1]]$children[[11]]
#                     )
#                     
#                     loc = c(svalue(latToOpt), svalue(lonToOpt), 
#                             svalue(latFromOpt), svalue(lonFromOpt))
#                     zoom = svalue(expGrpOpt2)
#                     maptype = svalue(expGrpOpt1)
#                     colour = ifelse(svalue(expGrpOpt3, index = TRUE) == 1,
#                                     gsub("^.+$", "bw", svalue(expGrpOpt3)),
#                                     gsub("u", "", svalue(expGrpOpt3)))
#                     
#                     .self$plotButtonHandler(
#                         vars, from, to, loc, zoom, maptype, colour,
#                         type, mode, low, high, size, col
#                     )
#                     
#                 }, container = botLayout
#             )
#             plotButton = gbutton(action = plotButtonAction)
#             add(mainGrp, plotButton)
#         }
}
},
        
        #######################################################################
        ## Function to check for a geographical data set.
        geoData = function() {
            if (!isGeoData(GUI$getActiveData())) {
                checkToProceed = gconfirm(
                    paste("Cannot find longitudes and latitudes in data set.",
                          "Proceed?", sep = "\n"),
                    title = "Warning", icon = "warning"
                )
            } else {
                return(TRUE)
            }
        },
        
        ## Function to handle proceed or stop
        ## if the data set is not geographical.
        checkGeoData = function() {
            if (!geoData()) {
                visible(GUI$gp1) <<- TRUE
                if (length(GUI$gp1$children[[1]]$children) == 3) {
                    delete(GUI$gp1$children[[1]],
                           GUI$gp1$children[[1]]$children[[1]])
                }
                return()
            }
        },
        
        ## Wrapper.
        evalText = function(txt) {
            eval(parse(text = txt))
        },
        
        ## Function to compute zoom based on bounding boxes.
        ## Reference: https://github.com/dkahle/ggmap/blob/master/R/get_map.R
        computeZoom = function() {
            wstPath = "mainGrp$children[[1]]$children[[1]]$children[[7]]"
            estPath = "mainGrp$children[[1]]$children[[1]]$children[[8]]"
            nthPath = "mainGrp$children[[1]]$children[[1]]$children[[9]]"
            sthPath = "mainGrp$children[[1]]$children[[1]]$children[[10]]"
            
            wst = svalue(evalText(wstPath))
            est = svalue(evalText(estPath))
            nth = svalue(evalText(nthPath))
            sth = svalue(evalText(sthPath))
            
            lonDiff = abs(nth - sth)
            latDiff = abs(wst - est)
            
            ## using the formula from the source code
            zoomLon = ceiling(log2(360*2 / lonDiff))
            zoomLat = ceiling(log2(180*2 / latDiff))
            
            zoom = min(zoomLon, zoomLat)
            return(zoom)
        },
        
        ## Function to generate arguments based on the button
        ## pressed to pass onto appropriate button handlers.
        argGenerator = function(buttonPressed) {
            ## set up paths for access to the options
            g1Path = "mainGrp$children[[1]]$children[[1]]"
            g2Path = "mainGrp$children[[2]]$children[[1]]"
            g3Path = "mainGrp$children[[3]]$children[[1]]"
            g4Path = "mainGrp$children[[4]]$children[[1]]"
            g5Path = "mainGrp$children[[5]]"
            tab1Path = paste0(g4Path, "$children[[1]]")
            tab2Path = paste0(g4Path, "$children[[2]]")
            tab3Path = paste0(g4Path, "$children[[3]]")
            tab4Path = paste0(g4Path, "$children[[4]]")
            tab5Path = paste0(g4Path, "$children[[5]]")
            
            ## arg: data
            data = activeData
            
            ## arg: lon
            lon = svalue(evalText(paste0(g1Path, "$children[[5]]")))
            
            ## arg: lat
            lat = svalue(evalText(paste0(g1Path, "$children[[6]]")))
            
            ## arg: colour_by & colour_by_cond
            cIndex = svalue(evalText(paste0(g2Path, "$children[[3]]")),
                            index = TRUE)
            if (cIndex == 1) {
                colour_by = NULL
                colour_by_cond = NULL
            } else {
                colour_by = svalue(evalText(paste0(g2Path, "$children[[3]]")))
                cLo = svalue(evalText(paste0(g2Path, "$children[[5]]")))
                cHi = svalue(evalText(paste0(g2Path, "$children[[4]]")))
                colour_by_cond = 
                    paste(paste(cLo, "<=", paste0("data$", colour_by)),
                          paste(paste0("data$", colour_by), "<=", cHi),
                          sep = " & ")
            }
            
            ## arg: size_by & size_by_cond
            sIndex = svalue(evalText(paste0(g3Path, "$children[[3]]")),
                            index = TRUE)
            if (sIndex == 1) {
                size_by = NULL
                size_by_cond = NULL
            } else {
                size_by = svalue(evalText(paste0(g3Path, "$children[[3]]")))
                sLo = svalue(evalText(paste0(g3Path, "$children[[5]]")))
                sHi = svalue(evalText(paste0(g3Path, "$children[[4]]")))
                size_by_cond = 
                    paste(paste(sLo, "<=", paste0("data$", size_by)),
                          paste(paste0("data$", size_by), "<=", sHi),
                          sep = " & ")
            }
            
            ## arg: location
            wst = svalue(evalText(paste0(g1Path, "$children[[7]]")))
            est = svalue(evalText(paste0(g1Path, "$children[[8]]")))
            sth = svalue(evalText(paste0(g1Path, "$children[[9]]")))
            nth = svalue(evalText(paste0(g1Path, "$children[[10]]")))
            location = c(nth, est, sth, wst)
            
            ## arg: zoom
            zoomAccess = paste0(tab2Path, "$children[[6]]")
            zoom = svalue(evalText(zoomAccess))
            zoom = as.numeric(gsub("\\s[(].+$", "", zoom))
            
            ## arg: maptype
            maptypeAccess = paste0(tab1Path, "$children[[3]]")
            maptype = svalue(evalText(maptypeAccess))
            
            ## arg: colour
            colAccess = paste0(tab1Path, "$children[[4]]")
            col = svalue(evalText(colAccess), index = TRUE)
            colour = ifelse(col == 1, "bw", "color")
            
            ## arg: type & mode
            modeAccess = paste0(tab2Path, "$children[[", 3:5, "]]")
            whichMode = sapply(modeAccess, function(x) svalue(evalText(x)))
            if (buttonPressed == "points") {
                ## type
                type = "point"
                ## mode
                selectedMode = paste(
                    c("\"colour\"", "\"size\"","\"alpha\"")[whichMode],
                    collapse = ", ")
                mode = paste0("c(", selectedMode, ")")
            } else if (buttonPressed == "contour") {
                ## type
                type = "contour"
                ## mode
                fill = svalue(evalText(paste0(tab5Path, "$children[[3]]")))
                if (length(whichMode) == 3) { whichMode = whichMode[-2] }
                whichMode = c(whichMode, fill)
                selectedMode = paste(
                    c("\"colour\"", "\"alpha\"", "\"fill\"")[whichMode],
                    collapse = ", ")
                mode = paste0("c(", selectedMode, ")")
                ## geom
                geomType = svalue(evalText(paste0(tab5Path, "$children[[4]]")),
                                  index = TRUE)
                geom = ifelse(geomType == 1, "polygon", "density2d")
            }
            
            ## arg: low, high & col
            c1 = paste0(tab3Path, "$children[[1]]", "$children[[", 2:3, "]]")
            c2 = paste0(tab3Path, "$children[[2]]$children[[2]]")
            
            if (svalue(evalText(modeAccess[2]))) {
                low = svalue(evalText(c1[1]))
                high = svalue(evalText(c1[2]))
                col = NULL
                if (low == "low") { low = "#5EB7F8" }
                if (high == "high") { high = "#1A334B" }
            } else {
                low = NULL
                high = NULL
                col = svalue(evalText(c2))
                if (col == "default") { col = NULL }
            }
            
            ## arg: size
            
            
            ## create arg as a list
            arg = list(data = data, lon = lon, lat = lat, 
                       colour_by = colour_by, colour_by_cond = colour_by_cond, 
                       size_by = size_by, size_by_cond = size_by_cond,
                       location = location, zoom = zoom,
                       maptype = maptype, colour = colour, type = type, 
                       mode = mode, low = low, high = high, col = col)
            if (buttonPressed == "contour") { arg$geom = geom }
            
            return(arg)
        },
        
        plotButtonHandler = function(arg) {
            ##########################################
            source("../iNZightMaps/R/general.R")
            source("../iNZightMaps/R/drawMap.R")
            source("../iNZightMaps/R/draw.R")
            source("../iNZightMaps/R/generateLine.R")
            source("../iNZightMaps/R/getBB.R")
            source("../iNZightMaps/R/isGeoData.R")
            source("../iNZightMaps/R/varSubset.R")
            source("../iNZightMaps/R/processFactor.R")
            library(ggmap)
            ##########################################
            
            map = do.call(draw, arg)
            
            GUI$plotWidget$addPlot()
            plot(map)
        },
        
        plotButtonHandler.a = function(vars, from, to,
                                     location, zoom, maptype, colour,
                                     type, mode, low, high, size, col) {
            ##########################################
            source("../iNZightMaps/R/general.R")
            source("../iNZightMaps/R/drawMap.R")
            source("../iNZightMaps/R/draw.R")
            source("../iNZightMaps/R/generateLine.R")
            source("../iNZightMaps/R/getBB.R")
            source("../iNZightMaps/R/isGeoData.R")
            source("../iNZightMaps/R/varSubset.R")
            source("../iNZightMaps/R/processFactor.R")
            library(ggmap)
            ##########################################
            
            data = activeData
            
            lon = vars[1]
            lat = vars[2]
            var = vars[3]
            
            cond = paste(
                paste(from, "<=", paste0("data$", var)),
                paste(paste0("data$", var), "<=", to),
                sep = " & "
            )
            
            maptype = tolower(maptype)
            maptype = gsub("[*]$", "", maptype)
            
            arg = list(data = data, lon = lon, lat = lat, 
                       var = var, var.cond = cond, 
                       location = location, zoom = zoom,
                       maptype = maptype, colour = colour,
                       type = type,  mode = mode, low = low, high = high, 
                       size = size, col = col)
            map = do.call(draw, arg)
            
            plot(map)
        },
        #######################################################################
        
        changeOpts = function(index) {
            ## delete current displayed options
            sapply(optGrp$children, function(x) delete(optGrp, x))
            
            ## if both options checked index is c(1, 2). So make it 3
            if (length(index) == 2)
                index = 3
            
            do.call(paste0("opt", index), args = list())
        },
        
        ## opt (when unchecked. display nothing)
        opt = function() {
            #sapply(optGrp$children, function(x) delete(optGrp, x))
        },
        
        ## "point"
        opt1 = function() {
            optsExpandGrp = gexpandgroup("Advanced settings",
                                         container = optGrp)
            optsExpandGrp$set_borderwidth(5)
            visible(optsExpandGrp) = FALSE
            
            optsNB = gnotebook(container = optsExpandGrp)
            size(optsNB) = c(300, 110)
            
            ##############
            ###  TAB 1 ###
            ##############
            tab1 = glayout(container = optsNB, label = "aesthetics")
            
            tab1.lab = glabel("mode", container = tab1)
            tooltip(tab1.lab) = paste("select how plotting symbols", 
                                      "are to be scaled by")
            
            tab1.opt1 = gcheckbox("colour", checked = TRUE)
            tab1.opt2 = gcheckbox("size", checked = TRUE)
            tab1.opt3 = gcheckbox("alpha", checked = TRUE)
            
            tooltip(tab1.opt1) = "symbols are coloured"
            tooltip(tab1.opt2) = "symbols are scaled by size of variable"
            tooltip(tab1.opt3) = "symbols are scaled with transparency"
            
            tab1[2, 2:3, expand = TRUE, anchor = c(-1, 0)] = tab1.lab
            tab1[2, 4, expand = TRUE] = tab1.opt1
            tab1[2, 5, expand = TRUE] = tab1.opt2
            tab1[2, 6, expand = TRUE] = tab1.opt3
            
            ## box
            tab1[1, 1:7] = gseparator(horizontal = TRUE)
            tab1[3, 1:7] = gseparator(horizontal = TRUE)
            tab1[1:3, 1] = gseparator(horizontal = FALSE)
            tab1[1:3, 7] = gseparator(horizontal = FALSE)
            
            ###############
            ###  TAB 2  ###
            ###############
            tab2 = glayout(container = optsNB, label = "colour")
            tab2.opt1 = gradio(c("gradient", "linear"), 
                               selected = 1, horizontal = FALSE)
            tab2.opt2a = gcombobox(
                c("low", "white", "green", "yellow", "orange"), 
                  selected = 1, editable = TRUE
            )
            tab2.opt2b = gcombobox(
                c("high", "red", "black", "navy", "blue"), 
                selected = 1, editable = TRUE
            )
            tab2.opt3 = gcombobox("default", selected = 1, editable = TRUE)
            enabled(tab2.opt3) = FALSE
            
            addHandlerChanged(
                tab2.opt1,
                handler = function(h,..) {
                    if (svalue(h$obj, index = TRUE) == 1 &
                            svalue(tab1.opt1) == TRUE) {
                        enabled(tab2.opt2a) = TRUE
                        enabled(tab2.opt2b) = TRUE
                        enabled(tab2.opt3) = FALSE
                    } else {
                        enabled(tab2.opt2a) = FALSE
                        enabled(tab2.opt2b) = FALSE
                        enabled(tab2.opt3) = TRUE
                    }
                }
            )
            
            addHandlerChanged(
                tab1.opt1,
                handler = function(h,...) {
                    if (svalue(h$obj) == FALSE) {
                        enabled(tab2.opt2a) = FALSE
                        enabled(tab2.opt2b) = FALSE
                        svalue(tab2.opt1, index = TRUE) = 2
                    }
                }
            )
            
            size(tab2.opt2a) = c(60, 23)
            size(tab2.opt2b) = c(60, 23)
            size(tab2.opt3) = c(150, 23)
            
            tab2[2:3, 2, expand = TRUE] = tab2.opt1
            tab2[2, 4, expand = TRUE] = tab2.opt2a
            tab2[2, 5, expand = TRUE] = tab2.opt2b
            tab2[3, 4:5, expand = TRUE] = tab2.opt3
            
            ## box
            tab2[1, 1:6] = gseparator(horizontal = TRUE)
            tab2[4, 1:6] = gseparator(horizontal = TRUE)
            tab2[1:4, 1] = gseparator(horizontal = FALSE)
            tab2[1:4, 6] = gseparator(horizontal = FALSE)
            
            ###############
            ###  TAB 3  ###
            ###############
            # tab3 = ggroup(container = optsNB, label = "size")
            
            ## scale
            tab3 = glayout(container = optsNB, label = "size")
            tab3.lab1 = glabel("scale")
            tab3.opt1 = gslider(from = 1, to = 10, by = 0.1, value = 4, 
                               container = tab3)
            
            tab3[2, 2] = tab3.lab1
            tab3[2, 3, expand = TRUE, fill = "both"] = tab3.opt1
            
#             ## scale range
#             tab3b = glayout(container = tab3)
#             visible(tab3b) = FALSE
#             
#             tab3.lab2 = glabel("scale range")
#             tab3.lab2a = glabel("low")
#             tab3.lab2b = glabel("high")
#             tab3.opt2a = gspinbutton(from = 1, to = 5, by = 0.1, value = 1)
#             tab3.opt2b = gspinbutton(from = 1, to = 5, by = 0.1, value = 1.5)
#             
#             tab3b[2:3, 2, visible = FALSE] = tab3.lab2
#             tab3b[2, 3, visible = FALSE] = tab3.lab2a
#             tab3b[2, 4, vislble = FALSE] = tab3.lab2b
#             tab3b[3, 3, visible = FALSE] = tab3.opt2a
#             tab3b[3, 4, visible = FALSE] = tab3.opt2b
#             
#             addHandlerChanged(
#                 tab1.opt2,
#                 handler = function(h,...) {
#                     if (svalue(h$obj) == TRUE) {
#                         visible(tab3a) = TRUE
#                         visible(tab3b) = FALSE
#                     } else {
#                         visible(tab3a) = FALSE
#                         visible(tab3b) = TRUE
#                     }
#                 }
#             )
            
            
            ## box
            tab3[1, 1:4] = gseparator(horizontal = TRUE)
            tab3[3, 1:4] = gseparator(horizontal = TRUE)
            tab3[1:3, 1] = gseparator(horizontal = FALSE)
            tab3[1:3, 4] = gseparator(horizontal = FALSE)
            
            ################
            ###  TAB 4   ###
            ################
            tab4 = glayout(container = optsNB, label = "map")
            tab4.lab1 = glabel("type")
            tab4.lab2 = glabel("zoom")
            tab4.lab3 = glabel("colour")
            
            tab4.opt1 = gcombobox(
                c("terrain", "satellite", "roadmap", 
                  "hybrid", "toner", "watercolour"),
                selected = 1, container = tab4
            )
            tab4.opt2 = gslider (
                from = 3, to = 21, by = 1, value = 10,
                container = tab4
            )
            tab4.opt3 = gcombobox(
                c("black-and-white", "colour"),
                selected = 1, container = tab4
            )
            
            tab4[2, 2, expand = TRUE, anchor = c(-1, 0)] = tab4.lab1
            tab4[3, 2, expand = TRUE, anchor = c(-1, 0)] = tab4.lab2
            tab4[4, 2, expand = TRUE, anchor = c(-1, 0)] = tab4.lab3
            tab4[2, 3, expand = TRUE] = tab4.opt1
            tab4[3, 3, expand = TRUE] = tab4.opt2
            tab4[4, 3, expand = TRUE] = tab4.opt3
            
            ## box
            tab4[1, 1:4] = gseparator(horizontal = TRUE)
            tab4[5, 1:4] = gseparator(horizontal = TRUE)
            tab4[1:5, 1] = gseparator(horizontal = FALSE)
            tab4[1:5, 4] = gseparator(horizontal = FALSE)
            
            svalue(optsNB) = 1
            
        },
        
        ## opt2 ("contour")
        opt2 = function() {
            tbl = glayout(container = optGrp)
            lab1 = glabel("Select mode")
            font(lab1) = list(weight = "bold")
            optGrp1 = gcheckboxgroup(c("colour", "alpha"),
                                     checked = FALSE, horizontal = FALSE,
                                     container = tbl, expand = FALSE)
            lab2 = glabel("Select geometry", container = tbl)
            font(lab2) = list(weight = "bold")
            optGrp2 = gradio(c("polygon", "contour"),
                             selected = 1, container = tbl, expand = FALSE)
            lab3 = glabel("Select colour")
            font(lab3) = list(weight = "bold")
            optGrp3 = ggroup(horizontal = FALSE, expand = FALSE, 
                             container = tbl)
            optGrp3a = gcombobox(c("Low", "Blue", "red", "green", "add more.."),
                                 selected = 1, container = optGrp3)
            optGrp3b = gcombobox(c("High", "blah"),
                                 selected = 1, container = optGrp3)
            optGrp3c = gcombobox(c("colour", "add more.."),
                                 selected = 1, container = optGrp3)
            lab4 = glabel("Specify size", container = tbl)
            font(lab4) = list(weight = "bold")
            optGrp4 = gslider(from = 0.1, to = 2.0, by = 0.1, value = 1.0,
                              container = tbl)
            lab5 = glabel("Factor options", container = tbl)
            font(lab5) = list(weight = "bold")
            optGrp5 = ggroup(horizontal = FALSE, expand = FALSE,
                             container = tbl)
            optGrp5a = gcheckbox("solid shape", checked = FALSE,
                                 container = optGrp5)
            optGrp5b = gcheckbox("use grid", checked = FALSE,
                                 container = optGrp5)
            
            tbl[1, 1:6, expand = TRUE] = gseparator(horizontal = TRUE, 
                                                  container = tbl)
            tbl[2:5, 2] = gseparator(horizontal = FALSE, container = tbl)
            tbl[2:5, 4] = gseparator(horizontal = FALSE, container = tbl)
            tbl[2:5, 6] = gseparator(horizontal = FALSE, container = tbl)
            
            tbl[2, 1] = lab1
            tbl[4, 1] = lab2
            tbl[2, 3] = lab3
            tbl[2, 5] = lab4
            tbl[2, 7] = lab5
            
            tbl[3, 1] = optGrp1
            tbl[5, 1] = optGrp2
            tbl[3, 3] = optGrp3
            tbl[3, 5] = optGrp4
            tbl[3, 7] = optGrp5
        },
        
        ## opt3 ("both")
        opt3 = function() {}
    )
)

# iNZMapModWin()

