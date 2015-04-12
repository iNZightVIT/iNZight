iNZightMapModWin <- setRefClass(
    "iNZightMapModWin",
    fields = list(
        GUI = "ANY",
        mainGrp = "ANY",
        activeData = "data.frame",
        lonVar = "character",
        latVar = "character",
        varNames = "character"
    ),
    methods = list(
        initialize = function(GUI) {
            initFields(GUI = GUI)
            
#             ## NOTE:
#             ##   could be more generic by implementing this in the main GUI class
#             ##   eg, GUI$setup("module")
#             ##
#             ## check if the module installed
#             if ("iNZightMaps" %in% rownames(installed.packages())) {
#                 lapply(c(iNZightMaps, ggmap), require)
#                 require(iNZightMaps)
#             } else {
#                 install = gconfirm("The module is not found. Would you like to download it?")
#                 if (install) {
#                     #install.packages("iNZightMaps", repo = "http://docker..?")
#                     devtools::install_github(repo = "iNZightVIT/iNZightMaps")
#                     require(iNZightMaps)
#                 } else {
#                     return
#                 }
#             }
            activeData <<- GUI$getActiveData()

            if (is.null(activeData)) {
                gmessage("Please import data", icon = "info")
                return()
            }
#             if (!isGeoData(activeData)) {
#                 proceed = gconfirm("The imported data set is not geographical. Click ")
#             }
            
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
            borderColour = "slategrey"
            
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
            g4 = ggroup(container = mainGrp, fill = "x")
            
            g5Cont = ggroup(container = mainGrp)
            visible(g5Cont) = FALSE
            g5 = gexpandgroup("Advanced settings", horizontal = FALSE,
                              container = g5Cont)
            
            addSpring(mainGrp)
            g6 = ggroup(container = mainGrp)
            
            label1 = getToolkitWidget(mainGrp)$
                getChildren()[[1]]$getChildren()[[2]]
            label2 = getToolkitWidget(mainGrp)$
                getChildren()[[2]]$getChildren()[[2]]
            label3 = getToolkitWidget(mainGrp)$
                getChildren()[[3]]$getChildren()[[2]]
            # label4 = getToolkitWidget(mainGrp)$
            #     getChildren()[[5]]$getChildren()[[2]]
            
            mainGrp$set_rgtk2_font(label1, frameFont)
            mainGrp$set_rgtk2_font(label2, frameFont)
            mainGrp$set_rgtk2_font(label3, frameFont)
            # mainGrp$set_rgtk2_font(label4, frameFont)
            
            g1$set_borderwidth(8)
            g2$set_borderwidth(8)
            g3$set_borderwidth(8)
            # g5$set_borderwidth(3)
            
            ############
            ###  g1  ###
            ############
            g1Layout = glayout(container = g1)#, spacing = 20)
            
            ###  labels  ###
            g1_lab1 = glabel("from")
            g1_lab2 = glabel("to")
            g1_lab3 = glabel("fom")
            g1_lab4 = glabel("to")
            
            #font(g1_lab1) = labFont
            #font(g1_lab2) = labFont
            #font(g1_lab3) = labFont
            #font(g1_lab4) = labFont
            
            ###  options  ###
            if (hasLon(activeData) & hasLat(activeData)) {
                lon = which(varNames == lonVar)
                lat = which(varNames == latVar)
                
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
            g1_opt1a = gcombobox(varNames, selected = lon)
            g1_opt1b = gspinbutton(from = lonFrom, to = lonTo,
                                     by = 0.01, value = lonFrom)
            g1_opt1c = gspinbutton(from = lonFrom, to = lonTo,
                                   by = 0.01, value = lonTo)
            
            ## option 2: latitude
            g1_opt2a = gcombobox(varNames, selected = lat)
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
                } else {
                    lonMin = -180
                    lonMax = 180
                }
                
                ## create spin buttons
                g1_opt1b = gspinbutton(
                    from = lonMin, to = lonMax, by = 0.1, value = lonMin)
                g1_opt1c = gspinbutton(
                    from = lonMin, to = lonMax, by = 0.1, value = lonMax)
                
                ## add
                g1Layout[2, 2, expand = TRUE] = g1_opt1b
                g1Layout[2, 4, expand = TRUE] = g1_opt1c
                
                ## update
                g1Layout$children[[8]] = g1_opt1b
                g1Layout$children[[7]] = g1_opt1c
            })
            addHandlerChanged(g1_opt2a, handler = function(h,...) {
                var = svalue(h$obj)
                
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
                } else {
                    latMin = -90
                    latMax = 90
                }
                
                ## create spin buttons
                g1_opt2b = gspinbutton(
                    from = latMin, to = latMax, by = 0.1, value = latMin)
                g1_opt2c = gspinbutton(
                    from = latMin, to = latMax, by = 0.1, value = latMax)
                
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
            #font(g2_lab1) = labFont
            #font(g2_lab2) = labFont
            
            ###  options  ###
            g2_opt1a = gcombobox(c("", varNames), selected = 1)
            g2_opt1b = gspinbutton(from = 0, to = 1, 
                                   length.out = 10, value = 0)
            g2_opt1c = gspinbutton(from = 0, to = 1, 
                                   length.out = 10, value = 0)
            g2_opt1d = gcheckbox("solid shapes", checked = FALSE)
            size(g2_opt1d) = c(100, 21)
            visible(g2_opt1d) = FALSE
            
            g2Layout[2, 1, expand = TRUE, anchor = c(1, 0)] = g2_lab1
            g2Layout[2, 3, expand = TRUE, anchor = c(1, 0)] = g2_lab2
            
            g2Layout[3, 4, expand = FALSE]  = g2_opt1d
            g2Layout[1, 1:4, expand = TRUE] = g2_opt1a
            g2Layout[2, 2, expand = TRUE]   = g2_opt1b
            g2Layout[2, 4, expand = TRUE]   = g2_opt1c
            
            ############
            ###  g3  ###
            ############
            g3Layout = glayout(container = g3)#, spacing = 20)
            
            ###  labels  ###
            g3_lab1 = glabel("from")
            g3_lab2 = glabel("to")
            
            #font(g3_lab1) = labFont
            #font(g3_lab2) = labFont
            
            ###  options  ###
            g3_opt1a = gcombobox(c(" ", varNames), selected = 1)
            g3_opt1b = gspinbutton(from = 0, to = 1, 
                                   length.out = 10, value = 0)
            g3_opt1c = gspinbutton(from = 0, to = 1, 
                                   length.out = 10, value = 0)
            
            
            g3Layout[2, 1, expand = TRUE, anchor = c(1, 0)] = g3_lab1
            g3Layout[2, 3, expand = TRUE, anchor = c(1, 0)] = g3_lab2
            
            g3Layout[1, 1:4, expand = TRUE] = g3_opt1a
            g3Layout[2, 2, expand = TRUE] = g3_opt1b
            g3Layout[2, 4, expand = TRUE] = g3_opt1c
            
            
            #############
            ###  g4   ###
            #############
            g4A = gframe("Colour", container = g4, expand = TRUE)
            g4B = gframe("Size", container = g4, expand = TRUE)
            
            g4A$set_borderwidth(4)
            g4B$set_borderwidth(4)
            
            size(g4A) = c(122, 32)
            size(g4B) = c(122, 32)
            
            ## gradient & linear colours
            hiCols = c("red", "black", "navy", "blue")
            gCols = c(paste("white", hiCols, sep = " & "),
                      paste("green", hiCols, sep = " & "),
                      paste("yellow", hiCols, sep = " & "),
                      paste("orange", hiCols, sep = " & "))
            lCols= c("dark red", "dark blue", "dark green", "dark magenta",
                     "dark slate blue", "hot pink 4", "light salmon 2", 
                     "pale green 3", "steel blue 3")
            
            g4_opt1a = gcombobox(gCols, selected = 1, container = g4A,
                                 expand = TRUE)
            g4_opt1b = gcombobox(lCols, selected = 1, container = g4A,
                                 expand = TRUE)
            visible(g4_opt1a) = FALSE
            
            ## size
            g4_opt2 = gcombobox(1:5, selected = 2, container = g4B,
                                expand = TRUE)
            
            size(g4_opt1a) = c(112, 22)
            size(g4_opt1b) = c(112, 22)
            size(g4_opt2) = c(112, 22)
            
            ############
            ###  g5  ###
            ############
            g5NB = gnotebook(container = g5)
            
            ###########
            ## tab 1 ##
            ###########
            tab1 = glayout(spacing = 5, container = g5NB, label = "map")
            
            ## labels
            tab1_lab1 = glabel("type")
            tab1_lab2 = glabel("background")
            
            ## options
            tab1_opt1 = gcombobox(c("terrain", "satellite",
                                    "roadmap", "hybrid"),
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
            tab2 = glayout(spacing = 7, container = g5NB, label = "aesthetics")
            # visible(tab2) = FALSE
            
            ## labels
            tab2_lab1 = glabel("mode")
            tab2_lab2 = glabel("zoom")
            
            ## options
            tab2_opt1a = gcheckbox("colour", checked = FALSE)
            tab2_opt1b = gcheckbox("size", checked = FALSE)
            tab2_opt1c = gcheckbox("alpha", checked = FALSE)
            
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
            tab3 = gvbox(container = g5NB, label = "colour", expand = TRUE)
            tab3A = glayout(spacing = 11, container = tab3)
            tab3B = glayout(spacing = 11, container = tab3)
            visible(tab3A) = FALSE
            
            ## labels
            tab3_lab1 = glabel("gradient colour")
            tab3_lab2 = glabel("linear colour")
            
            ## options
            tab3_opt1a = gcombobox(c("low", "white", "green", 
                                     "yellow", "orange"), 
                                   selected = 1, editable = TRUE)
            tab3_opt1b = gcombobox(c("high", "red", "black", "navy", "blue"), 
                                   selected = 1, editable = TRUE)
            tab3_opt2 = gcombobox("darkred", selected = 1, editable = TRUE)
            
            size(tab3_opt1a) = c(80, 21)
            size(tab3_opt1b) = c(80, 21)
            size(tab3_opt2) = c(200, 21)
            
            addHandlerChanged(g2_opt1a, handler = function(h,...) {
                var = svalue(h$obj)
                varInd = svalue(h$obj, index = TRUE)
                
                ## tick "colour" mode if "Colour by" selected
                svalue(tab2_opt1a) = ifelse(varInd == 1, FALSE, TRUE)
                visible(tab3A) = svalue(tab2_opt1a)
                visible(tab3B) = !visible(tab3A)
                visible(g4_opt1a) = svalue(tab2_opt1a)  # for new design
                visible(g4_opt1b) = !visible(g4_opt1a)  # for new design
                
                ## leave if not variable not selected
                if (varInd == 1) {
                    visible(g2_lab1)  = TRUE
                    visible(g2_lab2)  = TRUE
                    visible(g2_opt1b) = TRUE
                    visible(g2_opt1c) = TRUE
                    visible(g2_opt1d) = FALSE
                    return()
                }
                
                ## delete current display
                replicate(2, delete(g2Layout, g2Layout$children[[5]]))
                ## reset indices
                for (i in length(g2Layout$children):5) {
                    g2Layout$children[[i + 2]] = g2Layout$children[[i]]
                }
                
                if (is.numeric(activeData[, var])) {
                    ## find min and max
                    dat = activeData[is.finite(activeData[[var]]), var]
                    varMin = min(dat, na.rm = TRUE)
                    varMax = max(dat, na.rm = TRUE)
                    
                    ## create buttons
                    g2_opt1b = gspinbutton(
                        from = varMin, to = varMax, by = 0.1, value = varMin)
                    g2_opt1c = gspinbutton(
                        from = varMin, to = varMax, by = 0.1, value = varMax)
                    
                    enabled(g2_opt1b) = TRUE
                    enabled(g2_opt1c) = TRUE
                    enabled(g4_opt1a) = TRUE
                    
                    visible(g2_lab1)   = TRUE
                    visible(g2_lab2)   = TRUE
                    visible(g2_opt1b)  = TRUE
                    visible(g2_opt1c)  = TRUE
                    visible(g2_opt1d)  = FALSE
                    svalue(tab2_opt1c) = TRUE
                } else {
                    ## create buttons
                    g2_opt1b = gspinbutton(0, 1, by = 0.1, value = 0)
                    g2_opt1c = gspinbutton(0, 1, by = 0.1, value = 1)
                    
                    ## disable them & make them invisible
                    enabled(g2_opt1b) = FALSE
                    enabled(g2_opt1c) = FALSE
                    visible(g2_opt1b) = FALSE
                    visible(g2_opt1c) = FALSE
                    
                    ## grey out colour option
                    enabled(g4_opt1a) = FALSE
                    
                    ## display shape option
                    visible(g2_opt1d) = TRUE
                    svalue(tab2_opt1c) = FALSE
                    
                    ## hide "from" and "to"
                    visible(g2_lab1) = FALSE
                    visible(g2_lab2) = FALSE
                }
                
                ## add
                g2Layout[2, 2, expand = TRUE] = g2_opt1b
                g2Layout[2, 4, expand = TRUE] = g2_opt1c
                
                ## update
                g2Layout$children[[6]] = g2_opt1b
                g2Layout$children[[5]] = g2_opt1c
            })
            addHandlerChanged(g3_opt1a, handler = function(h,...) {
                var = svalue(h$obj)
                varInd = svalue(h$obj, index = TRUE)
                
                ## tick "size" mode if "Size by" selected
                svalue(tab2_opt1b) = ifelse(varInd == 1, FALSE, TRUE)
                #enabled(g4_opt2) = !svalue(tab2_opt1b)
                #visible(tab4A) = svalue(tab2_opt1b)
                #visible(tab4B) = !visible(tab4A)
                
                ## leave if not variable not selected
                if (varInd == 1) { return() }
                
                ## delete current display
                replicate(2, delete(g3Layout, g3Layout$children[[4]]))
                ## reset indices
                for (i in length(g3Layout$children):4) {
                    g3Layout$children[[i + 2]] = g3Layout$children[[i]]
                }
                
                if (is.numeric(activeData[, var])) {
                    ## find min and max
                    dat = activeData[is.finite(activeData[[var]]), var]
                    varMin = min(dat, na.rm = TRUE)
                    varMax = max(dat, na.rm = TRUE)
                    
                    ## create buttons
                    g3_opt1b = gspinbutton(
                        from = varMin, to = varMax, by = 0.1, value = varMin)
                    g3_opt1c = gspinbutton(
                        from = varMin, to = varMax, by = 0.1, value = varMax)
                    
                    enabled(g3_opt1b) = TRUE
                    enabled(g3_opt1c) = TRUE
                } else {
                    ## create buttons
                    g3_opt1b = gspinbutton(0, 1, by = 0.1, value = 0)
                    g3_opt1c = gspinbutton(0, 1, by = 0.1, value = 0)
                    
                    ## grey them out
                    enabled(g3_opt1b) = FALSE
                    enabled(g3_opt1c) = FALSE
                }
                
                ## add
                g3Layout[2, 2, expand = TRUE] = g3_opt1b
                g3Layout[2, 4, expand = TRUE] = g3_opt1c
                
                ## update
                g3Layout$children[[5]] = g3_opt1b
                g3Layout$children[[4]] = g3_opt1c
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
            tab4 = gvbox(container = g5NB, label = "size", expand = TRUE)
            tab4A = glayout(spacing = 11, container = tab4)
            tab4B = glayout(spacing = 11, container = tab4)
            visible(tab4A) = FALSE
            
            ## labels
            tab4_lab1 = glabel("gradient scale")
            tab4_lab2 = glabel("linear scale")
            
            ## options
            tab4_opt1a = gcombobox(1:10, selected = 1, editable = TRUE)
            tab4_opt1b = gcombobox(1:10, selected = 6, editable = TRUE)
            tab4_opt2 = gcombobox(1:10, selected = 2, editable = TRUE)
            
            size(tab4_opt1a) = c(80, 21)
            size(tab4_opt1b) = c(80, 21)
            size(tab4_opt2) = c(200, 21)
            
            addHandlerChanged(tab2_opt1b, handler = function(h,...) {
                visible(tab4A) = svalue(h$obj)
                visible(tab4B) = !visible(tab4A)
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
            tab5 = glayout(spacing = 7, container = g5NB, label = "contour*")
            
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
            svalue(g5NB) = 1
            ## collapse the expandgroup as default
            visible(g5) = FALSE
            
            
            #############
            ###  g6   ###
            #############
            pAct = gaction("Plot points", handler = function(h,...) {
                svalue(h$obj) = "Replot points"
                arg = .self$argGenerator(buttonPressed = "points")
                cat(paste("\n", paste(names(unlist(arg[-1])), unlist(arg[-1])), "\n"))
                .self$plotButtonHandler(arg)
            })
            pButton = gbutton(action = pAct, container = g6, expand = TRUE)
            pButton$set_icon("gw-points")
            
            cAct = gaction("Plot contour", handler = function(h,...) {
                svalue(h$obj) = "Replot contour"
                arg = .self$argGenerator(buttonPressed = "contour")
                #cat(paste("\n", unlist(arg[-1]), "\n"))
                .self$plotButtonHandler(arg)
            })
            cButton = gbutton(action = cAct, container = g6, expand = TRUE)
            cButton$set_icon("gw-contour")
            
            cancelButton = gbutton("Cancel", handler = function(h,...) {
                ## delete the module window
                sapply(GUI$moduleWindow$children, 
                       function(x) delete(GUI$moduleWindow, x))
                ## display the default view (data, variable, etc.)
                visible(GUI$gp1) <<- TRUE
            },
            container = mainGrp)
            
            size(pButton) = size(cButton) = c(135, 28)
            size(cancelButton) = c(270, 28)
        },
        
#         chkData = function(data) {
#             ## find the variable names
#             lon = colnames(data)[grepl("^[Ll][Oo][Nn].*$", colnames(data))]
#             lat = colnames(data)[grepl("^[Ll][Aa][Tt].*$", colnames(data))]
#             
#             if (length(lon) > 0 & length(lat) > 0) {
#                 lon_vals = data[!is.na(data[[lon]]), lon]
#                 lat_vals = data[!is.na(data[[lat]]), lat]
#                 
#                 hasLon = any(!is.finite(lon_vals)) |
#                          any(lon_vals < -180) |
#                          any(lon_vals > 180)
#                 hasLat = any(!is.finite(lat_vals)) |
#                          any(lat_vals < -90) |
#                          any(lat_vals > 90)
#                 
#                 if (!hasLon | !hasLat) {
#                     
#                 }
#             }
#         }
#         
#         ## Function to check for a geographical data set.
#         geoData = function() {
#             if (!isGeoData(GUI$getActiveData())) {
#                 checkToProceed = gconfirm(
#                     paste("Cannot find longitudes and latitudes in data set.",
#                           "Proceed?", sep = "\n"),
#                     title = "Warning", icon = "warning"
#                 )
#             } else {
#                 return(TRUE)
#             }
#         },
#         
#         ## Function to handle proceed or stop
#         ## if the data set is not geographical.
#         checkGeoData = function() {
#             
#             if (!isGeoData(GUI$getActiveData())) {
#                 proceed = gconfirm(
#                     "The imported data are not geographical. ",
#                     title = "Warning",
#                     icon  = "warning"
#                 )
#             }
#             
#             if (!geoData()) {
#                 visible(GUI$gp1) <<- TRUE
#                 if (length(GUI$gp1$children[[1]]$children) == 3) {
#                     delete(GUI$gp1$children[[1]],
#                            GUI$gp1$children[[1]]$children[[1]])
#                 }
#                 return()
#             }
#         },
        
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
            g4Path = "mainGrp$children[[4]]"
            g5Path = "mainGrp$children[[5]]$children[[1]]$children[[1]]"
            g6Path = "mainGrp$children[[6]]"
            tab1Path = paste0(g5Path, "$children[[1]]")
            tab2Path = paste0(g5Path, "$children[[2]]")
            tab3Path = paste0(g5Path, "$children[[3]]")
            tab4Path = paste0(g5Path, "$children[[4]]")
            tab5Path = paste0(g5Path, "$children[[5]]")
            
            ## arg: data
            data = activeData
            
            ## arg: lon
            lon = svalue(evalText(paste0(g1Path, "$children[[5]]")))
            
            ## arg: lat
            lat = svalue(evalText(paste0(g1Path, "$children[[6]]")))
            
            ## arg: colour_by & colour_by_cond
            colbyAccess = paste0(g2Path, "$children[[4]]")
            cIndex = svalue(evalText(colbyAccess), index = TRUE)
            
            cLoAccess = paste0(g2Path, "$children[[6]]")
            cHiAccess = paste0(g2Path, "$children[[5]]")
            
            colour_by      = NULL
            colour_by_cond = NULL
            factor_by      = NULL
            if (cIndex > 1) {
                if (enabled(evalText(cLoAccess))) {
                    colour_by = svalue(evalText(colbyAccess))
                    cLo = svalue(evalText(cLoAccess))
                    cHi = svalue(evalText(cHiAccess))
                    colour_by_cond = 
                        paste(paste(cLo, "<=", paste0("data$", colour_by)),
                              paste(paste0("data$", colour_by), "<=", cHi),
                              sep = " & ")
                    } else {
                        factor_by = svalue(evalText(colbyAccess))
                    }
            }
            
            ## arg: size_by & size_by_cond
            sizebyAccess = paste0(g3Path, "$children[[3]]")
            sIndex = svalue(evalText(sizebyAccess), index = TRUE)
            if (sIndex == 1) {
                size_by = NULL
                size_by_cond = NULL
            } else {
                size_by = svalue(evalText(sizebyAccess))
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
            bcol = svalue(evalText(colAccess), index = TRUE)
            colour = ifelse(bcol == 1, "bw", "color")
            
            ## arg: type & mode
            modeAccess = paste0(tab2Path, "$children[[", 3:5, "]]")
            whichMode = sapply(modeAccess, function(x) svalue(evalText(x)))
            if (buttonPressed == "points") {
                ## type
                type = "point"
                ## mode
                selectedMode = paste(
                    c("\"colour\"", "\"size\"", "\"alpha\"")[whichMode], 
                    collapse = ", ")
                if (!is.null(factor_by)) {
                    selectedMode = paste0(selectedMode, ", \"shape\"")
                }
                mode = paste0("c(", selectedMode, ")")
            } else if (buttonPressed == "contour") {
                if (sIndex != 1) {
                    msg = paste(
                        "Plot contour uses \"Colour by\" information only.",
                        "Click okay to set \"Size by\" to default.", sep = "\n")
                    continue = gconfirm(msg, icon = "error")
                    if (continue) {
                        newInd = evalText(sizebyAccess)
                        svalue(newInd, index = TRUE) = 1
                        size_by = size_by_cond = NULL
                    } else {
                        return()
                    }
                }
                ## type
                type = "contour"
                ## mode
                mode = c("fill", "alpha")
                ## geom
                geom = "polygon"
            }
            
            ## arg: low, high & col
            colAccess1 = paste0(g4Path, "$children[[1]]$children[[1]]")
            colAccess2 = paste0(g4Path, "$children[[1]]$children[[2]]")
            if (svalue(evalText(paste0(tab2Path, "$children[[3]]")))) {
                rawCols = svalue(evalText(colAccess1[1]))
                strCols = unlist(strsplit(rawCols, " & "))
                col_low  = strCols[1]
                col_high = strCols[2]
                col = NULL
                if (!is.null(factor_by)) {
                    col = col_low = col_high = NULL
                }
            } else {
                col = svalue(evalText(colAccess2))
                col_low = col_high = NULL
                if (buttonPressed == "contour") {
                    col_low = "gray80"
                    col_high = col
                    col = NULL
                }
            }
            
            ## arg: size
            sizeAccess = paste0(g4Path, "$children[[2]]$children[[1]]")
            if (svalue(evalText(paste0(tab2Path, "$children[[4]]")))) {
                size = NULL
                sizeValue = svalue(evalText(sizeAccess))
                size_low = -1 + sizeValue
                size_high = 4 + sizeValue
            } else {
                size = as.numeric(svalue(evalText(sizeAccess)))
                size_low = size_high = NULL
            }
            
            ## arg: solid
            solidAccess = paste0(g2Path, "$children[[3]]")
            solid = FALSE
            if (!is.null(factor_by) & svalue(evalText(solidAccess))) {
                solid = TRUE
            }
            
            ## create arg as a list
            arg = list(data = data, lon = lon, lat = lat, factor_by = factor_by,
                       colour_by = colour_by, colour_by_cond = colour_by_cond,
                       col = col, col_low = col_low, col_high = col_high,
                       size_by = size_by, size_by_cond = size_by_cond,
                       size = size, size_low = size_low, size_high = size_high,
                       location = location, zoom = zoom, maptype = maptype,
                       colour = colour, type = type, mode = mode, solid = solid)
            if (buttonPressed == "contour") {
                arg$geom = geom
                arg$col = NULL
                arg$size = arg$size_low = arg$size_high = NULL
            }
            
            return(arg)
        },
        
        ## For the plot buttons
        plotButtonHandler = function(arg) {
            ##########################################
            #source"../iNZightMaps/R/timeCat.R")
            #source"../iNZightMaps/R/drawMap.R")
            #source"../iNZightMaps/R/draw.R")
            #source"../iNZightMaps/R/generateLine.R")
            #source"../iNZightMaps/R/getBB.R")
            #source"../iNZightMaps/R/isGeoData.R")
            #source"../iNZightMaps/R/varSubset.R")
            #source"../iNZightMaps/R/processFactor.R")
            library(ggmap)
            ##########################################
            map = do.call(draw, arg)
            plot(map)
        }
    )
)
