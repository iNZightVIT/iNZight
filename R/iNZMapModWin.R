iNZMapModWin <- setRefClass(
    "iNZMapModWin",
    fields = list(
        GUI = "ANY",
        mapWin = "ANY",
        optGrp = "ANY"
    ),
    methods = list(
        initialize = function(GUI) {
            initFields(GUI = GUI)
            
            ## SET UP MAIN (MAP MODULE) WINDOW.
            mapWin <<- gwindow("Map...", 
                               width = 500, height = 250, ## trial & error size
                               expand = FALSE, parent = GUI$win)
            
            mainGrp = ggroup(horizontal = FALSE,
                             spacing = 10,
                             use.scrollwindow = FALSE,
                             container = mapWin)
            mainGrp$set_borderwidth(10)
            
            ## MORE FIELDS
            mainFont = list(weight = "bold", family = "normal", size = 12)
            labFont = list(size = 11)
            
            #########################
            ###  TOP MAIN WINDOW  ###
            #########################
            topWin = ggroup(horizontal = FALSE,
                            spacing = 10, container = mainGrp)
            
            topLab = glabel("View map", container = topWin)
            font(topLab) = mainFont
            
            topLayout = glayout(container = topWin)
            topLab1 = glabel("Location", container = topLayout)
            font(topLab1) = labFont
            
            loc = gedit(text = "Type location", container = topLayout)
            addHandlerKeystroke(
                loc,
                handler = function(h, ...) {
                    if (!grepl("^Ty.+$", svalue(h$obj)))
                        enabled(mapButton) = TRUE
                }
            )
            
            mapButtonAction = gaction(
                "Map",
                tooltip = "Click to see the map of the location",
                icon = "gtk-find",
                handler = function(h, ...) {
                    .self$mapButtonHandler(
                        svalue(loc), svalue(topOpt1),
                        svalue(topOpt2), svalue(topOpt3))
                }, container = topLayout
            )
            mapButton = gbutton(action = mapButtonAction)
            enabled(mapButton) = FALSE
            
            topExpandGrp = gexpandgroup("Additional options",
                                        horizontal = FALSE)
            topExpandGrp$set_borderwidth(5)
            visible(topExpandGrp) = FALSE
            
            topOptLayout = glayout(container = topExpandGrp)
            topOptLab1 = glabel("service", container = topOptLayout)
            topOptLab2 = glabel("map type", container = topOptLayout)
            topOptLab3 = glabel("zoom", container = topOptLayout)
            
            topOpt1 = gcombobox(
                c("Google", "OpenStreetMap"),
                selected = 1, container = topOptLayout
            )
            topOpt2 = gcombobox(
                c("terrain", "satellite", "roadmap", "hybrid*", "toner*"), 
                selected = 1, 
                handler = function(h,...) {
                    ## delete "*NOTE" label if any
                    l = length(topExpandGrp$children)
                    if (l > 1) {
                        replicate(2, delete(topExpandGrp, 
                                            topExpandGrp$children[[2]]))
                    }
                    
                    i = svalue(h$obj, index = TRUE)
                    if (i == 4 | i == 5) {
                        glabel("", container = topExpandGrp)    # to add space
                        glabel("*NOTE: Uses the Stamen Maps service",
                               container = topExpandGrp)
                    }
                }, container = topOptLayout
            )
            topOpt3 = gslider(
                from = 3, to = 21, by = 1, value = 10, 
                container = topOptLayout
            )
            
            ## box/frame around
            topOptLayout[1, 1:5] = gseparator(horizontal = TRUE)
            topOptLayout[5, 1:5] = gseparator(horizontal = TRUE)
            topOptLayout[1:5, 1] = gseparator(horizontal = FALSE)
            topOptLayout[1:5, 5] = gseparator(horizontal = FALSE)
            
            topOptLayout[2, 2, expand = TRUE, anchor = c(-1, 0)] = topOptLab1
            topOptLayout[2, 4, expand = TRUE] = topOpt1
            topOptLayout[3, 2, expand = TRUE, anchor = c(-1, 0)] = topOptLab2
            topOptLayout[3, 4, expand = TRUE] = topOpt2
            topOptLayout[4, 2, expand = TRUE, anchor = c(-1, 0)] = topOptLab3
            topOptLayout[4, 3:4, expand = TRUE] = topOpt3
            
            topLayout[1, 1, expand = TRUE, anchor = c(-1, 0)] = topLab1
            topLayout[1, 2:3, expand = TRUE] = loc
            topLayout[1, 5, expand = TRUE] = mapButton
            
            add(topWin, topExpandGrp)
            
            #########################
            ###  MID MAIN WINDOW  ###
            #########################
            addSpring(topWin)
            midWin = ggroup(horizontal = FALSE,
                            spacing = 10, container = mainGrp)
            
            midLab = glabel("Plot on map", container = midWin)
            font(midLab) = mainFont
            addSpring(midWin)
            
            midLayout = glayout(container = midWin, spacing = 10)
            midLab1 = glabel("Longitude", container = midLayout)
            midLab2 = glabel("Latitude", container = midLayout)
            midLab3 = glabel("Variable", container = midLayout)
            midLab4 = glabel("from", container = midLayout)
            midLab5 = glabel("to", container = midLayout)
            midLab6 = glabel("Type of plot", container = midLayout)
            
            font(midLab1) = labFont
            font(midLab2) = labFont
            font(midLab3) = labFont
            font(midLab4) = labFont
            font(midLab5) = labFont
            font(midLab6) = labFont
            
            variables = names(GUI$getActiveData())
            midOpt1 = gcombobox(c("", variables), container = midLayout)
            midOpt2 = gcombobox(c("", variables), container = midLayout)
            midOpt3 = gcombobox(c("", variables), container = midLayout)
            midOpt4 = gspinbutton(from = 0, to = 1, length.out = 10, 
                                  value = 0, container = midLayout)
            size(midOpt4) = c(50, 20)
            midOpt5 = gspinbutton(from = 0, to = 1, length.out = 10,
                                  value = 0, container = midLayout)
            
            size(midOpt1) = c(170, 25)
            size(midOpt2) = c(170, 25)
            size(midOpt3) = c(170, 25)
            size(midOpt4) = c(70, 20)
            size(midOpt5) = c(70, 20)
            
            addHandlerChanged(
                midOpt3,
                handler = function(h,...) {
                    var = svalue(midOpt3)
                    if (!(var %in% names(GUI$getActiveData())))
                        return()
                    
                    ## Delete the spin buttons
                    replicate(2, delete(midLayout, midLayout$children[[10]]))
                    ## Reset the indices
                    midLayout$children[[12]] = midLayout$children[[10]]
                    midLayout$children[[13]] = midLayout$children[[11]]
                    midLayout$children[[14]] = midLayout$children[[12]]
                    
                    ## Find the min & max of the selected variable
                    dat = GUI$getActiveData()[[var]]
                    
                    if (is.numeric(dat)) {
                        ## Numeric variable
                        min = min(dat, na.rm = TRUE)
                        max = max(dat, na.rm = TRUE)
                        # length(gregexpr("[[:digit:]]", as.character(x))[[1]])
                        
                        ## Create the spin buttons
                        midOpt4 = gspinbutton(
                            from = min, to = max, length.out = 100, 
                            value = min, container = midLayout)
                        
                        midOpt5 = gspinbutton(
                            from = min, to = max, length.out = 100, 
                            value = max, container = midLayout)
                    } else {
                        ## Factor
                        midOpt4 = glabel("")
                        midOpt5 = glabel("")
                        
                        ## TO-DO
                    }
                    
                    size(midOpt4) = c(70, 20)
                    size(midOpt5) = c(70, 20)
                    
                    ## Add them
                    midLayout[2, 7, expand = F] = midOpt4
                    midLayout[2, 9, expand = F] = midOpt5
                    
                    ## Update the indices
                    midLayout$children[[10]] = midOpt4
                    midLayout$children[[11]] = midOpt5
                }
            )
            
            midOpt6 = gcheckboxgroup(
                c("point","contour"),
                checked = FALSE, horizontal = TRUE, 
                handler = function(h,...)
                    changeOpts(svalue(h$obj, index = TRUE)),
                container = midLayout
            )
            
            usingMethods(opt, opt1, opt2, opt3)
            
            cancelButtonAction = gaction(
                "Cancel",
                tooltip = "Click to cancel",
                icon = "gtk-cancel",
                handler = function(h, ...) {
                    dispose(mapWin)
                }, container = midLayout
            )
            cancelButton = gbutton(action = cancelButtonAction)
            
            # midLayout[1, 1:3, expand = TRUE, anchor = c(-1, 0)] = midLab1
            # midLayout[2, 1:3, expand = TRUE, anchor = c(-1, 0)] = midLab2
            # midLayout[3, 1:3, expand = TRUE, anchor = c(-1, 0)] = midLab3
            # midLayout[4, 4, expand = TRUE, anchor = c(1, 0)] = midLab4
            # midLayout[4, 6, expand = TRUE, anchor = c(1, 0)] = midLab5
            # midLayout[5, 1:3, expand = TRUE, anchor = c(-1, 0)] = midLab6
            # midLayout[1, 4:7, expand = TRUE] = midOpt1
            # midLayout[2, 4:7, expand = TRUE] = midOpt2
            # midLayout[3, 4:7, expand = TRUE] = midOpt3
            # midLayout[4, 5, expand = TRUE] = midOpt4
            # midLayout[4, 7, expand = TRUE] = midOpt5
            # midLayout[5, 4:7, expand = TRUE] = midOpt6
            # midLayout[7, 6:7, expand = TRUE] = cancelButton
            
            midLayout[1, 1, expand = TRUE, anchor = c(-1, 0)] = midLab1
            midLayout[2, 1, expand = TRUE, anchor = c(-1, 0)] = midLab2
            midLayout[1, 6, expand = TRUE, anchor = c(-1, 0)] = midLab3
            midLayout[2, 6, expand = TRUE, anchor = c(1, 0)] = midLab4
            midLayout[2, 8, expand = TRUE, anchor = c(1, 0)] = midLab5
            midLayout[3, 1, expand = TRUE, anchor = c(-1, 0)] = midLab6
            
            midLayout[1, 2:4, expand = TRUE] = midOpt1
            midLayout[2, 2:4, expand = TRUE] = midOpt2
            midLayout[1, 7:9, expand = TRUE] = midOpt3
            midLayout[2, 7, expand = TRUE] = midOpt4
            midLayout[2, 9, expand = TRUE] = midOpt5
            midLayout[3, 2:4, expand = TRUE] = midOpt6
            midLayout[3, 8:9, expand = TRUE] = cancelButton
            midLayout[1:3, 5] = gseparator(horizontal = FALSE)
            
            optGrp <<- ggroup()
            add(mainGrp, optGrp)
            
            ## plot button is made and assigned after adding optGrp to mainGrp
            ## so that the options in optGrp can be accessed.
            plotButtonAction = gaction(
                "Plot",
                tooltip = "Click to plot with current settings",
                icon = "gtk-apply",
                handler = function(h, ...) {
                    ## checks
                    vars = c(svalue(midOpt1), 
                             svalue(midOpt2), 
                             svalue(midOpt3))
                    type = svalue(midOpt6)
                    # if (any(vars == "") | length(type == 0)) {
                    #     return(gmessage("All settings must be specified",
                    #                     title = "Error", icon = "error"))
                    # }
                    
                    ## create string paths to access options
                    path = "optGrp$children[[1]]$children[[1]]"
                    pathToTab1 = paste0(path, "$children[[1]]")
                    pathToTab2 = paste0(path, "$children[[2]]")
                    
                    ## tab 1
                    pathToTab1Opts = paste0(
                        pathToTab1, "$children[[", 2:4, "]]"
                    )
                    whichMode = sapply(
                        pathToTab1Opts, 
                        function(x) svalue(eval(parse(text = x)))
                    )
                    mode = paste0(
                        "c(", 
                        paste(c("\"size\"",
                                "\"colour\"",
                                "\"alpha\"")[whichMode], collapse = ", "),
                        ")"
                    )
                    
                    ## tab 2
                    pathToTab2Opts = paste0(
                        pathToTab2, "$children[[", 1:4, "]]"
                    )
                    whichColour = svalue(
                        eval(parse(text = pathToTab2Opts[1])), index = TRUE
                    )
                    if (whichColour == 1) {
                        col = svalue(eval(parse(text = pathToTab2Opts[2])))
                        low = NULL
                        high = NULL
                    } else if (whichColour == 2) {
                        col = NULL
                        low = svalue(eval(parse(text = pathToTab2Opts[3])))
                        high = svalue(eval(parse(text = pathToTab2Opts[4])))
                    }
                    
                    from = svalue(
                        mapWin$children[[1]]$children[[2]]$
                            children[[2]]$children[[10]]
                    )
                    to = svalue(
                        mapWin$children[[1]]$children[[2]]$
                            children[[2]]$children[[11]]
                    )
                    
                    .self$plotButtonHandler(
                        vars, from, to, svalue(loc), svalue(topOpt3),
                        svalue(topOpt2), svalue(topOpt1),
                        type, mode, low, high, size = NULL, col = NULL
                    )
                    
                }, container = midLayout
            )
            plotButton = gbutton(action = plotButtonAction)
            midLayout[3, 6:7, expand = TRUE] = plotButton
        },
        
        #######################################################################
        mapButtonHandler = function(loc, service, mapType, zoom) {
            ## CHECK HOW NAMESPACE WORKS IN RGWIDGETS2
            library(ggmap)
            source("../iNZightMaps/R/drawMap.R")
            source("../iNZightMaps/R/getBB.R")
            source("../iNZightMaps/R/general.R")
            ###########################################
            
            if (grepl("^$", loc))
                return(gmessage("Specify location.", 
                                title = "Error", icon = "error"))
            
            src = tolower(service)
            src = gsub("^openstreetmap$", "osm", src)
            maptype = tolower(mapType)
            maptype = gsub("[*]$", "", maptype)
            
            arg = list(location = loc, zoom = zoom,
                       maptype = maptype, src = src)
            map = try(do.call(drawMap, arg), silent = TRUE)
            if (inherits(map, "try-error"))
                gmessage("Location not found.", 
                         title = "Error", icon = "error")
            else
                plot(map)
        },
        
        plotButtonHandler = function(vars, from, to,
                                     location, zoom, maptype, service,
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
            
            dat = GUI$getActiveData()
            
            lon = vars[1]
            lat = vars[2]
            var = vars[3]
            
            cond = paste(
                paste(from, "<=", paste0("dat$", var)),
                paste(paste0("dat$", var), "<=", to),
                sep = " & "
            )
            
            src = tolower(service)
            src = gsub("^openstreetmap$", "osm", src)
            maptype = tolower(maptype)
            maptype = gsub("[*]$", "", maptype)
            
            arg = list(data = dat, lon = lon, lat = lat, 
                       var = var, var.cond = cond, 
                       location = location, zoom = zoom,
                       maptype = maptype, src = src, type = type, 
                       mode = mode, low = low, high = high, 
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
            tab1[2, 4, expand = TRUE, fill = "y"] = tab1.opt1
            tab1[2, 5, expand = TRUE, fill = "y"] = tab1.opt2
            tab1[2, 6, expand = TRUE, fill = "y"] = tab1.opt3
            
            ## box
            tab1[1, 1:7] = gseparator(horizontal = TRUE)
            tab1[3, 1:7] = gseparator(horizontal = TRUE)
            tab1[1:3, 1] = gseparator(horizontal = FALSE)
            tab1[1:3, 7] = gseparator(horizontal = FALSE)
            
            ###############
            ###  TAB 2  ###
            ###############
            tab2 = glayout(container = optsNB, label = "colour")
            tab2.opt1 = gradio(c("linear colour", "gradient colour"), 
                               selected = 1, horizontal = FALSE)
            tab2.opt2 = gcombobox("default", selected = 1, editable = TRUE)
            tab2.opt3a = gcombobox("low", selected = 1, editable = TRUE)
            tab2.opt3b = gcombobox("high", selected = 1, editable = TRUE)
            enabled(tab2.opt3a) = FALSE
            enabled(tab2.opt3b) = FALSE
            
            addHandlerChanged(
                tab2.opt1,
                handler = function(h,..) {
                    if (svalue(h$obj, index = TRUE) == 1) {
                        enabled(tab2.opt2) = TRUE
                        enabled(tab2.opt3a) = FALSE
                        enabled(tab2.opt3b) = FALSE
                    } else {
                        enabled(tab2.opt2) = FALSE
                        enabled(tab2.opt3a) = TRUE
                        enabled(tab2.opt3b) = TRUE
                    }
                }
            )
            
            size(tab2.opt2) = c(200, 23)
            size(tab2.opt3a) = c(80, 23)
            size(tab2.opt3b) = c(80, 23)
            
            tab2[2:3, 2, expand = TRUE] = tab2.opt1
            tab2[2, 4:5, expand = TRUE] = tab2.opt2
            tab2[3, 4, expand = TRUE] = tab2.opt3a
            tab2[3, 5, expand = TRUE] = tab2.opt3b
            
            ## box
            tab2[1, 1:6] = gseparator(horizontal = TRUE)
            tab2[4, 1:6] = gseparator(horizontal = TRUE)
            tab2[1:4, 1] = gseparator(horizontal = FALSE)
            tab2[1:4, 6] = gseparator(horizontal = FALSE)
            
            ###############
            ###  TAB 3  ###
            ###############
            tab3 = glayout(container = optsNB, label = "size")
            
            
#             optsLab1 = glabel("Mode")
#             optsLab2 = glabel("Colour")
#             optsLab3 = glabel("Symbol scale")
#             
#             opts1 = gcheckboxgroup(
#                 c("colour", "size", "alpha"),
#                 checked = c(1, 2, 3),
#                 horizontal = TRUE
#             )
#             
#             optsLayout[1, 1] = optsLab1
#             optsLayout[1, 2:8, expand = TRUE] = opts1
#             optsLayout[2, 1] =optsLab2
#             
#             tbl = glayout(container = optGrp)
#             lab1 = glabel("Select mode", container = tbl)
#             font(lab1) = list(weight = "bold")
#             optGrp1 = gcheckboxgroup(c("colour", "size", "alpha", "shape"),
#                                      checked = FALSE, horizontal = FALSE,
#                                      container = tbl)
#             lab2 = glabel("Select colour")
#             font(lab2) = list(weight = "bold")
#             optGrp2 = ggroup(horizontal = FALSE, expand = FALSE, 
#                              container = tbl)
#             optGrp2a = gcombobox(c("Low", "Blue", "red", "green", "add more.."),
#                                 selected = 1, container = optGrp2)
#             optGrp2b = gcombobox(c("High", "blah"),
#                                  selected = 1, container = optGrp2)
#             optGrp2c = gcombobox(c("colour", "add more.."),
#                                  selected = 1, container = optGrp2)
#             lab3 = glabel("Specify size", container = tbl)
#             font(lab3) = list(weight = "bold")
#             optGrp3 = gslider(from = 0.1, to = 2.0, by = 0.1, value = 1.0,
#                               container = tbl)
#             lab4 = glabel("Factor options", container = tbl)
#             font(lab4) = list(weight = "bold")
#             optGrp4 = ggroup(horizontal = FALSE, expand = FALSE,
#                              container = tbl)
#             optGrp4a = gcheckbox("solid shape", checked = FALSE,
#                                  container = optGrp4)
#             optGrp4b = gcheckbox("use grid", checked = FALSE,
#                                  container = optGrp4)
#             
#             tbl[1, 1:8, expand = TRUE] = gseparator(horizontal = TRUE)
#             tbl[2:3, 2] = gseparator(horizontal = FALSE)
#             tbl[2:3, 4] = gseparator(horizontal = FALSE)
#             tbl[2:3, 6] = gseparator(horizontal = FALSE)
#             
#             tbl[2, 1] = lab1
#             tbl[2, 3] = lab2
#             tbl[2, 5] = lab3
#             tbl[2, 7] = lab4
#             
#             tbl[3, 1] = optGrp1
#             tbl[3, 3] = optGrp2
#             tbl[3, 5] = optGrp3
#             tbl[3, 7] = optGrp4
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



