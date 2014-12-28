iNZMapModWin <- setRefClass(
    "iNZMapModWin",
    fields = list(
        GUI = "ANY",
        mapWin = "ANY",
        optGrp = "ANY",
        chkGrp = "ANY"
    ),
    methods = list(
        initialize = function(GUI) {
            #initFields(GUI = GUI)
            
            ## SET UP MAIN (MAP MODULE) WINDOW.
            mapWin <<- gwindow("Map...", 
                               ## trial & error size
                               width = 485, height = 100,
                               expand = FALSE) #, parent = GUI$win)
            mainGrp = ggroup(horizontal = FALSE,
                             spacing = 10,
                             use.scrollwindow = FALSE,
                             container = mapWin)
            mainGrp$set_borderwidth(10)
            
            ## MORE FIELDS
            mainFont = list(weight = "bold", family = "normal", size = 14)
            labFont = list(size = 11)
            
            ##################################
            ## TOP PORTION OF THE MAIN WINDOW.
            ##################################
            topWin = ggroup(horizontal = FALSE,
                            spacing = 10, container = mainGrp)
            addSpring(topWin)
            
            topLab = glabel("View map", container = topWin)
            font(topLab) = mainFont
            
            topLayout = glayout(container = topWin)
            topLab1 = glabel("Location", container = topLayout)
            font(topLab1) = labFont
            
            loc = gedit(text = "Type location", container = topLayout)
            
            okButton = gbutton(
                "MAP", 
                handler = function(h, ...) {
                    .self$mapButtonHandler(
                        svalue(loc), svalue(topOpt1),
                        svalue(topOpt2), svalue(topOpt3))
                }, container = topLayout)
            
            addSpring(topWin)
            topExpandGrp = gexpandgroup("Additional Options",
                                        horizontal = FALSE)
            
            topExpandGrp$set_borderwidth(10)
            visible(topExpandGrp) = FALSE
            
            topExpandGrpFrame = gframe(container = topExpandGrp,
                                       pos = 1, horizontal = FALSE)
            topExpandGrpFrame$set_borderwidth(5)
            
            topOptGrp = glayout(container = topExpandGrpFrame)
            topOptLab1 = glabel("Service:", container = topOptGrp)
            topOpt1 = gradio(c("Google", "OpenStreetMap"), 
                             horizontal = TRUE, selected = 1, 
                             container = topOptGrp)
            topOptLab2 = glabel("Map type:", container = topOptGrp)
            topOpt2 = gradio(c("Terrain", "Satellite", "Roadmap",
                               "Hybrid*", "Toner*"), 
                             horizontal = TRUE, selected = 1, 
                             container = topOptGrp)
            # topOptGrp3 = ggroup(container = topOptGrp)
            topOptLab3 = glabel("Zoom:", container = topOptGrp)
            # size(topLab3) = c(62, 10)
            topOpt3 = gslider(from = 3, to = 21, by = 1, value = 10, 
                              container = topOptGrp, fill="both")
            
            topOptGrp[1, 1, expand = TRUE, anchor = c(1, 0)] = topOptLab1
            topOptGrp[1, 2, expand = TRUE] = topOpt1
            topOptGrp[2, 1, expand = TRUE, anchor = c(1, 0)] = topOptLab2
            topOptGrp[2, 2, expand = TRUE] = topOpt2
            topOptGrp[3, 1, expand = TRUE, anchor = c(1, 0)] = topOptLab3
            topOptGrp[3, 2, expand = TRUE] = topOpt3
            
            
            ## can't add space using "addSpring(topExpandGrp)"
            ## so an empty label is used.
            glabel("", container = topExpandGrp)
            glabel("*NOTE: Uses the Stamen Maps service",
                   container = topExpandGrp)
            
            topLayout[1, 1, expand = TRUE, anchor = c(0, 0)] = topLab1
            topLayout[1, 2:3, expand = TRUE] = loc
            topLayout[1, 4, expand = TRUE] = okButton
            
            add(topWin, topExpandGrp)
            add(topWin, gseparator())
            
            ##################################
            ## MID PORTION OF THE MAIN WINDOW.
            ##################################
            midWin = ggroup(horizontal = FALSE,
                            spacing = 10, container = mainGrp)
            midLab = glabel("Plot on map", container = midWin)
            font(midLab) = mainFont
            
            midLayout = glayout(container = midWin)

#             tbl[1, 1] = glabel("Longitude")
#             tbl[1, 2] = gcombobox(c("", names(GUI$getActiveData())),
#                                    container = tbl)
#             tbl[2, 1] = glabel("Latitude")
#             tbl[2, 2] = gcombobox(c("", names(GUI$getActiveData())),
#                                    container = tbl)
#             tbl[3, 1] = glabel("Location")
#             ###################################################################
#             #loc = gedit(text = "Type location", container = tbl)
#             #tbl[3, 2] = loc
#             ###################################################################
#             
#             tbl[3, 3] = okButton
#             tbl[4, 2] = gcombobox(c("", names(GUI$getActiveData())),
#                                   container = tbl)
#             tbl[4, 3] = gcheckbox("Is it a factor?", checked = FALSE,
#                                   container = tbl)
#             tbl[5, 1] = glabel("Condition")
#             tbl[5, 2] = gedit(text = "Type condition", container = tbl)
#             tbl[6, 1:3] = gseparator(container = tbl)
#             
#             ## check box label
#             lab = glabel("Select plot type", container = tbl)
#             tbl[7, 1] = lab
#             font(lab) = list(weight = "bold", family = "normal", size = 11)
#             
#             ## check box
#             usingMethods(opt, opt1, opt2, opt3)
#             chkGrp <<- ggroup(horizontal = TRUE, expand = FALSE)
#             opts = gcheckboxgroup(c("Simple points",
#                                     "2D density estimation contours"),
#                                   checked = FALSE, horizontal = TRUE,
#                                   container = chkGrp)
#             add(mainGrp, chkGrp)
#             addHandlerChanged(opts,
#                               handler = function(h, ...) {
#                                   changeOpts(svalue(h$obj,
#                                                     index = TRUE))
#                               })
#             
#             ## options checkbox
#             optGrp <<- ggroup(horizontal = FALSE, exapnd = TRUE)
#             add(mainGrp, optGrp)
#             
#             ## Close button.
#             cancelButton = gbutton(
#                 "Cancel",
#                 handler = function(h, ...) dispose(mapWin),
#                 container = tbl)
        },
        
        #######################################################################
        mapButtonHandler = function(loc, service, mapType, zoom) {
            
            library(ggmap)
            source("../iNZightMaps/R/drawMap.R")
            source("../iNZightMaps/R/getBB.R")
            source("../iNZightMaps/R/general.R")
            
            src = tolower(service)
            src = gsub("^openstreetmap$", "osm", src)
            maptype = tolower(mapType)
            maptype = gsub("[*]$", "", maptype)
            
            arg = list(location = loc, zoom = zoom,
                       maptype = maptype, src = src)
            
            map = try(do.call(drawMap, arg), silent = TRUE)
            
            if (inherits(map, "try-error"))
                gmessage(paste("Location not found."))
            else
                plot(map)
        },
        #######################################################################
        
        changeOpts = function(index) {
            ## delete current displayed options
            invisible(sapply(optGrp$children, function(x) delete(optGrp, x)))
            do.call(paste0("opt", index), args = list())
        },
        
        ## opt (when unchecked. display nothing)
        opt = function() {},
        
        ## opt1 ("simple points")
        opt1 = function() {
            tbl = glayout(container = optGrp)
            #
            lab1 = glabel("Select mode", container = tbl)
            font(lab1) = list(weight = "bold")
            optGrp1 = gcheckboxgroup(c("colour", "size", "alpha", "shape"),
                                     checked = FALSE, horizontal = FALSE,
                                     container = tbl)
            lab2 = glabel("Select colour")
            font(lab2) = list(weight = "bold")
            optGrp2 = ggroup(horizontal = FALSE, expand = FALSE, 
                             container = tbl)
            optGrp2a = gcombobox(c("Low", "Blue", "red", "green", "add more.."),
                                selected = 1, container = optGrp2)
            optGrp2b = gcombobox(c("High", "blah"),
                                 selected = 1, container = optGrp2)
            optGrp2c = gcombobox(c("colour", "add more.."),
                                 selected = 1, container = optGrp2)
            lab3 = glabel("Specify size", container = tbl)
            font(lab3) = list(weight = "bold")
            optGrp3 = gslider(from = 0.1, to = 2.0, by = 0.1, value = 1.0,
                              container = tbl)
            lab4 = glabel("Factor options", container = tbl)
            font(lab4) = list(weight = "bold")
            optGrp4 = ggroup(horizontal = FALSE, expand = FALSE,
                             container = tbl)
            optGrp4a = gcheckbox("solid shape", checked = FALSE,
                                 container = optGrp4)
            optGrp4b = gcheckbox("use grid", checked = FALSE,
                                 container = optGrp4)
            
            ## set up buttons
            plotButton = gbutton(
                "Plot",
                handler = function(h, ...) {
                    
                }, container = tbl
            )
            cancelButton = gbutton(
                "Cancel",
                handler = function(h, ...) dispose(mapWin),
                container = tbl)
            
            tbl[1, 1:8, expand = TRUE] = gseparator(horizontal = TRUE, container = tbl)
            tbl[2:3, 2] = gseparator(horizontal = FALSE, container = tbl)
            tbl[2:3, 4] = gseparator(horizontal = FALSE, container = tbl)
            tbl[2:3, 6] = gseparator(horizontal = FALSE, container = tbl)
            
            tbl[2, 1] = lab1
            tbl[2, 3] = lab2
            tbl[2, 5] = lab3
            tbl[2, 7] = lab4
            
            tbl[3, 1] = optGrp1
            tbl[3, 3] = optGrp2
            tbl[3, 5] = optGrp3
            tbl[3, 7] = optGrp4
            
            tbl[4, 1:4] = plotButton
            tbl[4, 5:8] = cancelButton
        },
        
        ## opt2 ("contours")
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

iNZMapModWin()



