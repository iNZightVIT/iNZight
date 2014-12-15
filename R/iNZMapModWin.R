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
            initFields(GUI = GUI)
            
            ## set up main window
            mapWin <<- gwindow("Select variables to draw a map",
                               parent = GUI$win)
            mainGrp = ggroup(horizontal = FALSE, container = mapWin, 
                              expand = FALSE)
            mainGrp$set_borderwidth(15)
            
            ## create list of variables (dropdown)
            ## USE TAB HERE?
            
            ## set up dropdown variable selection
            topGrp = ggroup(horizontal = FALSE, container = mainGrp,
                            expand = FALSE)
            addSpring(topGrp)
            
            tbl = glayout(container = topGrp)
            tbl[1, 1] = glabel("Longitude")
            tbl[1, 2] = gcombobox(c("", names(GUI$getActiveData())),
                                   container = tbl)
            tbl[2, 1] = glabel("Latitude")
            tbl[2, 2] = gcombobox(c("", names(GUI$getActiveData())),
                                   container = tbl)
            tbl[3, 1] = glabel("Location")
            
            ###################################################################
            loc = gedit(text = "Type location", container = tbl)
            tbl[3, 2] = loc
            ###################################################################
            
            tbl[4, 1] = glabel("Variable")
            tbl[4, 2] = gcombobox(c("", names(GUI$getActiveData())),
                                  container = tbl)
            tbl[4, 3] = gcheckbox("Is it a factor?", checked = FALSE,
                                  container = tbl)
            tbl[5, 1] = glabel("Condition")
            tbl[5, 2] = gedit(text = "Type condition", container = tbl)
            tbl[6, 1:3] = gseparator(container = tbl)
            
            ## check box label
            lab = glabel("Select plot type", container = tbl)
            tbl[7, 1] = lab
            font(lab) = list(weight = "bold", family = "normal", size = 11)
            
            ## check box buttons
            usingMethods(opt1, opt2, opt3)
            chkGrp <<- ggroup(horizontal = FALSE, expand = FALSE)
            opts = gcheckboxgroup(c("Simple points",
                                    "2D density estimation contours"),
                                  checked = FALSE, horizontal = TRUE,
                                  container = chkGrp)
            add(mainGrp, chkGrp)
            addHandlerChanged(opts,
                              handler = function(h, ...) {
                                  changeOpts(svalue(h$obj,
                                                    index = TRUE))
                              })
            
            ## options checkbox
            optGrp <<- ggroup(horizontal = FALSE, exapnd = TRUE)
            add(mainGrp, optGrp)
        },
        
        #######################################################################
        mapButtonHandler = function() {
            svalue(.self$optGrp)
            ## get argument from selection and call drawMap()
        },
        #######################################################################
        
        changeOpts = function(index) {
            ## delete current displayed options
            invisible(sapply(optGrp$children, function(x) delete(optGrp, x)))
            do.call(paste0("opt", index), args = list())
        },
        
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
            okButton = gbutton(
                "Map",
                handler = function(h, ...) {
                    .self$mapButtonHandler()
                }, container = tbl)
            plotButton = gbutton(
                "Plot",
                handler = function(h, ...) {
                    
                }, container = tbl
            )
            cancelButton = gbutton(
                "Cancel",
                handler = function(h, ...) dispose(mapWin),
                container = tbl)
            
            tbl[1, 1:8] = gseparator(horizontal = TRUE, container = tbl)
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
            
            tbl[4, 1:4] = okButton
            tbl[4, 5:8] = plotButton
            tbl[6, 1:8] = cancelButton
        },
        
        ## opt2 ("contours")
        opt2 = function() {
            tbl = glayout(container = optGrp)
            lab1 = glabel("Select mode")
            font(lab1) = list(weight = "bold")
            optGrp1 = gcheckboxgroup(c("colour", "alpha"),
                                     checked = FALSE, horizontal = FALSE)
            lab2 = glabel("Select geometry")
            font(lab2) = list(weight = "bold")
            optGrp2 = gradio(c("polygon", "contour"),
                             selected = 1, container = tbl)
            
            tbl[1, 1] = lab1
            tbl[1, 2] = lab2
            
            tbl[2, 1] = optGrp1
            tbl[2, 2] = optGrp2
        },

        ## opt3 ("both")
        opt3 = function() {}
    )
)



