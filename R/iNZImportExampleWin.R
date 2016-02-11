iNZImportExampleWin <-
    setRefClass(
        "iNZImportExampleWin",
        fields = list(
            GUI = "ANY",
            importFileWin = "ANY",
            datasets = "ANY"
            ),
        methods = list(

            initialize = function(gui) {
                initFields(GUI = gui)

                ## packages that have data in them:
                pkgs <- c("iNZight", "iNZightMaps", "survey")
                
                ## create the window
                importFileWin <<- gwindow("Load Example Data", parent = GUI$win, visible = FALSE,
                                          height = 200, width = 600)
                mainGrp <- gvbox(container = importFileWin, expand = TRUE, fill = TRUE)
                mainGrp$set_borderwidth(15)
                
                tbl <- glayout(container = mainGrp, homogeneous = TRUE)
                ii <- 1

                ## select box populated with packages (defined above)
                lbl <- glabel("Module (package) :")
                dsPkg <- gcombobox(pkgs, selected = 1)
                tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- dsPkg
                ii <- ii + 1


                ## select box populated with datasets in chosen package
                lbl <- glabel("Dataset :")
                dsData <- gcombobox("", selected = 0)
                tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- dsData
                ii <- ii + 1

                
                tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- glabel("Title :")
                dsTitle <- glabel("")
                tbl[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- dsTitle

                addSpring(mainGrp)

                setDataMenu <- function() {
                    datasets <<- data(package = svalue(dsPkg))$results
                    ## dsNames <- ifelse(datasets[, "Title"] == "", datasets[, "Item"], datasets[, "Title"])
                    dsData$set_items(datasets[, "Item"])
                }
                setDataMenu()

                ## Change Handlers:
                addHandlerChanged(dsPkg, function(h, ...) {
                                      ## set the dataset menu
                                      setDataMenu()
                                  })
                addHandlerChanged(dsData, function(h, ...) {
                                      svalue(dsTitle) <- datasets[svalue(dsData, index = TRUE), "Title"]
                                  })

                
                ## OK / Cancel buttons
                btnGrp <- ggroup(cont = mainGrp)
                addSpring(btnGrp)

                cancelBtn <- gbutton("Cancel", expand = TRUE, cont = btnGrp,
                                     handler = function(h, ...) {
                                         dispose(importFileWin)
                                     })

                okBtn <- gbutton("OK", expand = TRUE, cont = btnGrp,
                                 handler = function(h, ...) {
                                     ## Set the data - will need to 'load' it into an evironment, then
                                     ## reassign it:
                                     ind <- svalue(dsData, index = TRUE)
                                     dname <- datasets[ind, "Item"]
                                     tmp.env <- new.env()
                                     data(list = dname, package = svalue(dsPkg), envir = tmp.env)

                                     ## Set the name to the title (or Item if title missing)
                                     attr(tmp.env[[dname]], "name") <- svalue(dsData)
                                     
                                     GUI$setDocument(iNZDocument$new(data = tmp.env[[dname]]))

                                     ## clean up
                                     rm("tmp.env")
                                     dispose(importFileWin)
                                 })
                
                
                visible(importFileWin) <<- TRUE
                
            }
            
            )
        )
