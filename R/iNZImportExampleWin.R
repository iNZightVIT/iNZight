iNZImportExampleWin <- setRefClass("iNZImportExampleWin",
    fields = list(
        GUI = "ANY",
        importFileWin = "ANY",
        datasets = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            initFields(GUI = gui)

            ## packages that have data in them:
            pkgsL <- list(
                "iNZight" = "Default",
                "iNZightMR" = "Multiple Response",
                "iNZightTS" = "Time Series",
                "iNZightMaps" = "Maps",
                "survey" = "Survey",
                "FutureLearnData" = "FutureLearn"
            )
            pkgs <- unlist(pkgsL)  ## becomes a named vector

            pkgs <- pkgs[names(pkgs) %in% rownames(installed.packages())]

            ## create the window
            importFileWin <<- gwindow("Load Example Data",
                parent = GUI$win,
                visible = FALSE,
                height = 200,
                width = 600
            )
            mainGrp <- gvbox(
                container = importFileWin,
                expand = TRUE,
                fill = TRUE
            )
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
                svalue(dsTitle) <- ""
                pkgname <- names(pkgs)[svalue(dsPkg, index = TRUE)]
                ds <- data(package = pkgname)$results
                # filter out non-data.frame ones
                dfs <- sapply(ds[, "Item"], function(x) {
                    d <- x
                    if (pkgname == "survey" && grepl('\\(.+\\)', x)) {
                        d <- gsub(" \\(.+\\)", "", x)
                        x <- gsub("\\)", "", gsub(".+\\(", "", x))
                        x <- gsub(" \\(.+", "", x)
                    }
                    check <- sprintf("data(%s, package = '%s')", x, pkgname)
                    eval(parse(text = check))
                    eval(parse(text = sprintf("is.data.frame(%s)", d)))
                })
                datasets <<- ds[dfs, , drop = FALSE]
                dsData$set_items(datasets[, "Item"])
            }
            setDataMenu()

            ## Change Handlers:
            addHandlerChanged(dsPkg,
                function(h, ...) {
                    ## set the dataset menu
                    setDataMenu()
                }
            )
            addHandlerChanged(dsData,
                function(h, ...) {
                    ttl <- datasets[svalue(dsData, index = TRUE), "Title"]
                    if (ttl == "")
                        ttl <- svalue(dsData)
                    svalue(dsTitle) <- ttl
                }
            )


            ## OK / Cancel buttons
            btnGrp <- ggroup(cont = mainGrp)
            addSpring(btnGrp)

            cancelBtn <- gbutton("Cancel",
                expand = TRUE,
                cont = btnGrp,
                handler = function(h, ...) {
                    dispose(importFileWin)
                }
            )

            okBtn <- gbutton("Ok",
                expand = TRUE,
                cont = btnGrp,
                handler = function(h, ...) {
                    ## Set the data - will need to 'load' it into an evironment, then
                    ## reassign it:
                    ind <- svalue(dsData, index = TRUE)
                    dataName <- dname <- datasets[ind, "Item"]
                    pkgname <- names(pkgs)[svalue(dsPkg, index = TRUE)]

                    if (pkgname == "survey") {
                        if (grepl('\\(.+\\)', dname)) {
                            dataName <- gsub("\\)", "", gsub(".+\\(", "", dataName))
                            dname <- gsub(" \\(.+", "", dname)
                        }
                    }

                    tmp.env <- new.env()
                    data(
                        list = dataName,
                        package = pkgname,
                        envir = tmp.env
                    )

                    ## Set the name to the title (or Item if title missing)
                    data <- tmp.env[[dname]]
                    GUI$rhistory$add(
                        sprintf(
                            "## Load example data set\ndata(%s, package = '%s')",
                            dname,
                            pkgname
                        ),
                        keep = TRUE
                    )

                    attr(data, "name") <- sprintf("%s_ex", dname)
                    attr(data, "code") <- sprintf(".dataset <- %s", dname)

                    GUI$setDocument(iNZDocument$new(data = data), reset = TRUE)

                    ## clean up
                    rm("tmp.env")
                    dispose(importFileWin)
                }
            )


            visible(importFileWin) <<- TRUE
            invisible(.self)
        } # initialize
    ) # methods list
) # setRefClass
