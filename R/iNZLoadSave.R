iNZLoadSaveWin <-
    setRefClass(
        "iNZLoadSaveWin",
        fields = list(
            GUI = "ANY",
            action = "character",
            fileWin = "ANY",
            filename = "character"
        ),
        methods = list(
            initialize = function(gui, what = c("load", "save"), f = NULL) {
                initFields(GUI = gui, action = match.arg(what))


                ## select a file name:
                if (!is.null(f)) {
                    filename <<- f
                } else {
                    init.d <- file.path(
                        "~", "Documents", "iNZightVIT", "Saved Data"
                    )
                    filename <<- gfile("Select file",
                        type = switch(action,
                            "load" = "open",
                            "save" = "save"
                        ),
                        filter = list(
                            "iNZight Save Files (.rds)" = 
                                list(patterns = "*.rds"),
                            "All Files" = 
                                list(patterns = "*")
                        ),
                        parent = gui$win,
                        initial.dir = path.expand(init.d)
                    )
                }

                ## load/save the file
                if (length(filename))
                    switch(action,
                        "load" = load(),
                        "save" = save()
                    )

                invisible(NULL)
            },
            load = function() {
                d <- readRDS(filename)
                t <- sapply(d, mode)
                for (n in names(t)[t == "character"])
                    d[[n]] <- as.factor(d[[n]])
                GUI$setDocument(
                    iNZDocument$new(data = d)
                )
            },
            save = function() {
                if (!grepl(".rds", filename)) 
                    filename <<- paste0(filename, ".rds")
                saveRDS(GUI$getActiveData(), filename)
            }
        )
    )
