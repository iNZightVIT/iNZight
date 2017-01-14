iNZLoadSaveWin <-
    setRefClass(
        "iNZLoadSaveWin",
        fields = list(
            GUI = "ANY",
            action = "ANY",
            fileWin = "ANY",
            filename = "ANY"
        ),
        methods = list(
            initialize = function(gui, action) {
                initFields(GUI = gui, action = action)

                init.d <- file.path("~", "Documents", "iNZightVIT", "Saved Data")

                ## select a file name:
                filename <<- gfile("Select file",
                                   type = switch(action,
                                                 "load" = "open",
                                                 "save" = "save"),
                                   filter = list(
                                       "iNZight Save Files (.rds)" = list(patterns = "*.rds"),
                                       "All Files" = list(patterns = "*")),
                                   parent = gui$win,
                                   initial.dir = path.expand(init.d))

                ## load/save the file
                if (length(filename))
                    switch(action,
                           "load" = {
                               gui$setDocument(iNZDocument$new(data = readRDS(filename)))
                           },
                           "save" = {
                               if (!grepl(".rds", filename)) filename <<- paste0(filename, ".rds")
                               saveRDS(gui$getActiveData(), filename)
                           })

                return(invisible(NULL))
            }
        )
    )
