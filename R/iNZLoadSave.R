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

                ## select a file name:
                filename <<- gfile("Select file",
                                   type = switch(action,
                                                 "load" = "open",
                                                 "save" = "save"),
                                   filter = list("Readable Files" = list(patterns = "*.rds")),
                                   parent = gui$win)

                ## load/save the file
                if (!is.null(filename))
                    switch(action,
                           "load" = {
                               gui$setDocument(iNZDocument$new(data = readRDS(filename)))
                           },
                           "save" = {
                               if (!grepl(".rds", filename)) filename <<- paste0(filename, ".rds")
                               saveRDS(gui$getActiveData(), filename)
                           })
            }
        )
    )
