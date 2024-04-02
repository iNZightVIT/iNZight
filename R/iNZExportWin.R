iNZExportWin <- setRefClass(
    "iNZExportWin",
    fields = list(
        file = "ANY",
        filetypes = "list",
        ftype = "ANY"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui) {
            callSuper(gui,
                title = "Export Data",
                width = "med",
                height = "small",
                ok = "Export",
                action = .self$export_data
            )
            on.exit(.self$show())

            tbl <- glayout(expand = TRUE, fill = TRUE)
            ii <- 1L

            lbl <- glabel("Export to :")
            font(lbl) <- list(weight = "bold")
            file <<- gfilebrowse(
                type = "save",
                handler = function(h, ...) detect_extension()
            )
            tbl[ii, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl[ii, 2:3, expand = TRUE] <- file
            ii <- ii + 1L

            lbl <- glabel("Export format :")
            font(lbl) <- list(weight = "bold")
            filetypes <<- list(
                csv = "Comma Separated Values (.csv)",
                txt = "Text File (.txt)",
                rds = "Serialised R object (.rds)"
            )
            ftype <<- gcombobox(unlist(filetypes))
            tbl[ii, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl[ii, 2:3, expand = TRUE] <- ftype
            ii <- ii + 1L

            add_body(tbl)

            invisible(NULL)
        },
        detect_extension = function() {
            f <- svalue(file)
            if (length(f) == 0 || f == "") {
                return()
            }

            ext <- tools::file_ext(f)
            if (!ext %in% names(filetypes)) {
                return()
            }

            svalue(ftype, index = TRUE) <<- which(names(filetypes) == ext)
        },
        export_data = function() {
            f <- svalue(file)
            if (length(f) == 0) {
                gmessage("Please specify a file name.", parent = GUI$modWin)
                return(FALSE)
            }
            if (file.exists(svalue(file))) {
                c <- gconfirm("The file exists. Overwrite?",
                    title = "Overwrite file?",
                    icon = "warning",
                    parent = GUI$modWin
                )
                if (!c) {
                    return(FALSE)
                }
            }

            # this will be moved to iNZightTools shortly...
            fn <- names(filetypes)[svalue(ftype, index = TRUE)]

            x <- try(
                switch(fn,
                    "csv" = {
                        readr::write_csv(GUI$getActiveData(lazy = FALSE), f)
                    },
                    "txt" = {
                        readr::write_delim(GUI$getActiveData(lazy = FALSE), f)
                    },
                    "rds" = {
                        saveRDS(GUI$getActiveData(lazy = FALSE), f)
                    }
                ),
                silent = TRUE
            )

            if (inherits(x, "try-error")) {
                gmessage(
                    paste0(
                        "Failed to export data.\n\n",
                        attr(x, "condition")$message
                    ),
                    parent = GUI$modWin,
                    icon = "error"
                )
                return(FALSE)
            }
            TRUE
        }
    )
)
