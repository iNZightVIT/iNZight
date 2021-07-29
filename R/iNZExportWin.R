iNZExportWin <- setRefClass(
    "iNZExportWin",
    fields = list(
        file = "character"
    ),
    contains = "iNZWindow",
    methods = list(
        initialize = function(gui) {
            callSuper(gui,
                title = "Export Data",
                width = "small",
                height = "small",
                ok = "Export",
                action = .self$export_data
            )

            tbl <- glayout(expand = TRUE, fill = TRUE)
            ii <- 1L

            lbl <- glabel("Export to :")
            font(lbl) <- list(weight = "bold")
            fchoose <- gfilebrowse(
                type = "save"
            )
            tbl[ii, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl[ii, 2:3, expand = TRUE] <- fchoose
            ii <- ii + 1L

            lbl <- glabel("Export format :")
            font(lbl) <- list(weight = "bold")
            ftypes <- gcombobox(
                c("CSV", "Excel")
            )
            tbl[ii, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl[ii, 2:3, expand = TRUE] <- ftypes
            ii <- ii + 1L


            add_body(tbl)

            invisible(NULL)
        },
        export_data = function() {
            TRUE
        }
    )
)
