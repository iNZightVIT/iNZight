iNZClipboard <- setRefClass(
    "iNZClipboard",
    fields = list(
        GUI = "ANY",
        textIn = "ANY",
        delimiter = "character",
        data = "ANY",
        dataOut = "ANY",
        okBtn = "ANY"
    ),
    methods = list(
        initialize = function(gui, type = c("paste", "copy")) {
            initFields(GUI = gui,
                delimiter = "\t"
            )

            w <- gwindow("Copy/paste data",
                visible = FALSE,
                parent = GUI$win,
                width = 600
            )
            g <- gvbox(container = w)

            textIn <<- gtext("",
                height = 500,
                wrap = FALSE,
                font.attr = list(family = "monospace"),
                container = g,
                expand = TRUE,
                handler = function(h, ...) pasteHandler()
            )

            dataOut <<- gdf(
                data.frame(`Paste data above` = NA),
                container = g
            )

            okBtn <<- gbutton("Load",
                container = g,
                handler = function(h, ...) {
                    GUI$setDocument(
                        iNZDocument$new(data = data),
                        reset = TRUE
                    )
                    dispose(w)
                }
            )

            visible(w) <- TRUE
        },
        pasteHandler = function() {
            data <<- try(
                iNZightTools::read_text(
                    svalue(textIn),
                    delim = delimiter
                ),
                silent = TRUE
            )
            if (!inherits(data, "try-error")) {
                dataOut$set_items(data)
            }
        }
    )
)
