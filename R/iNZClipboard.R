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
            initFields(
                GUI = gui,
                delimiter = "\t"
            )

            w <- gwindow("Copy/paste data",
                visible = FALSE,
                parent = GUI$win,
                width = 600
            )
            g <- gvbox(container = w)

            lbl <- glabel("  Copy data from Excel/Google Sheets/etc and paste below:",
                container = g,
                anchor = c(-1, 0)
            )
            font(lbl) <- list(weight = "bold")

            textIn <<- gtext("",
                height = 500,
                wrap = FALSE,
                font.attr = list(family = "monospace"),
                container = g,
                expand = TRUE,
                handler = function(h, ...) parseData()
            )

            dataOut <<- gdf(
                data.frame("Preview" = "Paste data above", stringsAsFactors = TRUE),
                container = g
            )

            btnGrp <- ggroup()

            lbl <- glabel("  Choose delimiter (or type one) :", container = btnGrp)
            delim_values <- c("tab (\t)", "comma (,)", "semicolon (;)")
            delimBox <- gcombobox(
                delim_values,
                editable = TRUE,
                handler = function(h, ...) {
                    d <- svalue(h$obj)
                    if (d %in% delim_values) {
                        d <- gsub("^.+\\(|\\)$", "", d)
                    }
                    delimiter <<- d
                    parseData()
                },
                container = btnGrp
            )

            addSpring(btnGrp)

            cancelBtn <- gbutton("Cancel",
                container = btnGrp,
                handler = function(h, ...) dispose(w)
            )

            okBtn <<- gbutton("Load",
                container = btnGrp,
                handler = function(h, ...) {
                    GUI$setDocument(
                        iNZDocument$new(data = data, preferences = GUI$preferences),
                        reset = TRUE
                    )
                    dispose(w)
                }
            )

            add(g, btnGrp)

            visible(w) <- TRUE
        },
        parseData = function() {
            txt <- svalue(textIn)

            if (txt == "") {
                dataOut$set_items(
                    data.frame("Preview" = "Paste data above", stringsAsFactors = TRUE)
                )
            }

            if (length(txt) == 1) {
                txt <- sprintf("%s\n", txt)
            }

            data <<- try(
                iNZightTools::read_text(
                    txt,
                    delim = delimiter
                ),
                silent = TRUE
            )
            if (!inherits(data, "try-error")) {
                dataOut$set_items(data)
            } else {
                dataOut$set_items(
                    data.frame(
                        Error = "Cannot parse data. Try specifying delimiter below.",
                        stringsAsFactors = TRUE
                    )
                )
            }
        }
    )
)

iNZCopyToClipboard <- setRefClass(
    "iNZCopyToClipboard",
    contains = "iNZWindow",
    fields = list(
        text_data = "ANY",
        delimiter = "character"
    ),
    methods = list(
        initialize = function(gui) {
            callSuper(gui,
                title = "Copy to Clipboard",
                width = "med",
                height = "med",
                ok = "Close",
                cancel = NULL,
                action = close
            )
            on.exit(.self$show())

            initFields(delimiter = "\t")

            g <- gvbox()
            add_body(g)

            lbl <- glabel("  Copy the data below to paste elsewhere:",
                container = g,
                anchor = c(-1, 0)
            )
            font(lbl) <- list(weight = "bold")

            text_data <<- gtext("",
                height = 500,
                wrap = FALSE,
                expand = TRUE
            )
            text_data$set_editable(FALSE)
            # addHandlerFocus(text_data,
            #     handler = function(h, ...) {
            #         RGtk2::gtkTextBufferSelectRange(
            #             text_data$buffer,
            #             text_data$buffer$GetStartIter()$iter,
            #             text_data$buffer$GetEndIter()$iter
            #         )
            #     }
            # )

            add_body(text_data, fill = TRUE, expand = TRUE)

            add_body(
                glabel("Use CTRL+A to select the data, and CTRL+C to copy it.")
            )

            setTextData()

            invisible(NULL)
        },
        setTextData = function() {
            text <- paste(
                apply(GUI$getActiveData(), 1L, paste, collapse = delimiter),
                collapse = "\n"
            )

            svalue(text_data) <<- ""
            text_data$insert_text(
                text,
                where = "beginning",
                font.attr = list(family = "monospace"),
                do.newline = FALSE
            )
        }
    )
)
