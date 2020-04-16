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
                    if (d %in% delim_values)
                        d <- gsub("^.+\\(|\\)$", "", d)
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
                        iNZDocument$new(data = data),
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
