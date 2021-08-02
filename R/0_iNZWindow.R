iNZWindow <- setRefClass("iNZWindow",
    fields = list(
        GUI = "ANY",
        # window = "ANY",
        header = "ANY",
        body = "ANY",
        footer = "ANY",
        ok_handler = "function", # should return TRUE, or FALSE if action failed
        help_url = "ANY",
        help_button = "ANY",
        ok_button = "ANY",
        cancel_button = "ANY",
        code_panel = "ANY",
        code_font = "list"
    ),
    methods = list(
        initialize = function(
            gui,
            title = "Window title",
            width = c("small", "med", "large"),
            height = c("small", "med", "large"),
            ok = "OK",
            cancel = "Cancel",
            action,
            help = NULL,
            scroll = FALSE,
            show_code = FALSE
        ) {
            if (is.null(gui)) return(invisibl(FALSE))
            if (!is.function(action)) stop("action should be a function")

            initFields(
                GUI = gui,
                ok_handler = action,
                help_url = help,
                code_panel = NULL,
                code_font = list(size = 8, family = "monospace")
            )
            show_code <- show_code && GUI$preferences$show.code

            if (is.character(width)) {
                width <- match.arg(width)
                width <- switch(width,
                    "small" = 380L,
                    "med" = 680L,
                    "large" = 1000L
                )
            }
            if (is.character(height)) {
                height <- match.arg(height)
                height <- switch(height,
                    "small" = 200L,
                    "med" = 360L,
                    "large" = 650L
                )
            }

            try(dispose(GUI$modWin), silent = TRUE)
            GUI$modWin <<- gwindow(
                title = title,
                visible = FALSE,
                width = width,
                height = height + show_code * 80L,
                parent = GUI$win
            )

            if (scroll) {
                # add scrollbars
            }

            g <- gvbox(container = GUI$modWin)
            g$set_borderwidth(10L)

            header <<- gvbox(container = g)
            body <<- gvbox(container = g)
            addSpace(g, 10L)
            addSpring(g)
            footer <<- ggroup(container = g)

            if (!is.null(help_url)) {
                help_button <<- gbutton("Help",
                    container = footer,
                    handler = function(h, ...)
                        help_page(help_url)
                )
                size(help_button) <<- c(120, -1)
            }

            addSpring(footer)
            cancel_button <<- gbutton(cancel,
                handler = function(h, ...) close()
            )
            size(cancel_button) <<- c(120, -1)

            ok_button <<- gbutton(ok,
                handler = function(h, ...) {
                    res <- ok_handler()
                    if (isTRUE(res)) close()
                }
            )
            size(ok_button) <<- c(120, -1)

            switch(GUI$OS,
                "windows" = {
                    add(footer, ok_button)
                    add(footer, cancel_button)
                },
                {
                    add(footer, cancel_button)
                    add(footer, ok_button)
                }
            )

            # Code panel under window
            if (show_code) {
                add(g, gseparator())
                g_code <- ggroup(container = g)
                code_panel <<- gtext(
                    "# R code will show here",
                    container = g_code,
                    font.attr = code_font,
                    fill = TRUE,
                    expand = TRUE
                )
                enabled(code_panel) <<- FALSE
            }

            invisible(TRUE)
        },
        add_heading = function(
            text,
            size = 10L,
            weight = "normal",
            align = c("left", "center", "right")
        ) {
            lbl <- glabel(text)
            font(lbl) <- list(size = size, weight = weight)
            anchor <- switch(
                match.arg(align),
                "left" = c(-1, 0),
                "center" = c(0, 0),
                "right" = c(1, 0)
            )
            add(header, lbl, anchor = anchor)

            invisible(NULL)
        },
        add_body = function(x, ...) add(body, x, ...),
        set_code = function(code) {
            if (is.null(code_panel)) return()
            svalue(code_panel) <<- ""
            font(code_panel) <<- code_font
            insert(code_panel, code, do.newline = FALSE)
        },
        show = function() {
            visible(GUI$modWin) <<- TRUE
        },
        close = function() {
            dispose(GUI$modWin)
        }
    )
)
