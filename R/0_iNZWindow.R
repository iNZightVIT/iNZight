iNZWindow <- setRefClass("iNZWindow",
    fields = list(
        GUI = "ANY",
        # window = "ANY",
        window_width = "numeric",
        window_height = "numeric",
        header = "ANY",
        body = "ANY",
        footer = "ANY",
        toolbar = "ANY",
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
            body_direction = c("vertical", "horizontal"),
            ok = "OK",
            cancel = "Cancel",
            action,
            help = NULL,
            scroll = FALSE,
            show_code = FALSE
        ) {
            if (is.null(gui)) return(invisible(FALSE))
            if (!is.function(action)) stop("action should be a function")

            initFields(
                GUI = gui,
                ok_handler = action,
                help_url = help,
                code_panel = NULL,
                code_font = list(size = 8L, family = "monospace")
            )
            show_code <- show_code && GUI$preferences$show.code

            if (is.character(width)) {
                width <- match.arg(width)
                window_width <<- switch(width,
                    "small" = 380L,
                    "med" = 680L,
                    "large" = 1000L
                )
            } else {
                window_width <<- width
            }

            if (is.character(height)) {
                height <- match.arg(height)
                window_height <<- switch(height,
                    "small" = 200L,
                    "med" = 360L,
                    "large" = 650L
                )
            } else {
                window_height <<- height
            }

            try(dispose(GUI$modWin), silent = TRUE)
            GUI$modWin <<- gwindow(
                title = title,
                visible = FALSE,
                width = window_width,
                height = window_height + show_code * 80L,
                parent = GUI$win
            )
            addHandlerDestroy(GUI$modWin, function(h, ...) closeHandler(h, ...))

            body_direction <- match.arg(body_direction)
            g <- gvbox(expand = TRUE)
            g$set_borderwidth(10L)

            header <<- gvbox(container = g)
            body <<- switch(body_direction,
                "vertical" = gvbox(container = g, expand = TRUE, fill = TRUE),
                "horizontal" = ggroup(container = g, expand = TRUE)
            )
            addSpace(g, 10L)
            # addSpring(g)
            footer <<- ggroup(container = g)

            if (!is.null(help_url)) {
                help_button <<- gbutton("Help",
                    container = footer,
                    handler = function(h, ...)
                        help_page(help_url)
                )
                size(help_button) <<- c(120, -1)
                help_button$set_icon("gw-help_topic")
            }

            toolbar <<- ggroup(container = footer)

            addSpring(footer)
            if (!is.null(cancel)) {
                cancel_button <<- gbutton(cancel,
                    handler = function(h, ...) close()
                )
                size(cancel_button) <<- c(120, -1)
                cancel_button$set_icon("close")
            } else {
                cancel_button <<- NULL
            }

            ok_button <<- gbutton(ok,
                handler = function(h, ...) {
                    res <- ok_handler()
                    if (isTRUE(res)) close()
                }
            )
            size(ok_button) <<- c(120, -1)
            ok_button$set_icon("ok")

            switch(GUI$OS,
                "windows" = {
                    add(footer, ok_button)
                    if (!is.null(cancel_button)) add(footer, cancel_button)
                },
                {
                    if (!is.null(cancel_button)) add(footer, cancel_button)
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

            if (scroll) {
                # add scrollbars
                scrolledWindow <- gtkScrolledWindow()
                scrolledWindow$setPolicy(
                    "GTK_POLICY_NEVER",
                    "GTK_POLICY_AUTOMATIC"
                )
                scrolledWindow$addWithViewport(
                    g$widget
                )
                add(GUI$modWin, scrolledWindow,
                    expand = TRUE, fill = TRUE
                )
            } else {
                add(GUI$modWin, g)
            }

            invisible(TRUE)
        },
        add_heading = function(
            ...,
            size = 10L,
            weight = "normal",
            align = c("left", "center", "right")
        ) {
            # calculate line width
            nc <- floor(10L / size * window_width / 6L)
            lbl <- glabel(
                add_lines(
                    paste(list(...), collapse = " "),
                    nchar = nc
                )
            )
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
        add_lines = function(x, nchar = 100L) {
            stringr::str_wrap(x, nchar)
        },
        add_body = function(x, ...) add(body, x, ...),
        add_toolbar = function(x, ...) add(toolbar, x, ...),
        body_spring = function() addSpring(body),
        body_space = function(x) addSpace(body, x),
        set_code = function(code) {
            if (is.null(code_panel)) return()
            svalue(code_panel) <<- ""
            code_panel$insert_text(
                code,
                where = "beginning",
                font.attr = code_font,
                do.newline = FALSE
            )
        },
        show = function() {
            visible(GUI$modWin) <<- TRUE
        },
        close = function() {
            dispose(GUI$modWin)
        },
        closeHandler = function(h, ...) TRUE
    )
)
