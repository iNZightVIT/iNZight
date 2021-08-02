iNZWindow <- setRefClass("iNZWindow",
    fields = list(
        GUI = "ANY",
        window = "ANY",
        header = "ANY",
        body = "ANY",
        footer = "ANY",
        ok_handler = "function", # should return TRUE, or FALSE if action failed
        help_url = "ANY",
        help_button = "ANY",
        ok_button = "ANY",
        cancel_button = "ANY"
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
            scroll = FALSE
        ) {

            if (!is.function(action)) stop("action should be a function")

            initFields(
                GUI = gui,
                ok_handler = action,
                help_url = help
            )

            width <- match.arg(width)
            height <- match.arg(height)

            if (is.character(width))
                width <- switch(width,
                    "small" = 380L,
                    "med" = 680L,
                    "large" = 1000L
                )
            if (is.character(height))
                height <- switch(height,
                    "small" = 200L,
                    "med" = 360L,
                    "large" = 650L
                )

            window <<- gwindow(
                title = title,
                visible = FALSE,
                width = width,
                height = height,
                parent = GUI$win
            )
            on.exit(visible(window) <<- TRUE)

            if (scroll) {
                # add scrollbars
            }

            g <- gvbox(container = window)
            g$set_borderwidth(10L)

            header <<- gvbox(container = g)
            body <<- gvbox(container = g)
            addSpace(g, 10L)
            addSpring(g)
            footer <<- ggroup(container = g)

            # make place for body content (with padding, etc)
            #

            # create footer with buttons (order consistent with OS):
            # - Windows: [ HELP                    ACTION   CANCEL ]
            # - Unix:    [ HELP                    CANCEL   ACTION ]
            if (!is.null(help_url)) {
                help_button <<- gbutton("Help",
                    container = footer,
                    handler = function(h, ...)
                        help_page(help_url)
                )
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

            invisible(NULL)
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
        close = function() {
            dispose(window)
        }
    )
)
