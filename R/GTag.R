# widget displaying a group of tags
GTags <- setRefClass("GTags",
    contains = "GGroup",
    fields = list(
        placeholder = "ANY",
        only_unique = "logical",
        init = "logical"
    ),
    methods = list(
        initialize = function(toolkit = NULL, items = NULL, placeholder = NULL,
                              only_unique = TRUE,
                              handler, action, container, ...) {

            initFields(
                only_unique = only_unique,
                change_signal = "button-press-event",
                init = FALSE
            )
            on.exit(init <<- TRUE)
            callSuper(toolkit)

            add_to_parent(container, .self, ...)
            set_borderwidth(5)

            if (!is.null(placeholder)) {
                placeholder <<- glabel(placeholder, container = .self)
                font(placeholder) <<- list(style = "italic")
            } else {
                placeholder <<- NULL
            }

            if (!is.null(items))
                lapply(items, gtag, container = .self)

            toggle_placeholder()
        },
        add_tag = function(text, ...) {
            if (only_unique && has_tag(text)) return(invisible(FALSE))

            gtag(text, container = .self)

            toggle_placeholder()
            invoke_change_handler()
            invisible(TRUE)
        },
        has_tag = function(x) {
            any(sapply(children, function(z) z$get_value() == x))
        },
        drop_tag = function(x, ...) {
            if (is.character(x))
                index <- which(sapply(children, function(z) z$get_value() == x))
            else index <- as.integer(x)
            if (length(index) == 0L) return (invisible(FALSE))

            if (index == 0L || index > length(children)) return(invisible(FALSE))
            tag <- children[[index]]
            remove_child(tag)

            toggle_placeholder()
            invoke_change_handler()
            invisible(TRUE)
        },
        drop_tags = function(x, ...) {
            sapply(.self$get_value(), .self$drop_tag)
            invoke_change_handler()
        },
        set_value = function(value, index = TRUE, drop = TRUE, ...) {
            if (!init) return()
            blockHandlers(.self)
            on.exit(unblockHandlers(.self))
            drop_tags()
            sapply(value, .self$add_tag)
            unblockHandlers(.self)
            invoke_change_handler()
        },
        get_value = function(index = TRUE, drop = TRUE, ...) {
            sapply(children, function(x) x$get_value())
        },
        toggle_placeholder = function() {
            visible(placeholder) <<- !is.null(placeholder) && length(children) == 1L
        },
        handler_widget = function() block, # put on block, not widget
        add_handler_changed = function(handler, action=NULL, ...) {
            add_handler_clicked(handler, action = action, ...)
        },
        add_handler_clicked = function(handler, action=NULL, ...) {
            block$addEvents(GdkEventMask["all-events-mask"])
            add_handler(block,
                "button-press-event",
                event_decorator(handler),
                action
            )
        }
    )
)

# A custom class drawing a label with a background border
gtags <- function(items, placeholder = NULL, only_unique = TRUE, handler = NULL, action = NULL, container = NULL, ...) {
    toolkit <- gWidgets2::guiToolkit()

    if (missing(items)) items <- NULL
    obj <- GTags$new(toolkit, items, placeholder, only_unique, handler, action, container, ...)

    check_return_class(obj, "GTags")
    obj
}

GTag <- setRefClass("GTag",
    contains = "GWidget",
    methods = list(
        initialize = function(toolkit = NULL, text = NULL, handler, action, container, ...) {

            widget <<- gtkLabel(text)
            block <<- gtkEventBox()
            block$add(widget)
            toolkit <<- toolkit

            block$modifyFg(
                GtkStateType["normal"],
                "black"
            )
            block$modifyBg(
                GtkStateType["normal"],
                "gray90"
            )

            widget$setPadding(6, 3)

            initFields()

            add_to_parent(container, .self, ...)

            callSuper(toolkit)
        },
        get_value = function(index = TRUE, drop = TRUE, ...) {
            widget$getLabel()
        }
    )
)

# A custom class drawing a label with a background border
gtag <- function(text, handler = NULL, action = NULL, container = NULL, ...) {
    toolkit <- gWidgets2::guiToolkit()

    obj <- GTag$new(toolkit, text, handler, action, container, ...)

    check_return_class(obj, "GTag")
    obj
}
