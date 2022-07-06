GMultiLabel <- setRefClass(
    "GMultiLabel",
    contains = "GWidgetWithItems",
    fields = list(
        change_handler = "ANY",
        placeholder = "character",
        remove_on_click = "logical",
        only_unique = "logical"
    ),
    methods = list(
        initialize = function(toolkit, items, placeholder = "",
                              removeOnClick = FALSE,
                              only_unique = TRUE,
                              horizontal,
                              handler, action, container, ...) {

            widget <<- if (horizontal) gtkHBox() else gtkVBox()
            block <<- gtkScrolledWindowNew()
            block$setPolicy("GTK_POLICY_AUTOMATIC", "GTK_POLICY_NEVER")

            block$addWithViewport(widget)
            gtkContainerSetBorderWidth(widget, 5)

            block$setSizeRequest(-1, 50)

            initFields(
                widgets = list(),
                change_signal = "clicked",
                change_handler = NULL,
                placeholder = placeholder,
                remove_on_click = removeOnClick,
                only_unique = only_unique
            )

            set_items(value = items)
            add_to_parent(container, .self, ...)

            add_handler_changed(handler)

            callSuper(toolkit)
        },
        get_value = function(drop = TRUE, ...) {
            get_items()
        },
        set_value = function(value, drop = TRUE, ...) {
            set_items(value)
        },
        # these don't do anything...
        get_index = function(...) TRUE,
        set_index = function(value, ...) invisible(),
        get_items = function(...) {
            items <- sapply(widgets, function(x) x$get_value())
            stats::setNames(items, NULL)
        },
        set_items = function(value, ...) {
            if (only_unique) value <- unique(value)
            widgets <<- sapply(value, gbutton)
            sapply(widget$getChildren(), gtkContainerRemove, object = widget)
            sapply(widgets, function(x) {
                font(x) <- list(size = 8)
                gtkBoxPackStart(x$block,
                    object = widget,
                    expand = FALSE,
                    padding = 5
                )
            })

            if (length(widgets) == 0L) {
                lbl <- gtkLabel(placeholder)
                gtkBoxPackStart(lbl,
                    object = widget,
                    expand = FALSE,
                    padding = 5
                )
            }

            # add click signal handler
            if (remove_on_click) {
                sapply(widgets, addHandlerClicked,
                    handler = function(h, ...) {
                        .self$drop_item(h$obj$get_value())
                    }
                )
            }

            # then update action ...
            update_handler()
            invisible()
        },
        add_item = function(value, ...) {
            items <- c(get_items(), value)
            set_items(items)
        },
        drop_item = function(value, ...) {
            if (is.character(value))
                index <- which(sapply(get_items(), function(x) x == value))
            else index <- as.integer(value)
            if (length(index) == 0L) return (invisible(FALSE))

            if (index == 0L || index > length(widgets)) return(invisible(FALSE))

            items <- sapply(widgets, function(x) x$get_value())[-index]
            set_items(items)

            invisible(TRUE)
        },
        clear = function() {
            set_items(NULL)
        },
        get_length = function() length(widgets),
        # hack a change handler ...
        add_handler_changed = function(handler, ...) {
            change_handler <<- handler
        },
        update_handler = function() {
            if (is.null(change_handler)) return(invisible())
            .self$change_handler(list(obj = .self))
        },
        handler_widget = function() widget
    )
)

gmultilabel <- function(items, placeholder = "", removeOnClick = FALSE,
                        only_unique = TRUE,
                        horizontal = TRUE, handler = NULL, action = NULL,
                        container = NULL, ...) {
    toolkit <- gWidgets2::guiToolkit()

    if (missing(items)) items <- NULL
    obj <- GMultiLabel$new(toolkit, items, placeholder,
        removeOnClick, only_unique,
        horizontal, handler, action, container, ...)

    check_return_class(obj, "GMultiLabel")
    obj
}

# widget displaying a group of tags
GTags <- setRefClass("GTags",
    contains = "GGroup",
    fields = list(
        placeholder = "ANY",
        only_unique = "logical",
        init = "logical"
    ),
    methods = list(
        initialize = function(toolkit = NULL,
                              items = NULL,
                              placeholder = NULL,
                              only_unique = TRUE,
                              handler = NULL,
                              action = NULL,
                              container = NULL,
                              ...) {

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

            handler_id <<- add_handler_changed(handler, action)

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

gtkTagNew <- function(str) {
    lbl <- gtkLabel(str)
    widget <- gtkEventBox()
    widget$add(lbl)
    widget$modifyFg(
        GtkStateType["normal"],
        "black"
    )
    widget$modifyBg(
        GtkStateType["normal"],
        "gray90"
    )
    lbl$setPadding(6, 3)
    widget
}

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
            block <<- gtkButton()
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

            initFields(
                change_signal = "clicked"
            )

            add_to_parent(container, .self, ...)

            callSuper(toolkit)
        },
        get_value = function(index = TRUE, drop = TRUE, ...) {
            widget$getLabel()
        },
        handler_widget = function() widget
    )
)

# A custom class drawing a label with a background border
gtag <- function(text, handler = NULL, action = NULL, container = NULL, ...) {
    toolkit <- gWidgets2::guiToolkit()

    obj <- GTag$new(toolkit, text, handler, action, container, ...)

    check_return_class(obj, "GTag")
    obj
}
