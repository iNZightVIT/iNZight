#' widget displaying a group of tags
#'
#' @importClassesFrom gWidgets2RGtk2 GWidget
GTags <- setRefClass("GTags",
    contains = "GGroup",
    fields = list(
        placeholder = "ANY",
        only_unique = "logical"
    ),
    methods = list(
        initialize = function(toolkit = NULL, items = NULL, placeholder = NULL,
                              only_unique = TRUE,
                              handler, action, container, ...) {

            initFields(
                only_unique = only_unique
            )
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
            invisible(TRUE)
        },
        toggle_placeholder = function() {
            visible(placeholder) <<- !is.null(placeholder) && length(children) == 1L
        }
    )
)

#' A custom class drawing a label with a background border
gtags <- function(items, placeholder = NULL, only_unique = TRUE, handler = NULL, action = NULL, container = NULL, ...) {
    toolkit <- gWidgets2::guiToolkit()

    if (missing(items)) items <- NULL
    obj <- GTags$new(toolkit, items, placeholder, only_unique, handler, action, container, ...)

    gWidgets2:::check_return_class(obj, "GTags")
    obj
}


# need to import this:
TARGET.TYPE.TEXT   <- gWidgets2RGtk2:::TARGET.TYPE.TEXT
TARGET.TYPE.OBJECT <- gWidgets2RGtk2:::TARGET.TYPE.OBJECT
widgetTargetTypes <- gWidgets2RGtk2:::widgetTargetTypes

GTag <- setRefClass("GTag",
    contains = "GWidget",
    methods = list(
        initialize = function(toolkit = NULL, text = NULL, handler, action, container, ...) {

            widget <<- gtkLabel(text)
            block <<- gtkEventBox()
            block$add(widget)
            toolkit <<- toolkit

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

#' A custom class drawing a label with a background border
gtag <- function(text, handler = NULL, action = NULL, container = NULL, ...) {
    toolkit <- gWidgets2::guiToolkit()

    obj <- GTag$new(toolkit, text, handler, action, container, ...)

    gWidgets2:::check_return_class(obj, "GTag")
    obj
}
