#' iNZight Image Button
#'
#' Creates an image that works as a button, and has mouseover icon effects
#' and a 'tooltip' that appears in the status bar.
#'
#' @param stock.id the icon to use
#' @param filename the image location
#' @param old_cursor the cursor before mouseover
#' @param tooltip a list with items `tooltip` and `widget`
#' @param ... additional arguments passed to `gimage`
#' @return a gimage with extra effects
#' @author Tom Elliott
#' @export
#' @examples
#' w <- gwindow()
#' g <- ggroup(container = w)
#' gib <- gimagebutton("close", container = g)
#' dispose(w)
gimagebutton <- function(stock.id = NULL, filename, old_cursor = NULL, tooltip = NULL,
                         ...) {
    img <- if (is.null(stock.id)) {
        gimage(filename = filename, ...)
    } else {
        gimage(stock.id = stock.id, ...)
    }

    tooltip(img) <- tooltip

    hover <- gdkCursorNew("GDK_HAND1")
    addHandler(img, "enter-notify-event",
        handler = function(h, ...) {
            getToolkitWidget(h$obj)$getWindow()$setCursor(hover)
            TRUE
        }
    )
    addHandler(img, "leave-notify-event",
        handler = function(h, ...) {
            getToolkitWidget(h$obj)$getWindow()$setCursor(old_cursor)
            TRUE
        }
    )
    img
}

# #' Large graphic button
# #'
# #' Creates a button with text above a large graphic.
# #'
# #' @param text The text label for the button
# #' @param filename The image location
# #' @param old_cursor The cursor before mouseover
# #' @param tooltip A list with items `tooltip` and `widget`
# #' @param handler A function to run when the button is clicked
# #' @param container The container to add the button to
# #' @param expand Whether to expand the button to fill the container
# #' @param fill Whether to fill the container with the button
# #' @param font The font to use for the text label
# #' @param size The size of the image
# #' @param ... Additional arguments passed to `gimage`
# #' @return A ggroup with a glabel and gimage
# ggraphicbutton <- function(text, filename,
#                            old_cursor = NULL,
#                            tooltip = NULL,
#                            handler = NULL,
#                            container = NULL,
#                            expand = FALSE,
#                            fill = FALSE,
#                            font = list(weight = "bold", size = 12),
#                            size = "large",
#                            ...) {
#     btn <- gvbox(container = container, expand = expand, fill = fill)

#     lbl <- glabel(text, container = btn)
#     font(lbl) <- font

#     image <- gimage(
#         filename = filename,
#         size = size,
#         container = btn,
#         ...
#     )

#     hover <- gdkCursorNew("GDK_HAND1")
#     gSignalConnect(
#         btn$widget, "enter-notify-event",
#         function(...) {
#             btn$widget$getWindow()$setCursor(hover)
#             TRUE
#         }
#     )
#     gSignalConnect(
#         btn$widget, "leave-notify-event",
#         function(...) {
#             btn$widget$getWindow()$setCursor(old_cursor)
#             TRUE
#         }
#     )
#     # addHandler(btn$widget, "enter-notify-event",
#     #     handler = function(h, ...) {
#     #         getToolkitWidget(h$obj)$getWindow()$setCursor(hover)
#     #         TRUE
#     #     }
#     # )
#     # addHandler(btn$block, "leave-notify-event",
#     #     handler = function(h, ...) {
#     #         getToolkitWidget(h$obj)$getWindow()$setCursor(old_cursor)
#     #         TRUE
#     #     }
#     # )

#     invisible(btn)
# }

# nothing <- function() {
#     devtools::load_all()
#     try(dispose(w))
#     w <- gwindow()
#     g <- ggroup(container = w)
#     x <- ggraphicbutton("Some image", "inst/images/pivot_longer.png",
#         container = g
#     )
# }
