# basic wrapper for gWidgets2::gbutton that adds cursor pointer on hover
gbutton <- function(..., old_cursor = NULL) {
    b <- gWidgets2::gbutton(...)
    hover <- gdkCursorNew("GDK_HAND1")
    addHandler(b, "enter-notify-event",
        handler = function(h, ...) {
            getToolkitWidget(h$obj)$getWindow()$setCursor(hover)
            TRUE
        }
    )
    addHandler(b, "leave-notify-event",
        handler = function(h, ...) {
            getToolkitWidget(h$obj)$getWindow()$setCursor(old_cursor)
            TRUE
        }
    )
    b
}
