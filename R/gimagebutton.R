gimagebutton <- function(stock.id = NULL, filename, old_cursor = NULL, ...) {
    img <- if (is.null(stock.id)) gimage(filename = filename, ...) else gimage(stock.id = stock.id, ...)
    
    hover <- gdkCursorNew("GDK_HAND1")
    addHandler(img, "enter-notify-event", handler=function(h,...) {
                   getToolkitWidget(h$obj)$getWindow()$setCursor(hover)
                   TRUE
               })
    addHandler(img, "leave-notify-event", handler=function(h,...) {
                   getToolkitWidget(h$obj)$getWindow()$setCursor(old_cursor)
                   TRUE
               })
    img
}
