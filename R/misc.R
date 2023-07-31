# some imports from gWidgets2
check_return_class <- function(obj, ret_class) {
    if (!any(sapply(ret_class, is, object = obj))) {
        stop(
            sprintf(
                "Expecting toolkit object of class (or subclass) %s. Got one of class %s",
                paste(ret_class, collapse = "; "),
                class(obj)[1]
            )
        )
    }
}

# Copied from 'gWidgets2RGtk2' by J Verzani
TARGET.TYPE.TEXT <- 80L
TARGET.TYPE.OBJECT <- 81L
widgetTargetTypes <- list(
    text = gtkTargetEntry("text/plain", 0, TARGET.TYPE.TEXT),
    object = gtkTargetEntry("text/plain", 0, TARGET.TYPE.OBJECT)
)

GWidget <- gWidgets2RGtk2:::GWidget
