library(gWidgets2)
library(gWidgets2RGtk2)


show_icons <- function() {
    all_icons <- names(getStockIcons())

    w <- gwindow(width = 200, height = 800)
    g <- gvbox(use.scrollwindow = TRUE, expand = TRUE, container = w)

    for (icon in gsub("gtk-", "", all_icons, fixed = TRUE)) {
        b <- gbutton(icon, container = g)
    }
}
