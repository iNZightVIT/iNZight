if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}

if (!dir.exists(system.file("gtk", package = "RGtk2"))) {
    cat("Installing gtk ...")
    gtk_url <- "https://inzight.nz/data/gtk+-bundle_2.22.1-20101229_win64.zip"

    # Downloading gtk
    download.file(gtk_url, destfile = "gtk.zip")
    dir.create("gtk")
    unzip("gtk.zip", exdir = "gtk")
    file.remove("gtk.zip")
}

Sys.setenv(GTK_PATH = file.path(getwd(), "gtk"))

RGtk2_LATEST <- numeric_version("2.20.41")
if (!requireNamespace("RGtk2", quietly = TRUE) ||
    packageVersion("RGtk2") < RGtk2_LATEST) {
    remotes::install_github("tmelliott/RGtk2/RGtk2", type = "source")
}

if (!requireNamespace("cairoDevice", quietly = TRUE)) {
    remotes::install_github("tmelliott/cairoDevice", type = "source")
}
