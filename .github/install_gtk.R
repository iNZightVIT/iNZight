# if windows
if (.Platform$OS.type == "windows" &&
    !dir.exists(system.file("gtk", package = "RGtk2"))) {
    cat("Installing gtk ...")
    gtk_url <- "https://inzight.nz/data/gtk+-bundle_2.22.1-20101229_win64.zip"

    # Downloading gtk
    download.file(gtk_url, destfile = "gtk.zip")
    dir.create("gtk")
    unzip("gtk.zip", exdir = "gtk")
    file.remove("gtk.zip")
}

Sys.setenv(GTK_PATH = file.path(getwd(), "gtk"))

if (!requireNamespace("remotes", quietly = TRUE)) {
    cat("Installing remotes ...\n")
    install.packages("remotes")
}
if (!requireNamespace("RGtk2", quietly = TRUE) ||
    packageVersion("RGtk2") < numeric_version("2.20.41")) {
    cat("Installing RGtk2 ...\n")
    remotes::install_github("tmelliott/RGtk2/RGtk2", type = "source", build = FALSE)
}
if (!requireNamespace("cairoDevice", quietly = TRUE)) {
    cat("Installing cairoDevice ...\n")
    install.packages("cairoDevice", type = "source", build = FALSE)
}

if (.Platform$OS.type == "windows" &&
    !dir.exists(system.file("gtk", package = "RGtk2"))) {
    cat("Moving gtk binary to RGtk2 ...\n")
    file.rename("gtk", system.file("gtk", package = "RGtk2"))
}
