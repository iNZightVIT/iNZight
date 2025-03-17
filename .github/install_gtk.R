# if windows
if (.Platform$OS.type == "windows") {
    cat("Downloading gtk ...\n")
    gtk_url <- "https://inzight.nz/data/gtk+-bundle_2.22.1-20101229_win64.zip"

    # Downloading gtk
    download.file(gtk_url, destfile = "gtk.zip")
    dir.create("gtk")
    unzip("gtk.zip", exdir = "gtk")
    file.remove("gtk.zip")

    on.exit({
        # delete if it doesn't get moved
    })
}

Sys.setenv(GTK_PATH = file.path(getwd(), "gtk"))

if (!requireNamespace("remotes", quietly = TRUE)) {
    cat("Installing remotes ...\n")
    install.packages("remotes")
}
if (!requireNamespace("RGtk2", quietly = TRUE)) {
    cat("Installing RGtk2 ...\n")
    if (.Platform$OS.type == "windows") {
        remotes::install_cran("RGtk2",
            INSTALL_opts = "--no-test-load"
        )
    } else {
        remotes::install_github("tmelliott/RGtk2/RGtk2",
            type = "source",
            build = FALSE,
            # R session needs to be reloaded before we can test RGtk2
            INSTALL_opts = c("--no-test-load")
        )
    }
}
if (!requireNamespace("cairoDevice", quietly = TRUE)) {
    cat("Installing cairoDevice ...\n")
    install.packages("cairoDevice",
        type = "source",
        build = FALSE
    )
}

if (.Platform$OS.type == "windows") {
    if (!file.exists(file.path(system.file("", package = "RGtk2"), "gtk"))) {
        cat("Moving gtk binary to RGtk2 ...\n")
        file.rename("gtk", file.path(system.file("", package = "RGtk2"), "gtk"))
    } else {
        unlink("gtk", recursive = TRUE)
    }
}
