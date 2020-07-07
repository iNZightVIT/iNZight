options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("RGtk2")

gtk_path <- file.path(system.file(package = "RGtk2"), "gtk", .Platform$r_arch)
if (! dir.exists( gtk_path ) ) {
    dir.create(gtk_path, recursive = TRUE)

    url <- "http://ftp.gnome.org/pub/gnome/binaries/win64/gtk+/2.22/gtk+-bundle_2.22.1-20101229_win64.zip"
    path <- file.path(tempdir(), basename(sub("\\?.*", "", url)))
    download.file(url, path, mode = "wb")

    unzip(path, exdir = gtk_path)
    RGtk2:::.configure_gtk_theme("MS-Windows")

    unlink(path)
}
