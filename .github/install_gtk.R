options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("RGtk2")

if (! dir.exists( file.path( system.file("RGtk2"), "gtk" ) ) ) {
    url <- "http://ftp.gnome.org/pub/gnome/binaries/win32/gtk+/2.22/gtk+-bundle_2.22.1-20101229_win64.zip"
    download.file(url, "gtk.zip")
    unzip("gtk.zip", exdir = system.file("RGtk2"))
    unlink("gtk.zip")
}
