options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("RGtk2")

gtkpath <- file.path(.libPaths()[1], "RGtk2")
if (! dir.exists(gtkpath)) stop("No directory ...")

gtkdir <- file.path(gtkpath, "gtk")
if (! dir.exists( gtkdir ) ) {
    dir.create(gtkdir)
    cd <- setwd(gtkdir)
    url <- "http://ftp.gnome.org/pub/gnome/binaries/win64/gtk+/2.22/gtk+-bundle_2.22.1-20101229_win64.zip"
    download.file(url, "gtk.zip")
    system("7z x gtk.zip > nul")
    unlink("gtk.zip")
    setwd(cd)
}

print(list.files(gtkpath))
print(list.files(gtkdir))
