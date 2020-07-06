options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("RGtk2")

gtkpath <- file.path(.libPaths()[1], "RGtk2")
if (! dir.exists(gtkpath)) stop("No directory ...")

gtkdir <- file.path(gtkpath, "gtk")
if (! dir.exists( gtkdir ) ) {
    dir.create(gtkdir)
    url <- "http://ftp.gnome.org/pub/gnome/binaries/win64/gtk+/2.22/gtk+-bundle_2.22.1-20101229_win64.zip"
    download.file(url, "gtk.zip")
    system("7z l gtk.zip")
    system(sprintf("7z x gtk.zip -o%s", gtkdir))
    unlink("gtk.zip")
}

print(list.files(gtkpath))
print(list.files(gtkdir))
cat("Loading RGtk2 library...\n")
library(RGtk2)
cat(".done.\n")

