if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
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
