# R script
github_deps <- c(
    "iNZightVIT/gWidgets2RGtk2@inz",
    "iNZightVIT/iNZightTools@release/1.9",
    "iNZightVIT/iNZightTS@dev",
    "iNZightVIT/iNZightMR@dev",
    "iNZightVIT/iNZightPlots@release/2.12",
    "iNZightVIT/iNZightModules@dev"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))

list.files(system.file("gtk", package = "RGtk2"))

if (.Platform$OS.type == "windows") {
    utils::install.packages("XML")
    if (!requireNamespace("XML")) {
        download.file(
            sprintf(
                "https://cran.r-project.org/bin/windows/contrib/%s/XML_3.99-0.3.zip",
                paste(strsplit(as.character(getRversion()), "\\.")[[1]][1:2], collapse = ".")
            ),
            "xml.zip"
        )
        unzip("xml.zip", exdir = .libPaths()[1])
        unlink("xml.zip")
    }

    sessionInfo()
}

remotes::install_github(github_deps)
remotes::install_deps(dependencies = TRUE)
remotes::install_cran("rcmdcheck")
