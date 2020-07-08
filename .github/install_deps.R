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

if (.Platform$OS.type == "windows") {
    if (!requireNamespace("XML", quietly = TRUE))
        utils::install.packages("XML", type = "binary")

    if (!requireNamespace("cairoDevice", quietly = TRUE))
        utils::install.packages("cairoDevice", type = "binary")

    if (!requireNamespace("RODBC", quietly = TRUE))
        utils::install.packages("RODBC", type = "binary")
}

remotes::install_github(
    github_deps,
    INSTALL_opts = "--no-multiarch"
)
remotes::install_deps(
    dependencies = TRUE,
    INSTALL_opts = "--no-multiarch"
)
remotes::install_cran("rcmdcheck")
