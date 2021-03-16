# R script
github_deps <- c(
    "iNZightVIT/gWidgets2RGtk2@inz",
    "iNZightVIT/iNZightTools@dev",
    "iNZightVIT/iNZightTS@1.5.4",
    "iNZightVIT/iNZightMR@2.2.5",
    "iNZightVIT/iNZightPlots@2.12",
    "iNZightVIT/iNZightRegression@1.2.8",
    "iNZightVIT/iNZightModules@2.5.3"
)

OS <- Sys.getenv("OS_TYPE")
options(
    repos = c(
        if (OS == "Linux") RSPM = Sys.getenv("RSPM"),
        CRAN = "https://cloud.r-project.org"
    ),
    install.packages.compile.from.source = "never"
)

if (OS == "Windows" && getRversion() < numeric_version("4")) {
    install.packages("RODBC", type = "binary")
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
