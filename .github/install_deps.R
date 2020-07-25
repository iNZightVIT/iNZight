# R script
github_deps <- c(
    "iNZightVIT/gWidgets2RGtk2@inz",
    "iNZightVIT/iNZightTools@1.9",
    "iNZightVIT/iNZightTS@1.5.4",
    "iNZightVIT/iNZightMR@2.2.5",
    "iNZightVIT/iNZightPlots@2.12",
    "iNZightVIT/iNZightRegression@1.2.8",
    "iNZightVIT/iNZightModules@2.5.3"
)

options(
    repos = c(
        if (RSPM = Sys.getenv("RSPM"),
        CRAN = "https://cloud.r-project.org"
    ),
    install.packages.compile.from.source = "never"
)

if (.Platform$OS.type == "windows" && getRversion() < numeric_version("4")) {
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
