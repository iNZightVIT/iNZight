# R script
github_deps <- c(
    "iNZightVIT/gWidgets2RGtk2@inz",
    "iNZightVIT/iNZightTools@1.9",
    "iNZightVIT/iNZightRegression@1.2.8",
    "iNZightVIT/iNZightTS@1.5.4",
    "iNZightVIT/iNZightMR@2.2.5",
    "iNZightVIT/iNZightPlots@2.12",
    "iNZightVIT/iNZightModules@2.5.3"
)

options(
    repos = c(CRAN = "https://cloud.r-project.org"),
    install.packages.compile.from.source = "never"
)

remotes::install_github(
    github_deps,
    INSTALL_opts = "--no-multiarch"
)
remotes::install_deps(
    dependencies = TRUE,
    INSTALL_opts = "--no-multiarch"
)
remotes::install_cran("rcmdcheck")
