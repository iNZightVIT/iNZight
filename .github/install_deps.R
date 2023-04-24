# R script
github_deps <- c(
    "tmelliott/surveyspec",
    "tmelliott/gWidgets2@patch-1",
    "iNZightVIT/gWidgets2RGtk2@inz",
    "iNZightVIT/iNZightTools@1.13.0",
    "iNZightVIT/iNZightTS@dev",
    "iNZightVIT/iNZightMR@2.2.7",
    "iNZightVIT/iNZightPlots@2.15.0",
    "iNZightVIT/iNZightRegression@dev",
    "iNZightVIT/iNZightModules@dev"
)

OS <- Sys.getenv("OS_TYPE")
options(
    repos = c(
        if (OS == "Linux") RSPM = Sys.getenv("RSPM"),
        inzight = "https://r.docker.stat.auckland.ac.nz",
        CRAN = "https://cloud.r-project.org"
    ),
    install.packages.compile.from.source = "never"
)

if (OS == "Windows" && getRversion() < numeric_version("4")) {
    install.packages("RODBC", type = "binary")
}
if (OS == "Windows" && !requireNamespace("utf8", quietly = TRUE)) {
    install.packages("utf8", type = "source")
}
if (OS == "Linux") {
    remotes::install_github("lawremi/RGtk2",
        subdir = "RGtk2",
        dependencies = TRUE,
        INSTALL_opts = "--no-multiarch"
    )
    remotes::install_github("tmelliott/cairoDevice",
        dependencies = TRUE,
        INSTALL_opts = "--no-multiarch"
    )
    install.packages(
        c("gWidgets2", "gWidgets2RGtk2"),
        repos = c(getOption("repos"), "https://r.docker.stat.auckland.ac.nz")
    )
}

remotes::install_github(
    github_deps,
    dependencies = TRUE,
    INSTALL_opts = "--no-multiarch"
)
remotes::install_deps(
    dependencies = TRUE,
    INSTALL_opts = "--no-multiarch"
)
remotes::install_cran("rcmdcheck")
