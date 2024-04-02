# R script
github_deps <- c(
    "tmelliott/surveyspec",
    "tmelliott/gWidgets2@patch-1",
    "iNZightVIT/gWidgets2RGtk2@inz",
    "iNZightVIT/iNZightTools@2.0.0",
    "iNZightVIT/iNZightTS@1.5.10",
    "iNZightVIT/iNZightMR@2.3.0",
    "iNZightVIT/iNZightPlots@2.15.0",
    "iNZightVIT/iNZightRegression@1.3.3",
    "iNZightVIT/iNZightModules@2.5.3"
)

OS <- Sys.getenv("OS_TYPE")
options(
    repos = c(
        if (OS == "Linux") RSPM <- Sys.getenv("RSPM"),
        inzight = "https://r.docker.stat.auckland.ac.nz",
        CRAN = "https://cloud.r-project.org"
    )
    # install.packages.compile.from.source = "never"
)

if (OS == "Windows" && getRversion() < numeric_version("4")) {
    install.packages("RODBC", type = "binary")
}
if (OS == "Windows" && !requireNamespace("utf8", quietly = TRUE)) {
    install.packages("utf8", type = "source")
}
if (OS == "Windows" && packageVersion("Matrix") < "1.6-0") {
    install.packages("Matrix", type = "source")
}
# rlang >= 1.1.3
if (OS == "Windows" &&
    (!requireNamespace("rlang", quietly = TRUE) ||
        packageVersion("rlang") < "1.1.3")
) {
    install.packages("rlang", type = "source")
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

if (OS == "Windows") {
    if (!requireNamespace("estimability", quietly = TRUE)) {
        remotes::install_version("estimability", version = "1.4.1")
    }
    if (!requireNamespace("vctrs", quietly = TRUE)) {
        install.packages("vctrs", type = "source")
    }
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
