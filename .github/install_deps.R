# R script
if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak", type = "source")
}

# split by ,\n
needs <- strsplit(
    read.dcf("DESCRIPTION")[1, "Config/Needs/dependencies"],
    ",\n"
)[[1]]

# drop RGtk2
needs <- needs[!grepl("RGtk2$", needs)]

pak::repo_add("https://r.docker.stat.auckland.ac.nz")

if (Sys.getenv("OS_TYPE") == "Windows" &&
    getRversion() < numeric_version("4.4")) {
    needs <- c(
        needs,
        "Matrix@1.6-5",
        "MatrixModels@0.5-2"
    )
}

pak::pak(c(".", needs, "iNZightMaps=?ignore"),
    dependencies = TRUE
)
pak::pak("rcmdcheck")
