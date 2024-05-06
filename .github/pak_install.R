# Install/Update packages
cat("::group::Install/update packages\n")
Sys.setenv("PKGCACHE_HTTP_VERSION" = "2")
library(pak, lib.loc = Sys.getenv("R_LIB_FOR_PAK"))

# install RGtk2
if (!requireNamespace("RGtk2", quietly = TRUE)) {
    cat("Installing RGtk2\n")
    install.packages("RGtk2",
        lib = Sys.getenv("R_LIB_FOR_PAK"),
        INSTALL_opts = "--no-multiarch"
    )
}

# install cairoDevice
if (!requireNamespace("cairoDevice", quietly = TRUE)) {
    cat("Installing cairoDevice\n")
    install.packages("cairoDevice",
        lib = Sys.getenv("R_LIB_FOR_PAK"),
        INSTALL_opts = "--no-multiarch"
    )
}

pak::lockfile_install(".github/pkg.lock")
## Clean up lock file
unlink(".github/pkg.lock")
cat("::endgroup::\n")
