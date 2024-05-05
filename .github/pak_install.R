# Install/Update packages
cat("::group::Install/update packages\n")
Sys.setenv("PKGCACHE_HTTP_VERSION" = "2")
library(pak, lib.loc = Sys.getenv("R_LIB_FOR_PAK"))
options(repos = c(options$repos, "https://r.docker.stat.auckland.ac.nz"))
pak::lockfile_install(".github/pkg.lock")
## Clean up lock file
unlink(".github/pkg.lock")
cat("::endgroup::\n")
