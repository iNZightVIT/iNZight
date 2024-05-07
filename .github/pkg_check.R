## --------------------------------------------------------------------
CHECK_DIR <- "check"
ARGS <- c("--no-manual", "--as-cran")
BUILD_ARGS <- "--no-manual"
ERROR_ON <- "warning"

options(crayon.enabled = TRUE)
cat("LOGNAME=", Sys.info()[["user"]], "\n",
    sep = "",
    file = Sys.getenv("GITHUB_ENV"), append = TRUE
)
if (Sys.getenv("_R_CHECK_FORCE_SUGGESTS_", "") == "") {
    Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = "false")
}
if (Sys.getenv("_R_CHECK_CRAN_INCOMING_", "") == "") {
    Sys.setenv("_R_CHECK_CRAN_INCOMING_" = "false")
}
cat("check-dir-path=", file.path(getwd(), CHECK_DIR), "\n",
    file = Sys.getenv("GITHUB_OUTPUT"), sep = "", append = TRUE
)
check_results <- rcmdcheck::rcmdcheck(
    args = ARGS,
    build_args = BUILD_ARGS,
    error_on = ERROR_ON,
    check_dir = CHECK_DIR
)
