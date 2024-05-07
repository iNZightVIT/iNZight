cat("::group::Checking RGtk2 ...\n")
library(RGtk2)
w <- try(gtkWindowNew(), silent = TRUE)
if (inherits(w, "try-error") || is.null(w)) {
    cat("::error::RGtk2 failed to load")
    quit(status = 1)
}
if (!"GdkWindow" %in% class(w$window)) {
    cat("::error::RGtk2 hasn't loaded correctly")
    quit(status = 1)
}
cat("::endgroup::\n")
