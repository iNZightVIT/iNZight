## Suite-end cleanup for leftover GTK UI / graphics devices.
##
## Do NOT call gtkStopEventLoop() here: with a live display connection that
## freezes WM pings → "Application Not Responding". Stop the loop only when
## the package unloads / the R process exits.
##
## After closing devices/windows, pump + GC so GObject finalizers run while
## the event loop is still alive (interactive `q()` is otherwise very slow).
withr::defer(
    {
        try(
            {
                while (length(grDevices::dev.list())) {
                    try(grDevices::dev.off(), silent = TRUE)
                }
                tm <- Rgtk4::gtkWindowGetToplevels()
                n <- Rgtk4::gListModelGetNItems(tm)
                for (i in rev(seq_len(as.integer(n)) - 1L)) {
                    w <- Rgtk4::gListModelGetObject(tm, i)
                    try(Rgtk4::gtkWindowDestroy(w), silent = TRUE)
                }
                for (i in seq_len(20L)) {
                    try(Rgtk4::gtkMainIterationDo(FALSE), silent = TRUE)
                }
                for (i in seq_len(5L)) {
                    gc()
                    for (j in seq_len(10L)) {
                        try(Rgtk4::gtkMainIterationDo(FALSE), silent = TRUE)
                    }
                }
            },
            silent = TRUE
        )
    },
    teardown_env()
)

test_that("gtk cleanup registered", {
    expect_true(TRUE)
})
