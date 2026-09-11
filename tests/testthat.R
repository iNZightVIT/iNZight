## Test file order (alphabetical prefixes):
##   A foundation | B data I/O | C wrangling | D plots/code | E analysis | F surveys | G modules
library(testthat)
library(iNZight)

options(readr.show_progress = FALSE)

test_check("iNZight")

## Close leftover devices/windows and drain finalizers while the loop lives.
## Do not stop the event loop here (ANR on a still-connected display client).
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
