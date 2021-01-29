# playing around with the graphics events
library(gWidgets2)
library(RGtk2) # make available

try(dispose(w), TRUE)
w <- gwindow()
g <- ggroup(container = w)

d <- ggraphics(container = g, fill = TRUE, expand = TRUE)

library(iNZightPlots)
inzplot(Sepal.Length ~ Sepal.Width, data = iris)

# can we get the rubberband coords?

d$add_handler_selection_changed(function(h, ...) {
    rb <- d$rubber_band
    # if (!rb$dragging) return()

    # from top-left
    # want: from bottom-left
    ymax <- size(d)[[2]]
    p0 <- c(rb$x0, ymax - rb$y0)
    p1 <- c(rb$x, ymax - rb$y)

    # convert to percentage ..
    dx <- size(d)
    p0 <- p0 / dx
    p1 <- p1 / dx
    print(p0)
    print(p1)

    # location on the ROOT
    plist <- lapply(list(p0, p1),
        function(p) {
            p <- grid::unit(p, "npc")
            print(p)
            c(
                grid::convertX(p[1], "inches"),
                grid::convertY(p[2], "inches")
            )
        }
    )
    print("===")
    print(plist)
    p0 <- plist[[1]]
    p1 <- plist[[2]]

    grid::seekViewport("VP:locate.these.points")
    vp <- grid::current.viewport()
    transform <- solve(grid::current.transform())

    plist <- sapply(list(p0, p1),
        function(p) {
            location <- (c(p, 1) %*% transform)
            location <- grid::unit(location / location[3L], "inches")
            print(location)
            c(
                x = grid::convertX(location[1L], "native"),
                y = grid::convertY(location[2L], "native")
            )
        }
    )
    print("---")
    print(plist)

    plist <- as.data.frame(apply(plist, 1, sort))
    rowid <- which(
        iris$Sepal.Width >= plist$x[1] &
        iris$Sepal.Width <= plist$x[2] &
        iris$Sepal.Length >= plist$y[1] &
        iris$Sepal.Length <= plist$y[2],
    )
    inzplot(Sepal.Length ~ Sepal.Width, data = iris,
        locate.id = rowid, locate.col = 'red')
})
