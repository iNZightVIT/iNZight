gpagedtable <- function(items, ncol = 5L, container = NULL, ...) {
    GPagedTable$new(items, ncol, container, ...)
}

GPagedTable <- setRefClass(
    "GPagedTable",
    # contains = "GWidgetWithItems",
    fields = list(
        block = "ANY",
        items = "ANY",
        table = "ANY",
        pager = "ANY",
        pageLabel = "ANY"
    ),
    methods = list(
        initialize = function(items = NULL, nc = 5L, container = NULL, ...) {
            initFields(
                items = items,
                table = gtable(data.frame()),
                pager = list(
                    pageSize = nc,
                    nPage = 1L,
                    page = 1L
                ),
                pageLabel = glabel("")
            )

            block <<- gvbox(container = container, ...)

            g_ctrls <- ggroup(container = block)
            addSpring(g_ctrls)
            btn_back <- gimagebutton("backward",
                container = g_ctrls,
                handler = function(h, ...) incrementPage(-1L)
            )
            add(g_ctrls, pageLabel)
            btn_fwd <- gimagebutton("forward",
                container = g_ctrls,
                handler = function(h, ...) incrementPage(1L)
            )

            add(block, table, expand = TRUE, fill = TRUE)

            if (!is.null(items)) set_items(items)
        },
        set_items = function(items) {
            items <<- items
            pager$nPage <<- ceiling(ncol(items) / pager$pageSize)
            pager$page <<- 1L

            update()
        },
        update = function() {
            cfrom <- (pager$page - 1L) * pager$pageSize + 1L
            cto <- min(ncol(items), cfrom + pager$pageSize - 1L)

            table$set_items(
                items[, cfrom:cto, drop = FALSE]
            )
            pageLabel$set_value(sprintf("%d of %d", pager$page, pager$nPage))
        },
        incrementPage = function(x) {
            if (abs(x) != 1L) warning("only increment by 1 or -1")
            if (x > 0 && pager$page < pager$nPage) pager$page <<- pager$page + 1L
            if (x < 0 && pager$page > 1L) pager$page <<- pager$page - 1L
            update()
        }
    )
)
