DemoModule2 <- setRefClass(
    "DemoModule2",
    contains = "CustomModule",
    fields = list(
        GUI = "ANY"
    ),
    methods = list(
        initialize = function(gui, name) {
            callSuper(gui, 
                name = name,
                embedded = TRUE
            )

            ## The main code for your module goes here,
            ## inside a top-level container called "mainGrp"
            label <- glabel("This is a second demo module",
                container = mainGrp)

            cat("Running new module 2\n")
        },
        close = function() {
            cat("Closing module 2\n")

            callSuper()
        }
    ),
    where = e
)
