DemoModule <- setRefClass(
    "Demo Module",
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
            label <- glabel("This is a demo module",
                container = mainGrp)

            cat("Running new module\n")
        },
        close = function() {
            cat("Closing module\n")

            callSuper()
        }
    ),
    ## This is currently required to get around namespace locking
    where = e
)
