DemoModule3 <- setRefClass(
    "DemoModule3",
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
            label <- glabel("This is yet another demo module",
                container = mainGrp)

            cat("Running a newer module\n")
        },
        close = function() {
            cat("Closing module 3\n")

            callSuper()
        }
    ),
    where = e
)
