iNZInfoWindow <- setRefClass(
    "iNZInfoWindow",
    fields = list(
        GUI = "ANY",
        env = "ANY",
        dataname = "ANY",
        curSet = "ANY", curMod = "ANY",
        win = "ANY",
        control_position = "character",
        info_text = "ANY", info_font = "list",
        ctrl_panel = "ANY",
        code_panel = "ANY", code_box = "ANY",
        font_size = "numeric"
    ),
    methods = list(
        initialize = function(gui, controls = c("bottom", "top"),
                              name = "Information Window") {
            initFields(
                GUI = gui,
                control_position = controls,
                font_size = gui$preferences$font.size
            )

            # Check that the data exists
            env <<- new.env()
            curSet <<- GUI$getActiveDoc()$getSettings()
            if (is.null(curSet$x)) {
                gmessage("No variable selected.")
                dispose(win)
                return()
            }
            gen_set_list()

            win <<- gwindow(title = name,
                width = 900 * font_size / 10,
                height = 600 * font_size / 10,
                parent = GUI$win,
                visible = FALSE
            )

            code_panel <<- ggroup()
            code_box <<- gtext("info_function(...)",
                expand = TRUE,
                wrap = FALSE,
                font.attr = list(
                    family = "monospace",
                    size = font_size
                ),
                container = code_panel
            )

            info_font <<- list(
                family = "monospace",
                size = font_size
            )
            info_text <<- gtext(
                text = "",
                wrap = FALSE,
                font.attr = info_font
            )

            ctrl_panel <<- ggroup()


            # Main container will consist of three components:
            #  1. code panel (can be toggled; controls info)
            #  2. info text
            #  3. control panel (controls code)
            g <- gvbox(container = win)

            if (controls == "top") add(g, ctrl_panel)
            add(g, code_panel)
            add(g, info_text, expand = TRUE)
            if (controls == "bottom") add(g, ctrl_panel)

        },
        gen_set_list = function() {
            "Generate the initial settings list"
            dataname <<- GUI$dataNameWidget$datName
            curSet$data <<- as.name(dataname)
            curSet$data_name <<- dataname
            ## Design or data?
            curMod <<- GUI$getActiveDoc()$getModel()
            assign(dataname, GUI$getActiveData(), envir = env)

            if (!is.null(curMod$dataDesign)) {
                stop("not working yet")
                curSet$data <<- NULL
                # assign
                # design <<- curMod$createSurveyObject()
                # curSet$design <<- .design
            }
        }
    )
)


## A summary window
iNZGetSummary <- setRefClass(
    "iNZGetSummary",
    contains = "iNZInfoWindow",
    fields = list(
    ),
    methods = list(
        initialize = function(gui) {
            callSuper(gui, controls = "bottom", name = "Summary")

            smry_call <- gen_call()
            svalue(code_box) <<- smry_call
            font(code_box) <<- info_font

            smry <- eval(smry_call, env)
            svalue(info_text) <<- paste(smry, collapse = "\n")
            font(info_text) <<- info_font

            visible(win) <<- TRUE
        },
        gen_call = function() {
            "Generate the function call based on user's chosen vars/settings"

            # This will, at some stage, fetch values from the CODE CALL
            # when it is modified by the user ... and update curSet ... =]

            construct_call(curSet, curMod,
                data = as.name(dataname),
                what = "summary"
            )
        }
    )
)
