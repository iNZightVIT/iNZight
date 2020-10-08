context("Data manipulation and information")

# try(ui$close(), TRUE); load_all()
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(try(ui$close(), TRUE))
Sys.sleep(5)

test_that("Data Maid report", {
    skip_if_not(rmarkdown::pandoc_available())
    w <- iNZDataReportWin$new(ui)
    on.exit(try(gWidgets2::dispose(w$win), TRUE))
})

test_that("Subsetting and reordering columns", {
    # try(gWidgets2::dispose(ui$modWin), TRUE); load_all()
    w <- iNZReorderVarsWin$new(ui)
    on.exit(try(gWidgets2::dispose(ui$modWin), TRUE))
})

test_that("Filtering data leaves code OK", {
    ui$ctrlWidget$V1box$set_value("height")
    ui$ctrlWidget$V2box$set_value("armspan")
    ui$getActiveDoc()$setSettings(list(colby = as.name("gender")))
    expect_match(
        svalue(ui$code_panel$input),
        "inzplot(height ~ armspan, colby = gender, data = data)",
        fixed = TRUE
    )

    w <- iNZFilterWin$new(ui)
    gWidgets2::dispose(ui$modWin)
    w$opt1()
    ui$modWin$children[[1]]$children[[1]]$children[[2]]$set_value("gender")
    svalue(ui$modWin$children[[1]]$children[[2]]) <- "male"
    expect_silent(
        ui$modWin$children[[1]]$children[[3]]$children[[1]]$invoke_change_handler()
    )
    expect_match(
        svalue(ui$code_panel$input),
        "inzplot(height ~ armspan, colby = gender, data = data.filtered)",
        fixed = TRUE
    )
})

iNZAggragateWin <- setRefClass(
    "iNZAggragateWin",
    fields = list(
        GUI = "ANY",
        data = "ANY",
        catvars = "character", numvars = "character",
        available_aggvars = "ANY", aggvars = "ANY",
        aggbtn_add = "ANY", aggbtn_rmv = "ANY",
        btn_up = "ANY", btn_down = "ANY", reordering = "logical",
        smryvars = "ANY",
        close_btn = "ANY", ok_btn = "ANY"
    ),
    methods = list(
        initialize = function(gui) {
            initFields(
                GUI = gui,
                data = gui$getActiveData(),
                reordering = FALSE
            )
            allvars <- colnames(data)
            vt <- sapply(data, iNZightTools::vartype)
            catvars <<- allvars[vt == "cat"]
            numvars <<- allvars[vt != "cat"] # includes datetimes

            try(dispose(GUI$modWin), silent = TRUE)

            GUI$modWin <<- gwindow("Aggregate data",
                parent = GUI$win,
                visible = FALSE,
                width = 800,
                height = 500
            )
            g <- gvbox(container = GUI$modWin, expand = TRUE)
            g$set_borderwidth(5)

            ########################## Main body
            mainGrp <- ggroup(
                container = g,
                expand = TRUE
            )
            mainGrp$set_borderwidth(10)

            ### +++++++ Variable selection
            g_var <- gvbox(container = mainGrp, expand = TRUE)

            ### +++ Aggregation variables
            g_aggvars <- gframe("Choose aggregation variables",
                container = g_var)
            font(g_aggvars) <- list(weight = "bold")
            g_aggvars$set_borderwidth(5)

            available_aggvars <<- gtable(items = list(Available = catvars),
                multiple = TRUE,
                container = g_aggvars)
            size(available_aggvars) <<- c(100, 160)

            g_aggbtns <- gvbox(container = g_aggvars)
            addSpring(g_aggbtns)
            aggbtn_add <<- gimagebutton("forward",
                size = "large_toolbar",
                container = g_aggbtns,
                tooltip = "Add selected",
                handler = function(h, ...) {
                    add_aggvars(svalue(available_aggvars, index = TRUE))
                }
            )
            aggbtn_rmv <<- gimagebutton("backward",
                size = "large_toolbar",
                container = g_aggbtns,
                tooltip = "Remove selected",
                handler = function(h, ...) {
                    rmv_aggvars(svalue(aggvars, index = TRUE))
                }
            )
            addSpring(g_aggbtns)

            aggvars <<- gtable(items = list(Selected = character()),
                multiple = TRUE,
                container = g_aggvars)
            size(aggvars) <<- c(100, 160)

            g_aggbtns2 <- gvbox(container = g_aggvars)
            btn_up <<- gimagebutton("1uparrow",
                size = "large_toolbar",
                container = g_aggbtns2,
                tooltip = "Move selected variable up",
                handler = function(h, ...) {
                    if (reordering) return()
                    reordering <<- TRUE
                    on.exit(reordering <<- FALSE)

                    index <- svalue(aggvars, index = TRUE)
                    if (length(index) != 1) return()
                    if (index == 1) return()
                    selected <- aggvars$get_items()
                    selected_index <- seq_along(selected)
                    selected_index[index] <- index - 1
                    selected_index[index - 1] <- index
                    selected <- selected[selected_index]
                    aggvars$set_items(data.frame(Selected = selected))
                    svalue(aggvars) <<- index - 1
                }
            )
            btn_down <<- gimagebutton("1downarrow",
                size = "large_toolbar",
                container = g_aggbtns2,
                tooltip = "Move selected variable down",
                handler = function(h, ...) {
                    if (reordering) return()
                    reordering <<- TRUE
                    on.exit(reordering <<- FALSE)

                    index <- svalue(aggvars, index = TRUE)
                    if (length(index) != 1) return()
                    selected <- aggvars$get_items()
                    if (index == length(selected)) return()
                    selected_index <- seq_along(selected)
                    selected_index[index] <- index + 1
                    selected_index[index + 1] <- index
                    selected <- selected[selected_index]
                    aggvars$set_items(data.frame(Selected = selected))
                    svalue(aggvars) <<- index + 1
                }
            )



            ### +++ Summary variables
            g_smryvars <- gframe("Choose variables to summarise",
                container = g_var)
            font(g_smryvars) <- list(weight = "bold")
            g_smryvars$set_borderwidth(5)

            smryvars <<- gtable(list(Summarize = numvars),
                multiple = TRUE,
                container = g_smryvars)
            size(smryvars) <<- c(100, 160)


            ### +++++++ Summary selection
            gsmry <- gframe("Summaries to calculate",
                container = mainGrp)


            addSpring(g)

            ########################## Window buttons
            btnGrp <- ggroup(container = g)
            close_btn <<- gbutton("Close",
                container = btnGrp,
                handler = function(h, ...) dispose(GUI$modWin))
            addSpring(btnGrp)
            ok_btn <<- gbutton("Aggregate",
                container = btnGrp,
                handler = function(h, ...) do_aggregation())

            visible(GUI$modWin) <<- TRUE
        },
        add_aggvars = function(index) {
            available <- as.character(available_aggvars$get_items())
            selected <- as.character(aggvars$get_items())

            if (index[1] == -1) {
                selected <- c(selected, available)
                available <- ""
            } else {
                selected <- c(selected, available[index])
                available <- available[-index]
                if (length(available) == 0) available <- ""
            }
            selected <- selected[selected != ""]

            available_aggvars$set_items(data.frame(Available = available))
            aggvars$set_items(data.frame(Selected = selected))
        },
        rmv_aggvars = function(index) {
            available <- as.character(available_aggvars$get_items())
            selected <- as.character(aggvars$get_items())

            if (index[1] == -1) {
                available <- c(available, selected)
                selected <- ""
            } else {
                available <- c(available, selected[index])
                selected <- selected[-index]
                if (length(selected) == 0) selected <- ""
            }
            available <- available[available != ""]

            available_aggvars$set_items(data.frame(Available = available))
            aggvars$set_items(data.frame(Selected = selected))
        },
        do_aggregation = function() {

        }
    )
)

w <- iNZAggragateWin$new(ui)


test_that("Aggregating data adds correct code", {
    w <- iNZAggragateWin$new(ui)
    w <- ui$modWin
    w$children[[1]]$children[[3]]$children[[2]]$set_value("gender")
    w$children[[1]]$children[[3]]$children[[4]]$set_value("travel")
    w$children[[1]]$children[[3]]$children[[7]]$set_value("Mean")
    expect_silent(
        w$children[[1]]$children[[4]]$children[[1]]$invoke_change_handler()
    )
})
