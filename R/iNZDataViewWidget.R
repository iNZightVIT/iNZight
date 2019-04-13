iNZDataViewWidget <- setRefClass(
    "iNZDataViewWidget",
    fields = list(
        GUI = "ANY", ## the iNZight GUI object
        dataGp = "ANY", ## group containing the 2 different view groups
        dfView = "ANY", ## group that contains data view
        varView = "ANY", ## group that contains variable view
        ## max size before dataview gets deactived
        dataThreshold = "numeric"
        ),
    methods = list(
        initialize = function(gui, dataThreshold) {
            initFields(GUI = gui,
                       dataThreshold = dataThreshold)
            dataGp <<- ggroup(horizontal = TRUE, expand = TRUE)
            dataSet <- GUI$getActiveData()
            ## create the data.frame view
            createDfView()
            ## create the variable view
            createVarView()
            ## start in dataView if size is less than 200k
            if (nrow(dataSet) * ncol(dataSet) <= dataThreshold)
                visible(dfView) <<- TRUE
            else
                visible(varView) <<- TRUE
        },
        ## recreate both views with active dataset
        updateWidget = function() {
            view <- visible(dfView)
            ## delete the currently displayed views
            try(invisible(sapply(dataGp$children,
                                 function(x) delete(dataGp, x))),
                silent = TRUE)
            ## create the data.frame view
            createDfView()
            ## create the variable view
            createVarView()
            dataSet <- GUI$getActiveData()
            if(!view || (nrow(dataSet) * ncol(dataSet) > dataThreshold))
                visible(varView) <<- TRUE
            else
                visible(dfView) <<- TRUE

        },
        ## only update the variable view
        updateVarView = function() {
            view <- visible(varView)
            createVarView()
            visible(varView) <<- view
        },
        ## only update the data.frame view
        updateDfView = function() {
            view <- visible(dfView)
            createDfView()
            visible(dfView) <<- view
        },
        ## create the data.frame view (invisible)
        createDfView = function() {
            dataSet <- GUI$getActiveData()
            dfView <<- ggroup(container = dataGp, expand = TRUE)

            ## only create the gdf if the dataset is small enough
            if (nrow(dataSet) * ncol(dataSet) <= dataThreshold) {
                visible(dfView) <<- FALSE
                dfWidget <- gdf(dataSet, expand = TRUE)
                ## dfWidget$remove_popup_menu() ## - called by $add_dnd_columns()
                dfWidget$add_dnd_columns()
                add(dfView, dfWidget, expand = TRUE)
                ## if the data.frame gets edited, update the iNZDocument
                addHandlerChanged(
                    dfWidget,
                    handler = function(h, ...) {
                        X1 <- dfWidget[]
                        if(class(X1) != "data.frame")
                            newData <- data.frame(X1)
                        GUI$getActiveDoc()$getModel()$updateData(X1)})
            } else {
                visible(dfView) <<- FALSE
            }
        },
        ## create variable view (invisible)
        createVarView = function() {
            dataSet <- GUI$getActiveData()
            varView <<- ggroup(container = dataGp, expand = TRUE)
            visible(varView) <<- FALSE
            ## if more than 19 columns are in the dataSet, split the variable
            ## view into 2 tables
            N <- 19

            ## prefix variable type to variable names
            vnames <- names(dataSet)
            ## These are explicitely removes by `gsub` in the addDropSource handler below
            vtypes <- sapply(dataSet, function(x) 
                switch(iNZightTools::vartype(x), 
                    'num' = '(n)', 
                    'cat' = '(c)', 
                    'dt' = '(t)'
                ))

            vnames <- paste(vtypes, vnames)

            # if(length(names(dataSet)) > N && length(names(dataSet)) < 80) {
            if(length(dataSet) < 100000 && length(names(dataSet)) > 80) {
                varWidget <- list(
                    gtable(vnames[1:floor(N/2)], expand = TRUE),
                    gtable(vnames[(floor(N/2)+1):ncol(dataSet)],
                           expand = TRUE))
                names(varWidget[[1]]) <- "VARIABLES"
                names(varWidget[[2]]) <- "...CONTINUED"
            } else {
                varWidget <- list(gtable(vnames, expand = TRUE))
                names(varWidget[[1]]) <- "VARIABLES (n = numeric, c = categorical, dt = date/time)"
            }
            ## use the variable view as dropsource and add to data group
            invisible(lapply(varWidget, function(x) {
                add(varView, x, expand = TRUE)
                x$remove_popup_menu()
                addDropSource(x, handler = function(h, ...) {
                    ## Remove the variable type from the tag (otherwise `variable doesn't exist`)
                    gsub("\\([a-z]\\) ", "", svalue(h$obj)) 
                })}))
        },
        ## change the currently active View
        changeView = function() {
            if(visible(dfView)) {
                visible(dfView) <<- FALSE
                visible(varView) <<- TRUE
            } else {
                visible(varView) <<- FALSE
                visible(dfView) <<- TRUE
            }
        },
        ## set view to data.frame view
        dataView = function() {
            visible(varView) <<- FALSE
            visible(dfView) <<- TRUE
        },
        ## set view to list of columns
        listView = function() {
            visible(dfView) <<- FALSE
            visible(varView) <<- TRUE
        })
    )
