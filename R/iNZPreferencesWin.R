iNZPrefsWin <- setRefClass(
    "iNZPrefsWin",
    fields = list(
        GUI = "ANY"
        ),
    methods = list(
        initialize = function(gui = NULL) {
            initFields(GUI = gui)

            prefs <- GUI$preferences
            
            if (!is.null(GUI)) {
               try(dispose(GUI$modWin), silent = TRUE) ## close any current mod windows
               GUI$modWin <<- gwindow("iNZight Preferences", parent = GUI$win,
                                      width = 600, height = 400)

               g <- gvbox(container = GUI$modWin, expand = FALSE)
               g$set_borderwidth(5)

               trackOpt <- gcheckbox("Allow iNZight to collect annonymous usage information",
                                     checked = prefs$track)
               add(g, trackOpt)

               updOpt <- gcheckbox("Check for updates when iNZight launched (NOTE: this will not automatically update iNZight)",
                                     checked = prefs$check.updates)
               add(g, updOpt)

               

               btnGrp <- ggroup(container = g, expand = FALSE)
               okButton <- gbutton("OK", expand = FALSE,
                             handler = function(h, ...) {
                                 GUI$preferences <<- list(track = svalue(trackOpt),
                                                          check.updates = svalue(updOpt))
                                 GUI$savePreferences()
                                 dispose(GUI$modWin)
                             })
               cancelButton <- gbutton("Cancel", expand = FALSE,
                                       handler = function(h, ...) dispose(GUI$modWin))
               btnTb <- glayout()
               btnTb[1, 1, expand = TRUE] <- okButton
               btnTb[1, 2, expand = TRUE] <- cancelButton
               add(btnGrp, btnTb)
               
               visible(GUI$modWin) <- TRUE
           }
        }
        )
    )
