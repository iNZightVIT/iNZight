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

               lab <- glabel("Default Window Size (will take effect next time you start iNZight)")
               font(lab) <- list(weight = "bold")
               add(g, lab, anchor = c(-1, -1))

               tbl <- glayout()
               tbl[1, 1] <- glabel("Width (px): ")
               winWd <- gedit(prefs$window.size[1], width = 4)
               tbl[1, 2] <- winWd
               tbl[1, 4] <- glabel("Height (px): ")
               winHt <- gedit(prefs$window.size[2], width = 4)
               tbl[1, 5] <- winHt

               useCur <- gbutton("Use current dimensions")
               addHandlerClicked(useCur, function(h, ...) {
                   curDim <- size(GUI$win)
                   svalue(winWd) <- curDim[1]
                   svalue(winHt) <- curDim[2]
               })
               tbl[1, 8] <- useCur

               useDef <- gbutton("Reset default")
               addHandlerClicked(useDef, function(h, ...) {
                   curDim <- GUI$defaultPrefs()$window.size
                   svalue(winWd) <- curDim[1]
                   svalue(winHt) <- curDim[2]
               })
               tbl[1, 9] <- useDef


               add(g, tbl)
               

               btnGrp <- ggroup(container = g, expand = FALSE)
               okButton <- gbutton("OK", expand = FALSE,
                             handler = function(h, ...) {
                                 GUI$preferences <<- list(track = svalue(trackOpt),
                                                          check.updates = svalue(updOpt),
                                                          window.size =
                                                          as.numeric(c(svalue(winWd), svalue(winHt))))
                                 GUI$savePreferences()
                                 dispose(GUI$modWin)
                             })
               cancelButton <- gbutton("Cancel", expand = FALSE,
                                       handler = function(h, ...) dispose(GUI$modWin))
               btnTb <- glayout()
               btnTb[1, 1, expand = TRUE] <- okButton
               btnTb[1, 2, expand = TRUE] <- cancelButton
               add(btnGrp, btnTb)
               
               visible(GUI$modWin) <<- TRUE
           }
        }
        )
    )
