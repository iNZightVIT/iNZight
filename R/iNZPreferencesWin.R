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
                GUI$modWin <<- gwindow("iNZight Preferences",
                    parent = GUI$win,
                    width = 600,
                    height = 400
                )

                g <- gvbox(container = GUI$modWin, expand = FALSE)
                g$set_borderwidth(5)

                updOpt <- gcheckbox(
                    "Check for updates when iNZight launched (NOTE: this will not automatically update iNZight)",
                    checked = prefs$check.updates
                )
                add(g, updOpt)

                addSpace(g, 30)

                lab <- glabel("Default Window Size - Changes take effect next time your start iNZight")
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

                addSpace(g, 30)

                gfontSize <- ggroup(container = g)
                glabel("Font size (for summary and inference windows):", container = gfontSize)
                fsizebtn <- gspinbutton(5, 30, by = 1, value = prefs$font.size, container = gfontSize)
                fsizeprv <- gtext("Preview text",
                    height = 40,
                    width = 200,
                    container = gfontSize,
                    font.attr = list(size = svalue(fsizebtn))
                )
                addHandlerChanged(fsizebtn,
                    function(h, ...) {
                        # this doesn't work [yet] for numeric size values
                        # fsizeprv$set_font(list(size = svalue(h$obj)))
                        svalue(fsizeprv) <- ""
                        insert(fsizeprv, "Preview text", font.attr = list(size = svalue(h$obj)))
                    }
                )

                addSpace(g, 30)

                popoutWin <- gcheckbox(
                    "Use dual-window display mode (strongly advised for Windows users who are not computer novices",
                    checked = prefs$popout
                )
                add(g, popoutWin)
                addSpring(g)

                ## CANCEL / OK buttons
                btnGrp <- ggroup(container = g, expand = FALSE)
                addSpring(btnGrp)

                cancelButton <- gbutton("Cancel",
                    expand = FALSE,
                    cont = btnGrp,
                    handler = function(h, ...) dispose(GUI$modWin)
                )

                addSpace(btnGrp, 15)

                okButton <- gbutton("Save",
                    expand = FALSE,
                    cont = btnGrp,
                    handler = function(h, ...) {
                        GUI$preferences <<- list(
                            check.updates = svalue(updOpt),
                            window.size = as.numeric(c(svalue(winWd), svalue(winHt))),
                            popout = svalue(popoutWin),
                            font.size = svalue(fsizebtn)
                        )
                        GUI$savePreferences()
                        dispose(GUI$modWin)
                    }
                )

                addSpace(btnGrp, 15)

                ## extra space between buttons and bottom of window
                addSpace(g, 15)

                visible(GUI$modWin) <<- TRUE
            }
        }
    )
)
