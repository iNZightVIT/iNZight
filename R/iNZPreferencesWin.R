iNZPrefsWin <- setRefClass(
    "iNZPrefsWin",
    fields = list(
        GUI = "ANY",
        prefs = "list",
        curprefs = "list",
        sections = "ANY",
        cancelBtn = "ANY", saveBtn = "ANY"
    ),
    methods = list(
        initialize = function(gui = NULL) {
            if (is.null(gui)) return()
            initFields(GUI = gui, prefs = gui$preferences, curprefs = gui$preferences)

            try(dispose(GUI$modWin), silent = TRUE)
            GUI$modWin <<- gwindow("iNZight Preferences",
                parent = GUI$win,
                width = 700,
                height = 500,
                visible = FALSE
            )

            g_main <- gvbox(container = GUI$modWin)
            g_main$set_borderwidth(5L)

            sections <<- gnotebook(
                tab.pos = 3L,
                container = g_main,
                expand = TRUE
            )

            ## --------------------------- GENERAL
            sec_general <- gvbox(label = "General", container = sections)
            sec_general$set_borderwidth(5L)

            ### ---------------- Check for updates
            p_check.updates <- gcheckbox(
                "Check for updates when iNZight launched",
                checked = prefs$check.updates,
                container = sec_general,
                handler = function(h, ...) set_pref("check.updates", svalue(h$obj))
            )
            lbl <- glabel(paste(sep = "\n",
                "If updates are available, this will be displayed in the title bar of iNZight.",
                "Updates will not automatically be applied."),
                container = sec_general,
                anchor = c(-1, 0))
            font(lbl) <- list(size = 9)


            ## --------------------------- APPEARANCE
            sec_appearance <- gvbox(label = "Appearance", container = sections)



            ################ BUTTONS
            g_buttons <- ggroup(container = g_main)
            addSpring(g_buttons)

            cancelBtn <<- gbutton("Exit without saving",
                container = g_buttons,
                handler = function(h, ...) dispose(GUI$modWin))

            saveBtn <<- gbutton("Save changes",
                container = g_buttons,
                handler = function(h, ...) {
                    print(prefs)
                }
            )
            enabled(saveBtn) <<- FALSE

            svalue(sections) <<- 1L
            on.exit(visible(GUI$modWin) <<- TRUE)


            if (FALSE) {
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

                devFeatures <- gcheckbox(
                    "Enable developmental features",
                    checked = prefs$dev.features
                )
                add(g, devFeatures)
                # lbl <- glabel(
                #     paste(sep = "\n",
                #         "     These will be marked with [beta], and may change or have unintended side effects, such as crashing.",
                #         "     You can help us improve them by lettings us know if you experience problems!"
                #     )
                # )
                # font(lbl) <- list(size = 8)
                # add(g, lbl, expand = TRUE, anchor = c(-1, 0))

                ## Code
                showCode <- gcheckbox(
                    "Display editable code boxes for the current plot or summary information",
                    checked = prefs$showCode
                )
                enabled(showCode) <- svalue(devFeatures)
                add(g, showCode)

                addHandlerChanged(devFeatures,
                    handler = function(h, ...) {
                        enabled(showCode) <- svalue(devFeatures)
                    }
                )

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
                            font.size = svalue(fsizebtn),
                            dev.features = svalue(devFeatures),
                            show.code = svalue(showCode)
                        )
                        GUI$savePreferences()
                        dispose(GUI$modWin)

                        confmsg <- paste(sep = "\n",
                            "Some changes require reloading iNZight. Do that now?",
                            "All changes will be saved."
                        )
                        if (!interactive() || gconfirm(confmsg, icon = "question"))
                            GUI$reload()
                    }
                )

                addSpace(btnGrp, 15)

                ## extra space between buttons and bottom of window
                addSpace(g, 15)

                visible(GUI$modWin) <<- TRUE
            }
        },
        set_pref = function(name, value) {
            prefs[[name]] <<- value
            enabled(saveBtn) <<- !identical(prefs, curprefs)
        }
    )
)
