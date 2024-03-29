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
            if (is.null(gui)) {
                return()
            }
            initFields(
                GUI = gui,
                prefs = gui$preferences,
                curprefs = gui$preferences
            )

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
                handler = function(h, ...) {
                    set_pref("check.updates", svalue(h$obj))
                }
            )
            lbl <- glabel(
                paste(
                    sep = "\n",
                    "If updates are available, this will be displayed in the title bar of iNZight.",
                    "Updates will not automatically be applied."
                ),
                container = sec_general,
                anchor = c(-1, 0)
            )
            font(lbl) <- list(size = 9)

            ### ---------------- Language
            g_lang <- gformlayout(container = sec_general)
            languages <- c(en = "English")
            p_lang <- gcombobox(languages,
                selected = which(names(languages) == prefs$language),
                label = "Language :",
                container = g_lang,
                handler = function(h, ...) {
                    set_pref("language", names(languages)[h$obj$get_index()])
                }
            )
            enabled(p_lang) <- length(languages) > 1L
            visible(g_lang) <- file.exists(
                system.file("translations.csv", package = "iNZight")
            )

            ## --------------------------- APPEARANCE
            sec_appearance <- gvbox(label = "Appearance", container = sections)
            sec_appearance$set_borderwidth(5L)

            tbl_appearance <- glayout(container = sec_appearance)
            ii <- 1L

            ### ---------------- Window mode: single or dual
            lbl <- glabel("Window mode :")
            p_windowmode <- gcombobox(c("Single", "Dual"),
                selected = prefs$popout + 1L,
                handler = function(h, ...) {
                    set_pref("popout", h$obj$get_index() == 2L)
                }
            )
            tbl_appearance[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_appearance[ii, 2L, expand = TRUE] <- p_windowmode
            ii <- ii + 1L

            lbl <- glabel(
                paste(
                    sep = "\n",
                    "In single window mode (the default), iNZight uses one single window containing",
                    "both the control panel and plot window.",
                    "In dual window mode, the control panel and plot window are separate. Dual window",
                    "mode is recommended for Windows users as the graphics device is prone to 'flickering'",
                    "in single window mode."
                )
            )
            font(lbl) <- list(size = 9)
            tbl_appearance[ii, 2L, anchor = c(-1, 0), expand = TRUE] <- lbl
            ii <- ii + 1L

            lbl <- glabel("Window size (px) :")
            g_windowsize <- ggroup()
            tbl_appearance[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_appearance[ii, 2L, expand = TRUE] <- g_windowsize
            ii <- ii + 1L

            lbl <- glabel("Width :", container = g_windowsize)
            p_window.width <- gspinbutton(300, 2000, 50,
                value = prefs$window.size[1],
                container = g_windowsize,
                handler = function(h, ...) {
                    set_pref("window.size", c(svalue(h$obj), prefs$window.size[2]))
                }
            )
            addSpace(g_windowsize, 10)
            lbl <- glabel("Height :", container = g_windowsize)
            p_window.height <- gspinbutton(200, 1800, 50,
                value = prefs$window.size[2],
                container = g_windowsize,
                handler = function(h, ...) {
                    set_pref("window.size", c(prefs$window.size[1], svalue(h$obj)))
                }
            )

            addSpring(g_windowsize)
            useCur <- gbutton("Use current",
                container = g_windowsize,
                handler = function(h, ...) {
                    curDim <- size(GUI$win)
                    svalue(p_window.width) <- curDim[1]
                    svalue(p_window.height) <- curDim[2]
                }
            )

            useDef <- gbutton("Reset default",
                container = g_windowsize,
                handler = function(h, ...) {
                    curDim <- GUI$defaultPrefs()$window.size
                    svalue(p_window.width) <- curDim[1]
                    svalue(p_window.height) <- curDim[2]
                }
            )

            tbl_appearance[ii, 1:2] <- gseparator()
            ii <- ii + 1L

            lbl <- glabel("Font size :")
            g_fontsize <- ggroup()
            tbl_appearance[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_appearance[ii, 2L, expand = TRUE] <- g_fontsize
            ii <- ii + 1L

            p_font <- gspinbutton(5, 30, 1L,
                value = prefs$font.size,
                container = g_fontsize,
                handler = function(h, ...) {
                    font(font_preview) <- list(size = svalue(h$obj))
                    set_pref("font.size", svalue(h$obj))
                }
            )
            font_preview <- glabel(
                "This is the font size used in summary and inference output.",
                container = g_fontsize
            )
            font(font_preview) <- list(size = prefs$font.size)


            ## --------------------------- PLOT SETTINGS
            plot_settings <- gvbox(label = "Plot Defaults", container = sections)
            plot_settings$set_borderwidth(5L)

            tbl_plot <- glayout(container = plot_settings)
            ii <- 1L

            ### ---------------- Default ggplot theme
            lbl <- glabel("Default (gg) theme :")

            curIndex <-
                if (is.null(prefs$gg_theme)) {
                    1L
                } else if (is.character(prefs$gg_theme)) {
                    which(AVAILABLE_THEMES == prefs$gg_theme)
                } else {
                    length(AVAILABLE_THEMES) + 1L
                }
            p_gg_theme <- gcombobox(
                c(names(AVAILABLE_THEMES), "Custom"),
                selected = curIndex,
                handler = function(h, ...) {
                    set_pref("gg_theme", as.character(AVAILABLE_THEMES[h$obj$get_index()]))
                }
            )
            tbl_plot[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_plot[ii, 2L, expand = TRUE] <- p_gg_theme
            ii <- ii + 1L

            # if custom, display a text box...!



            ## --------------------------- DEV FEATURES
            sec_dev <- gvbox(label = "Developmental Features", container = sections)
            sec_dev$set_borderwidth(5L)
            lbl <- glabel(
                paste(
                    "We may occasionally include developmental features in our",
                    "official release version. This allows users to experience",
                    "\nand give feedback on them as they are being developed,",
                    "but please be aware that they may not be stable and may change",
                    "\nsignificantly before their final version.",
                    "\nIf you do choose to enable these, we will happily take feedback",
                    "sent to inzight_support@stat.auckland.ac.nz"
                ),
                container = sec_dev,
                anchor = c(-1, 0)
            )
            font(lbl) <- list(size = 9)

            ### ---------------- Enable dev features
            p_dev.features <- gcheckbox("Enable developmental features",
                checked = prefs$dev.features,
                container = sec_dev,
                handler = function(h, ...) {
                    set_pref("dev.features", svalue(h$obj))
                    visible(g_dev) <- prefs$dev.features
                }
            )

            g_dev <- gvbox(container = sec_dev)
            visible(g_dev) <- prefs$dev.features

            gseparator(container = g_dev)

            ### ---------------- Code widgets
            p_show.code <- gcheckbox("Show editable code boxes",
                checked = prefs$show.code,
                container = g_dev,
                handler = function(h, ...) set_pref("show.code", svalue(h$obj))
            )
            lbl <- glabel(
                paste(
                    "This feature shows editable code boxes for the main plot,",
                    "as well as inference and summary information windows,",
                    "\nand some other components of iNZight. The code shown can",
                    "be modified by the user and run, allowing users",
                    "\nto get a basic feel for interfacing with code. In most cases,",
                    "changes to the code will be reflected in the",
                    "\ninterface (where it is possible to do so)."
                ),
                container = g_dev,
                anchor = c(-1, 0)
            )
            font(lbl) <- list(size = 9)

            gseparator(container = g_dev)

            ### ---------------- Code widgets
            p_multiple_x <- gcheckbox("Enable multiple response variable interface",
                checked = prefs$multiple_x,
                container = g_dev,
                handler = function(h, ...) set_pref("multiple_x", svalue(h$obj))
            )
            lbl <- glabel(
                paste(
                    "Enabling this option will replace the Variable 1 drop-down",
                    "box with a multiple-label drop area. You can drag-and-drop",
                    "\none or more variables here which will all be used as",
                    "primary outcome variables. This is useful if you have multiple",
                    "\nresponse type data, or variables with the same values",
                    "you want to be able to compare in a single plot."
                ),
                container = g_dev,
                anchor = c(-1, 0)
            )
            font(lbl) <- list(size = 9)


            ################ BUTTONS
            g_buttons <- ggroup(container = g_main)
            addSpring(g_buttons)

            cancelBtn <<- gbutton("Exit without saving",
                container = g_buttons,
                handler = function(h, ...) dispose(GUI$modWin)
            )

            saveBtn <<- gbutton("Save changes",
                container = g_buttons,
                handler = function(h, ...) {
                    GUI$preferences <<- prefs
                    GUI$savePreferences()
                    dispose(GUI$modWin)

                    confmsg <- paste(
                        sep = "\n",
                        "Some changes require reloading iNZight. Do that now?",
                        "All changes will be saved."
                    )
                    if (!interactive() || gconfirm(confmsg,
                        icon = "question", parent = GUI$win
                    )) {
                        GUI$reload()
                    }
                }
            )
            enabled(saveBtn) <<- FALSE

            svalue(sections) <<- 1L
            on.exit(visible(GUI$modWin) <<- TRUE)
        },
        set_pref = function(name, value) {
            prefs[[name]] <<- value
            enabled(saveBtn) <<- !identical(prefs, curprefs)
        }
    )
)
