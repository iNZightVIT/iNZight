iNZAboutWidget <- setRefClass(
    "iNZAboutWidget",
    fields = list(
        GUI = "ANY",
        win = "ANY"
    ),
    methods = list(
        initialize = function(gui = list(win = NULL), title = "About iNZight", heading = "iNZight") {
            initFields(GUI = gui)

            window_width <- 500
            win <<- gwindow(title,
                width = window_width,
                height = 600,
                visible = FALSE,
                parent = GUI$win
            )
            g <- gvbox(expand = FALSE, cont = win, spacing = 5)
            g$set_borderwidth(10)

            mainlbl <- glabel(heading, container = g)
            font(mainlbl) <- list(
                weight = "bold",
                family = "sans",
                size = 20
            )
            verlbl <- glabel(
                sprintf(
                    "Version %s - Released %s",
                    packageDescription("iNZight")$Version,
                    format(
                        as.POSIXct(
                            packageDescription("iNZight")$Date
                        ),
                        "%d %B, %Y"
                    )
                ),
                container = g
            )
            font(verlbl) <- list(
                weight = "normal",
                family = "sans",
                size = 10
            )
            rverlbl <- glabel(
                sprintf("Running on R version %s", getRversion()),
                container = g
            )
            font(rverlbl) <- list(
                weight = "normal",
                family = "sans",
                size = 10
            )

            addSpace(g, 10)

            ## sponsors
            sponsorlbl <- glabel("SPONSORS",
                container = g
            )
            font(sponsorlbl) <- list(
                weight = "bold",
                family = "sans",
                size = 10
            )

            g_sponsors <- gvbox(container = g)
            g_sponsors$set_borderwidth(5)

            logo_path <- function(logo) {
                system.file(
                    file.path("sponsors", sprintf("%s_logo50.png", logo)),
                    package = "iNZight"
                )
            }
            sponsors <- list(
                uoa = "https://auckland.ac.nz",
                statsnz = "https://stats.govt.nz",
                minedu = "https://minedu.govt.nz",
                abs = "https://abs.gov.au",
                guinz = "https://growingup.co.nz/",
                terourou = "https://terourou.org",
                inzan = NULL
            )

            max_width <- window_width - 20
            row_width <- 0
            g_row <- ggroup(container = g_sponsors)
            addSpring(g_row)

            for (sponsor in names(sponsors)) {
                url <- sponsors[[sponsor]]
                logo <- logo_path(sponsor)
                if (!file.exists(logo)) next

                img <- png::readPNG(logo)

                if (row_width > 0 && row_width + dim(img)[2] > max_width) {
                    # create a new row
                    addSpring(g_row)
                    addSpace(g, 5)
                    g_row <- ggroup(container = g_sponsors)
                    addSpring(g_row)
                    row_width <- 0
                }

                # add image
                gimagebutton(logo,
                    container = g_row,
                    handler = function(h, ...) {
                        if (!is.null(url)) browseURL(url)
                    }
                )
                addSpring(g_row)

                # add space
                row_width <- row_width + dim(img)[2] + 10
            }

            addSpace(g, 10)

            licenselbl <- glabel("LICENSE",
                container = g
            )
            font(licenselbl) <- list(
                weight = "bold",
                family = "sans",
                size = 10
            )
            addSpace(g, 5)

            gpltxt <- gtext(expand = TRUE, cont = g, wrap = TRUE)
            l1 <- insert(gpltxt,
                paste(
                    "\nThis program is free software; you can redistribute it and/or",
                    "modify it under the terms of the GNU General Public License",
                    "as published by the Free Software Foundation; either version 2",
                    "of the License, or (at your option) any later version.\n"
                ),
                font.attr = list(size = 9)
            )
            l2 <- insert(gpltxt,
                paste(
                    "This program is distributed in the hope that it will be useful,",
                    "but WITHOUT ANY WARRANTY; without even the implied warranty of",
                    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
                    "GNU General Public License for more details.\n"
                ),
                font.attr = list(size = 9)
            )
            l3 <- insert(gpltxt,
                paste(
                    "You should have received a copy of the GNU General Public License",
                    "along with this program; if not, write to the Free Software",
                    "Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.\n"
                ),
                font.attr = list(size = 9)
            )
            l4 <- insert(gpltxt,
                paste(
                    "You can view the full licence here:\nhttp://www.gnu.org/licenses/gpl-2.0-standalone.html"
                ),
                font.attr = list(size = 9)
            )
            addSpace(g, 5)
            contactlbl <- glabel(
                "For help, contact inzight_support@stat.auckland.ac.nz",
                container = g
            )
            font(contactlbl) <- list(
                weight = "normal",
                family = "sans",
                size = 8
            )

            visible(win) <<- TRUE

            # Move to center
            if (is.null(GUI$win)) {
                center_window(win)
            }

            invisible(NULL)
        }
    )
)
