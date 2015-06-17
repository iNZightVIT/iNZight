iNZSurveyDesign <- setRefClass("iNZSurveyDesign",
                               fields = list(
                                   GUI = "ANY",
                                   designWin = "ANY"
                                   ),
                               methods = list(
                                   initialize = function(GUI) {
                                       initFields(GUI = GUI)

                                       if (is.null(GUI$getActiveData())) {
                                           gerror("Please import a data set first.",
                                                    title = "No data set", icon = "error")
                                           return()
                                       } else if ((names(GUI$getActiveData())[1] == "empty")) {
                                           gmessage("Please import a data set first.",
                                                    title = "No data set", icon = "error")
                                           return()
                                       }

                                       designWin <<- gwindow("Specify survey design", parent = GUI$win,
                                                            width = 450, height = 300, visible = FALSE)
                                       gg <- gvbox(container = designWin, expand = TRUE)
                                       gg$set_borderwidth(5)
                                       
                                       ttl <- glabel("Specify Survey Design", cont = gg)
                                       font(ttl) <- list(weight = "bold", size = 11)
                                       
                                       addSpace(gg, 5)
                                       tbl <- glayout(cont = gg)
                                       
                                       vars <- c("", colnames(GUI$getActiveData()))
                                       
                                       ii <- 2
                                       lbl <- glabel("Strata variable: ")
                                       tbl[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
                                       stratVar <- gcombobox(vars)
                                       tbl[ii, 2, expand = TRUE] <- stratVar
                                       
                                       ii <- ii + 1
                                       lbl <- glabel("1st stage clustering variable: ")
                                       tbl[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
                                       clus1Var <- gcombobox(vars)
                                       tbl[ii, 2, expand = TRUE] <- clus1Var
                                       
                                       ii <- ii + 1
                                       lbl <- glabel("2nd stage clustering variable: ")
                                       tbl[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
                                       clus2Var <- gcombobox(vars)
                                       tbl[ii, 2, expand = TRUE] <- clus2Var

                                       ii <- ii + 1
                                       nestChk <- gcheckbox("Use nested sampling")
                                       tbl[ii, 2, expand = TRUE] <- nestChk
                                       
                                       ii <- ii + 2
                                       lbl <- glabel("Weighting variable: ")
                                       tbl[ii, 1, expand = TRUE, fill = FALSE, anchor = c(1, 0)] <- lbl
                                       wtVar <- gcombobox(vars)
                                       tbl[ii, 2, expand = TRUE] <- wtVar

                                       
                                       
                                       addSpring(gg)
                                       
                                       btnGrp <- ggroup(cont = gg)
                                       addSpring(btnGrp)
                                       cancelBtn <- gbutton("Cancel", cont = btnGrp)
                                       addSpace(btnGrp, 10)
                                       createBtn <- gbutton("OK", cont = btnGrp)
                                       addSpace(btnGrp, 10)
                                       
                                       addSpace(gg, 10)


                                       addHandlerClicked(cancelBtn, handler = function(h, ...) {
                                           dispose(designWin)
                                       })
                                       addHandlerClicked(createBtn, handler = function(h, ...) {
                                           strat <- svalue(stratVar, index = FALSE)
                                           clus1 <- svalue(clus1Var, index = FALSE)
                                           clus2 <- svalue(clus2Var, index = FALSE)
                                           wts <- svalue(wtVar, index = FALSE)
                                           nest <- as.character(svalue(nestChk))

                                           if (strat == "") strat <- NULL
                                           if (clus1 == "") clus1 <- NULL
                                           if (clus2 == "") clus2 <- NULL
                                           if (wts == "") wts <- NULL

                                           GUI$getActiveDoc()$getModel()$setDesign(
                                               strat, clus1, clus2, wts, nest
                                               )

                                           dispose(designWin)
                                       })

                                       visible(designWin) <<- TRUE
                                   }
                                   )
                               )
