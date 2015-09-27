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


                                       gmessage("IMPORTANT: The Survey functionality in iNZight is still being developed (i.e., it is in BETA), and is provided here for any curious users to explore. The inference information has only been recently implemented for some features, but shouldn't be relied on yet (we need to check everything for possible flaws). If you do notice any obvious bugs (i.e., something happens, but it looks wrong) let us know! However, many features aren't implemented and you may simply see nothing happen - this is not a bug. \n\nFinally, if you do any data- or variable-manipulation (filter, add/remove variables, etc) you will need to update the Survey object by opening up this window again and clicking 'OK' (without modifying anything) to update the design object.",
                                              title = "BETA Software - Use with Caution!", parent = GUI$win, icon = "warning")

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

                                       ii <- ii + 1
                                       lbl <- glabel("Finite population correction: ")
                                       tbl[ii, 1, expand = TRUE, fill = FALSE, anchor= c(1, 0)] <- lbl
                                       fpcVar <- gcombobox(vars, editable = TRUE)
                                       tbl[ii, 2, expand = TRUE] <- fpcVar

                                       
                                       
                                       addSpring(gg)
                                       
                                       btnGrp <- ggroup(cont = gg)
                                       addSpace(btnGrp, 10)
                                       #advancedBtn <- gbutton("Advanced", cont = btnGrp)
                                       addSpring(btnGrp)
                                       cancelBtn <- gbutton("Cancel", cont = btnGrp)
                                       addSpace(btnGrp, 10)
                                       createBtn <- gbutton("OK", cont = btnGrp)
                                       addSpace(btnGrp, 10)
                                       
                                       addSpace(gg, 10)

                                       #addHandlerClicked(advancedBtn, handler = function(h, ...) {
                                       #})
                                       addHandlerClicked(cancelBtn, handler = function(h, ...) {
                                           dispose(designWin)
                                       })
                                       addHandlerClicked(createBtn, handler = function(h, ...) {
                                           strat <- svalue(stratVar, index = FALSE)
                                           clus1 <- svalue(clus1Var, index = FALSE)
                                           clus2 <- svalue(clus2Var, index = FALSE)
                                           wts <- svalue(wtVar, index = FALSE)
                                           fpc <- svalue(fpcVar, index = FALSE)
                                           nest <- as.character(svalue(nestChk))

                                           if (strat == "") strat <- NULL
                                           if (clus1 == "") clus1 <- NULL
                                           if (clus2 == "") clus2 <- NULL
                                           if (wts == "") wts <- NULL
                                           if (fpc == "") fpc <- NULL

                                           GUI$getActiveDoc()$getModel()$setDesign(
                                               strat, clus1, clus2, wts, nest, fpc, gui = GUI
                                               )

                                           setOK <- try(GUI$getActiveDoc()$getModel()$createSurveyObject(), TRUE)
                                           
                                           if (!inherits(setOK, "try-error")) {
                                               if (is.null(strat) & is.null(clus1) &
                                                   is.null(clus2) & is.null(wts) & is.null(fpc)) {
                                                   ## ENABLE A WHOLE LOT OF STUFF
                                                   enabled(GUI$menubar$menu_list[["Dataset"]][[3]]) <<- TRUE
                                                   enabled(GUI$menubar$menu_list[["Variables"]][["Numeric Variables"]][[2]]) <<- TRUE
                                                   enabled(GUI$menubar$menu_list[["Plot"]][[3]]) <<- TRUE
                                                   #enabled(GUI$sumBtn) <<- TRUE
                                                   enabled(GUI$infBtn) <<- TRUE
                                               } else {
                                                   ## DISABLE A WHOLE LOT OF STUFF
                                                   enabled(GUI$menubar$menu_list[["Dataset"]][[3]]) <<- FALSE
                                                   enabled(GUI$menubar$menu_list[["Variables"]][["Numeric Variables"]][[2]]) <<- FALSE
                                                   enabled(GUI$menubar$menu_list[["Plot"]][[3]]) <<- FALSE
                                                   #enabled(GUI$sumBtn) <<- FALSE
                                                   enabled(GUI$infBtn) <<- FALSE

                                                   dispose(designWin)
                                               }
                                           } else {
                                               gmessage(paste0(
                                                   "There is a problem with the specification of the survey design:\n\n",
                                                   setOK
                                                   ), icon = "error")
                                           }                                           
                                       })


                                       ## Populate the lists:
                                       curDes <- GUI$getActiveDoc()$getModel()$getDesign()
                                       if (!is.null(curDes)) {
                                           if (!is.null(curDes$strata))
                                               svalue(stratVar) <- curDes$strata
                                           if (!is.null(curDes$clus1))
                                               svalue(clus1Var) <- curDes$clus1
                                           if (!is.null(curDes$clus2))
                                               svalue(clus2Var) <- curDes$clus2
                                           if (!is.null(curDes$nest))
                                               svalue(nestChk) <- curDes$nest
                                           if (!is.null(curDes$wt))
                                               svalue(wtVar) <- curDes$wt
                                           if (!is.null(curDes$fpc))
                                               svalue(fpcVar) <- curDes$fpc
                                       }

                                       visible(designWin) <<- TRUE
                                   }
                                   )
                               )
