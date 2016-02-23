## ## iNZ toolbar class
## ##' @importClassesFrom gWidgets2RGtk2 GToolBar
## GToolBariNZ <- setRefClass("GToolBariNZ",
##                            contains = "GToolBar",
##                            methods=list(
##                                initialize=function(toolkit=NULL,
##                                    toolbar.list=list(),
##                                    style = c("both", "icons", "text", "both-horiz"),
##                                    container = NULL,
##                                    ...) {
##                                    widget <<- gtkToolbar()
##                                    widget$setStyle(match.arg(style))

##                                    initFields(block=widget,
##                                               toolbar_list=list()
##                                               )

##                                    add_toolbar_items(toolbar.list)

##                                    if(!is.null(container))
##                                        add_to_parent(container, .self, ...)

##                                    callSuper(toolkit)
##                                })
##                            )

## .gtoolbariNZ.guiWidgetsToolkitRGtk2 <-
##     function(toolkit,
##              toolbar.list=list(),
##              style = c("both","icons","text","both-horiz"),
##              container = NULL,
##              ... ) {
##         GToolBariNZ$new(toolkit,
##                         toolbar.list = toolbar.list, style = style,
##                         container = container, ...)
##     }

## gtoolbariNZ <- function(
##                      toolbar.list=list(),
##                      style = c("both", "icons", "text", "both-horiz"),
##                      container = NULL,
##                      ... ,
##                      toolkit=guiToolkit()){
##   obj <- .gtoolbariNZ(toolkit,
##                       toolbar.list=toolbar.list,
##                       style=match.arg(style),
##                       container=container ,...
##                     )

##   check_return_class(obj, "GToolBariNZ")
##   return(obj)
## }


## ## generic for toolkit dispatch
## ##
## ## @export
## ## @rdname gtoolbar
## .gtoolbariNZ <- function(toolkit,
##                       toolbar.list=list(),
##                       style = c("both", "icons", "text", "both-horiz"),
##                       container = NULL,
##                       ... )
##            UseMethod( '.gtoolbariNZ' )


## ##' add toolbar items to toolbar
## ##'
## ##' A toolbar item is a list of action items or a toolbar instance
## ##' @inheritParams add
## ##' @rdname gtoolbar
## ##' @method add GToolBar
## ##' @export
## add.GToolBariNZ <- function(obj, child, expand=FALSE, fill=NULL, anchor=NULL, ...) {
##     dispatcher <- function(obj, child) UseMethod("dispatcher")
##     dispatcher.GToolBariNZ <- function(child, obj) obj$add_toolbar_items(svalue(child))
##     dispatcher.list <- function(obj, child) obj$add_toolbar_items(child)
##     dispatcher(child, obj)
## }




## ##' "svalue<-" method
## ##'
## ##' for a toolbar, \code{svalue<-} replaces the toolbar items with new ones specified by value.
## ##' @inheritParams svalue
## ##' @usage \method{svalue}{GToolBar} (obj, index=NULL, ...) <- value
## ##' @rdname gtoolbar
## ##' @method svalue<- GToolBar
## ##' @export
## "svalue<-.GToolBariNZ" <- function(obj, index=NULL, ..., value) NextMethod()

