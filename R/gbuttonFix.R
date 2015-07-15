GButton$methods(set_icon = function(value, size = "button") {
                    ## Set icon using a stock icon
                    icon <- getStockIconByName(value, toolkit=toolkit)
                    if(!is.null(icon)) {
                        image <- gtkImageNew()
                        image$SetFromStock(icon, size=size)
                        widget$setImage(image)
                        widget$image$show()
                    }
                })
