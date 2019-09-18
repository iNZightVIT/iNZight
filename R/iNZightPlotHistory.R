history_item <- function(name, code, img, plot, id) {
  list(
    name = name,
    code = code,
    img_file = img,
    id = id,
    plot = plot
  )
}

plot_entry <- function(item, window, i) {
  plot_group <- glayout(expand = TRUE, fill = TRUE)
  plot_image <- gimage(item$img_file)
  addHandlerClicked(plot_image, function(h, ...) {
    print(item$plot)
  })
  
  plot_group[1:2, 1] <- plot_image
  plot_group[1:2, 2, fill = "x", expand = TRUE, anchor = c(-1, 0)] <- glabel(item$name)
  plot_group[1:2, 3:9, fill = "x", expand = TRUE] <- gtext(item$code)
  plot_group[1, 10] <- gbutton("Copy", handler = function(h, ...) {
    tryCatch({
      clipr::write_clip(item$code)
      gmessage("Successfully copied to clipboard", parent = window)
    }, error = function(e) gmessage(e, icon = "error", parent = window))
  })
  plot_group[2, 10] <- gbutton("Delete", handler = function(h, ...) {
    
  })
  
  plot_group
}

iNZplothistory <- setRefClass(
  "iNZplothistory",
  fields = list(
    GUI = "ANY",
    history = "list",
    i = "numeric"
  ),
  methods = list(
    initialize = function(gui) {
      initFields(GUI = gui, history = list(), i = 0L)
    },
    add = function(plot) {
      i <<- i + 1
      
      class(plot) <- c("gg", "ggplot")
      
      tryCatch({
        ggplot2::ggsave(
          paste0("plot", i, ".png"), 
          plot + ggplot2::theme_void() + ggplot2::theme(legend.position="none", title = ggplot2::element_blank()), 
          width = 1.5, 
          height = 1.5, 
          dpi = 50
        )
      })
      
      new_item <- history_item(
        name = paste0("Plot ", i),
        code = attr(plot, "code"),
        plot = plot,
        img = paste0("plot", i, ".png"),
        id = i
      )
      
      
      history[[i]] <<- new_item
    },
    show = function() {
      w <- gwindow(width = 700, height = 300, parent = GUI$win)
      g <- gvbox(expand = TRUE, fill = "x")
      plot_list <- gvbox(use.scrollwindow = TRUE, expand = TRUE, fill = "xy")
      
      gWidgets2::add(w, g)
      gWidgets2::add(g, glabel("The following is a list of the plots you have stored"))
      gWidgets2::add(g, plot_list, expand = TRUE)
      gWidgets2::add(g, gbutton("OK", handler = function(h, ...) {
        GUI$updatePlot()
        dispose(w)
      }))
      
      plot_items <- lapply(history, plot_entry, window = w)
      
      invisible(lapply(plot_items, gWidgets2::add, obj = plot_list, expand = TRUE, fill = "x"))
    }
  )
)
