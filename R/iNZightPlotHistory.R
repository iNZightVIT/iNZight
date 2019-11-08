history_item <- function(name, code, img, expr, data_name, id) {
  list(
    name = name,
    code = code,
    img_file = img,
    id = id,
    expr = expr,
    data_name = data_name
  )
}

with_packages <- function(pkg, expr) {
  if (length(pkg) == 1) {
    rlang::expr(
      withr::with_package(!!pkg, eval(!!expr))
    )
  } else {
    rlang::expr(
      withr::with_package(!!pkg[1], !!with_packages(pkg[-1], expr))
    )
  }
}

iNZplothistory <- setRefClass(
  "iNZplothistory",
  fields = list(
    GUI = "ANY",
    history = "list",
    i = "numeric",
    temp.dir = "ANY",
    plot_list = "ANY",
    code_box = "ANY",
    copy_chunk = "logical"
  ),
  methods = list(
    initialize = function(gui) {
      initFields(
        GUI = gui, 
        history = list(), 
        i = 0L, 
        temp.dir = tempdir(),
        copy_chunk = FALSE
      )
    },
    add = function(plot, code = NULL, module = NULL) {
      i <<- i + 1
      
      class(plot) <- c("gg", "ggplot")
      
      if (!is.null(attr(plot, "plottype")) && attr(plot, "plottype") == "gg_gridplot") {
        tryCatch({
          ggplot2::ggsave(
            file.path(temp.dir, sprintf("plot%d.png", i)),
            waffle::waffle(c(a = 3, b = 1), rows = 1) + ggplot2::theme_void(), 
            width = 1.5, 
            height = 1.5, 
            dpi = 50
          )
        })
      } else {
        tryCatch({
          ggplot2::ggsave(
            file.path(temp.dir, sprintf("plot%d.png", i)),
            plot + ggplot2::theme_void() + ggplot2::theme(legend.position="none", title = ggplot2::element_blank()), 
            width = 1.5, 
            height = 1.5, 
            dpi = 50
          )
        })
      }
      
      if (is.null(attr(plot, "plottype")) || !grepl("^gg", attr(plot, "plottype"))) {
        if (is.null(code)) {
          attr(plot, "code_expr") <- rlang::parse_expr(paste(attr(plot, "code")[-1], collapse = " "))
          attr(plot, "data_name") <- "map.data"
        } else {
          attr(plot, "code") <- code
          attr(plot, "code_expr") <- rlang::parse_expr(paste(code[-1], collapse = " "))
          attr(plot, "data_name") <- "region.data"
        }
      }
      
      new_item <- history_item(
        name = paste0("Plot ", i),
        code = paste0(attr(plot, "code"), collapse = "\n"),
        expr = attr(plot, "code_expr"),
        data_name = attr(plot, "data_name"),
        img = file.path(temp.dir, sprintf("plot%d.png", i)),
        id = i
      )
      
      
      history[[as.character(i)]] <<- new_item
    },
    show = function() {
      w <- gwindow(width = 700, height = 300, parent = GUI$win)
      g <- gvbox(expand = TRUE, fill = "x")
      plot_list <<- gvbox(use.scrollwindow = TRUE, expand = TRUE, fill = "xy")
      code_group <- gexpandgroup("Run Code", horizontal = FALSE)
      visible(code_group) <- FALSE
      code_group_horizontal <- ggroup()
      code_box <<- gtext("# Copy and paste R code into this text box")
      as_chunk <- gcheckbox("Wrap copied code in Rmarkdown code chunk")
      addHandlerChanged(as_chunk, function(h, ...) {
        copy_chunk <<- svalue(as_chunk)
      })
      
      gWidgets2::add(w, g)
      gWidgets2::add(g, glabel("The following is a list of the plots you have stored"))
      gWidgets2::add(g, as_chunk)
      gWidgets2::add(g, plot_list, expand = TRUE)
      gWidgets2::add(g, code_group)
      gWidgets2::add(code_group_horizontal, code_box, expand = TRUE)
      gWidgets2::add(code_group_horizontal, gbutton("Run Code", handler = function(h, ...) submitCode()))
      gWidgets2::add(code_group, code_group_horizontal)
      gWidgets2::add(g, gbutton("OK", handler = function(h, ...) {
        GUI$updatePlot()
        dispose(w)
      }))
      if (length(history) > 0) {
        plot_items <- lapply(names(history), function(i) plot_entry(history[[i]], window = w, i = i))
      
        invisible(lapply(plot_items, gWidgets2::add, obj = plot_list, expand = TRUE, fill = "x"))
      } else {
        gWidgets2::add(plot_list, glabel("You haven't stored any plots yet - click the \"Store Code\" button in the plotting menu to keep a list \nof the plots you'd like the R code for"), anchor = c(0, 0), expand = TRUE, fill = TRUE)
      }
    },
    plot_entry = function(item, window, i) {
      plot_group <- glayout(expand = TRUE, fill = TRUE)
      plot_image <- gimage(item$img_file)
      addHandlerClicked(plot_image, function(h, ...) {
        # if (is.null(module)) {
          eval_env <- rlang::env(!!rlang::sym(attr(GUI$getActiveData(), "name")) := GUI$getActiveData())
        # } else {
        #   eval_env <- rlang::env(region.data := module$combinedData[['region.data']])
        # }
        eval_results <- lapply(item$expr, eval, envir = eval_env)
        # print(eval_results)
        print(eval_results[[length(eval_results)]])
      })
      
      # old_cursor <- getToolkitWidget(plot_image)$getWindow()$getCursor()
      hover <- gdkCursorNew("GDK_HAND1")
      
      addHandler(plot_image, "enter-notify-event", handler=function(h,...) {
        getToolkitWidget(plot_image)$getWindow()$setCursor(hover)
        TRUE
      })
      
      
      addHandler(plot_image, "leave-notify-event", handler=function(h,...) {
        getToolkitWidget(plot_image)$getWindow()$setCursor(gdkCursorNew("GDK_LEFT_PTR"))
        TRUE
      })
      
      plot_group[1:2, 1] <- plot_image
      plot_group[1:2, 2, fill = "x", anchor = c(-1, 0)] <- gedit(item$name, handler = function(h, ...) {
        history[[i]]$name <<- svalue(h$obj)
      })
      plot_group[1:2, 3:9, fill = "x", expand = TRUE] <- gtext(item$code)
      plot_group[1, 10] <- gbutton("Copy", handler = function(h, ...) {
        tryCatch({
          if (copy_chunk) {
            clipr::write_clip(paste("```{r}", item$code, "```", sep = "\n"))
          } else {
            clipr::write_clip(item$code)
          }
          gmessage("Successfully copied to clipboard", parent = window)
        }, error = function(e) gmessage(e, icon = "error", parent = window))
      })
      plot_group[2, 10] <- gbutton("Delete", handler = function(h, ...) {
        history[[i]] <<- NULL
        delete(plot_list, plot_group)
        if (length(history) == 0) {
          gWidgets2::add(plot_list, glabel("You haven't stored any plots yet - click the \"Store Code\" button in the plotting menu to keep a list \nof the plots you'd like the R code for"), anchor = c(0, 0), expand = TRUE, fill = TRUE)
        }
      })
      
      plot_group
    },
    submitCode = function() {
      find_libraries <- function(expr) {
        which_libraries <- gregexpr("library\\(([-_A-z0-9.])+\\)", expr)
        lines_containing_library <- unlist(lapply(which_libraries, function(x) x > 0))
        libraries <- unlist(lapply(regmatches(expr, which_libraries), function(x) if (length(x) > 0 && x[1] == "library") x[2]))
        new_expr <- expr[!lines_containing_library]
        
        list(
          expr = new_expr,
          libraries = libraries
        )
      }
      
      code_text <- parse(text = svalue(code_box))
      parsed <- find_libraries(code_text)
      
      # if (is.null(module)) {
        eval_env <- rlang::env(!!rlang::sym(attr(GUI$getActiveData(), "name")) := GUI$getActiveData())
      # } else {
      #   eval_env <- rlang::env(region.data := module$combinedData[['region.data']])
      # }
      
      if (length(parsed$libraries) > 0) {
        code_text <- with_packages(parsed$libraries, parsed$expr)
      }
      
      eval_results <- eval(code_text, envir = eval_env)
      print(eval_results)
    }
  )
)
