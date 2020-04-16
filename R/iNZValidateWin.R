
iNZValidateWin <- setRefClass(
  "iNZValidateWin",
  fields = list(
    GUI    = "ANY",
    window = "ANY",
    vali   = "ANY",
    cf     = "ANY"
  ),
  methods = list(
    initialize = function(GUI) {
      initFields(GUI = GUI)
      open.window()
    },
    open.file = function(file, rules.box) {
      print(rules.box)
      file.vali <- validate::validator(.file = file)
      svalue(rules.box) <- sub("^ V[0-9]+: ", "", capture.output(print(file.vali))[-1])
    },
    save.file = function(file, rules) {
      write(rules, file = file)
    },
    open.window = function() {
      window <<- gwindow("Validate Data", width = 800, height = 500, parent = GUI$win, visible = FALSE)

      gv <- ggroup()
      gv$set_borderwidth(15)

      add(window, gv, expand = TRUE, fill = TRUE)

      group.left <- gvbox()
      group.right <- gvbox()

      add(gv, group.left, expand = TRUE, fill = TRUE)
      add(gv, group.right, expand = TRUE, fill = TRUE)

      details.default <- "Double click on the results of a \nvalidation rule in the bottom-left \ntable to display a detailed summary \nhere."

      details.box <- gtext(text = details.default, wrap = FALSE)
      font(details.box) <- c(family = "monospace")

      rules.box <- gtext(text = "", font.attr = list(family = "monospace"))
      results.box <- gtable(data.frame())

      lbl <- glabel("Validate Dataset")
      lbl.rulesbox <- glabel("Validation Rules:")
      lbl.results <- glabel("Results:")
      lbl.details <- glabel("Details:")

      font(lbl) <- list(weight = "bold", size = 12, family = "sans")
      font(lbl.rulesbox) <- list(weight = "bold")
      font(lbl.results) <- list(weight = "bold")
      font(lbl.details) <- list(weight = "bold")
      helpbtn <- gimagebutton(stock.id = "gw-help", handler = function(h, ...){
        browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/data_options/#validate")
      })
      titlelyt <- glayout(homegenous = FALSE)
      titlelyt[1, 1:19, expand = TRUE] <- lbl.details
      titlelyt[1, 20, expand = TRUE, anchor = c(1, -1)] <- helpbtn

      group.identifier <- ggroup()
      add(group.identifier, glabel("Unique Identifier: "))
      dropdown.identifier <- gcombobox(c("Row Number", colnames(GUI$getActiveData())), container = group.identifier, expand = TRUE)

      open.button <- gbutton("Open Rules", handler = function(h, ...) {
        open.dialog <- gfile("Open Rules...", type = "open")
        open.file(open.dialog, rules.box)
      })

      save.button <- gbutton("Save Rules", handler = function(h, ...) {
        save.dialog <- gfile("Save Rules...", type = "save", initial.filename = paste0(attr(GUI$getActiveData(), "name", exact = TRUE), "_rules.txt"))
        save.file(save.dialog, svalue(rules.box))
      })

      validate.button <- gbutton("Validate Dataset", handler = function(h, ...) {
        rules <- unlist(strsplit(svalue(rules.box), "\\n"))
        rules <- rules[rules != ""]
        has.labels <- grepl("^.*:.*", rules)
        labels <- paste0("V", 1:length(rules))
        labels[has.labels] <- unlist(lapply(strsplit(rules[has.labels], ":"), `[[`, 1))

        tryCatch({
          rules.df <- data.frame(
            name = labels,
            rule = gsub("^.+:", "", rules),
            stringsAsFactors = TRUE
          )
          vali <<- validate::validator(.data = rules.df)
          cf <<- validate::confront(GUI$getActiveData(), vali)

          results.df <- iNZightTools::validation_summary(cf)

          results.df <- results.df[order(results.df[["Fails"]] / results.df[["Total"]], decreasing = TRUE), ]

          results.box[] <- results.df

          svalue(details.box) <- details.default
          font(details.box) <- c(family = "monospace")
        },
        error = function(e) {
          error.message <- sprintf("Error reading validation rules: \n  %s", e)
          if (grepl("is not subsettable", e)) {
            error.message <- paste0(error.message, "\n\nThis error is most often caused when you have a line with only a variable name.")
          } else if (grepl("unexpected end of input", e)) {
            error.message <- paste0(error.message, "\n\nThis error is most often caused when you forget to add the comparison variable name after the comparison operator.")
          } else if (grepl("unexpected symbol", e)) {
            error.message <- paste0(error.message, "\n\nThis error is most often caused when you try to put two rules on a single line.")
          }

          gmessage(error.message, title = "Validation Rules Error", icon = "error", parent = window)
        })
      })

      open.button$set_icon("open")
      save.button$set_icon("save")
      validate.button$set_icon("apply")

      ok.button <- gbutton("OK", handler = function(h, ...) {
        dispose(window)
      })

      update.details <- function(h, ...) {
        i <- which(svalue(results.box) == validate::as.data.frame(vali, stringsAsFactors = TRUE)[["rule"]])

        if (svalue(dropdown.identifier, TRUE) == 1) {
          id.var <- NA
        } else {
          id.var <- svalue(dropdown.identifier)
        }

        svalue(details.box) <- iNZightTools::validation_details(cf, vali, paste0("V", i), id.var, GUI$getActiveData())
        font(details.box) <- c(family = "monospace")
      }

      addHandlerChanged(results.box, update.details)
      addHandlerChanged(dropdown.identifier, update.details)

      group.leftbuttons <- ggroup()
      add(group.leftbuttons, open.button, expand = TRUE, fill = "x")
      add(group.leftbuttons, save.button, expand = TRUE, fill = "x")
      add(group.leftbuttons, validate.button, expand = TRUE, fill = "x")

      add(group.left, lbl)
      # add(group.left, open.button)
      add(group.left, lbl.rulesbox, anchor = c(-1, 0))
      add(group.left, rules.box, expand = TRUE, fill = TRUE)
      add(group.left, group.leftbuttons)
      add(group.left, lbl.results, anchor = c(-1, 0))
      add(group.left, results.box, expand = TRUE, fill = TRUE)
      # add(group.left, save.button)

      add(group.right, titlelyt)
      add(group.right, group.identifier)
      add(group.right, details.box, expand = TRUE, fill = TRUE)
      add(group.left, ok.button, anchor = c(1, 0))

      visible(window) <<- TRUE
    }
  )
)
