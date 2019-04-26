context("Validation menu")

ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

ui$setDocument(iNZDocument$new(data = census.at.school.500), reset = TRUE)

test_that("rule files open", {
  rule.file <- "test_rules.txt"
  val <- iNZValidateWin$new(ui)
  left.side <- val$window$children[[1]]$children[[1]]
  val$open.file(rule.file, left.side$children[[3]])
  
  expect_equal(
    paste0(grep("^#", readLines(rule.file), value = TRUE, invert = TRUE), collapse = "\n"),
    svalue(left.side$children[[3]])
  )
})

