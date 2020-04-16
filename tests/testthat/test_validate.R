context("Validation menu")

ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

ui$setDocument(iNZDocument$new(data = census.at.school.500), reset = TRUE)

test.rules <- "age > 0\nage < 18"
temp.file <- tempfile()

test.results <- structure(
  list(
    Rule = structure(1:2, .Label = c("age < 18", "age > 0"), class = "factor"),
    Total = c(500L, 500L),
    Passes = c(497L, 499L),
    Fails = c(2L, 0L),
    `Fails (%)` = c("0.40%", "0.00%"),
    ..visible = c(TRUE, TRUE)
  ),
  class = "data.frame",
  row.names = 2:1
)

test_that("rules validate on active dataset", {
  val <- iNZValidateWin$new(ui)
  left.side <- val$window$children[[1]]$children[[1]]

  on.exit(left.side$children[[7]]$invoke_change_handler())

  svalue(left.side$children[[3]]) <- test.rules

  left.side$children[[4]]$children[[3]]$invoke_change_handler()

  expect_equal(
    test.results,
    left.side$children[[6]]$items
  )
})

test_that("rule files save", {
  val <- iNZValidateWin$new(ui)
  left.side <- val$window$children[[1]]$children[[1]]

  on.exit(left.side$children[[7]]$invoke_change_handler())

  val$save.file(temp.file, test.rules)

  expect_equal(
    test.rules,
    paste0(grep("^#", readLines(temp.file), value = TRUE, invert = TRUE), collapse = "\n")
  )
})

test_that("rule files open", {
  val <- iNZValidateWin$new(ui)
  left.side <- val$window$children[[1]]$children[[1]]

  on.exit(left.side$children[[7]]$invoke_change_handler())

  val$open.file(temp.file, left.side$children[[3]])

  expect_equal(
    test.rules,
    svalue(left.side$children[[3]])
  )
})
