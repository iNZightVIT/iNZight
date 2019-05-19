context("Get Inference window")

# ui$close()
# devtools::load_all("../..")
ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

ui$setDocument(iNZDocument$new(data = census.at.school.500), reset = TRUE)
Sys.sleep(5)

test_that("Get inference window - dot plots", {
    svalue(ui$ctrlWidget$V1box) <- "height"

    iwin <- ui$infBtn$invoke_change_handler()
    expect_is(iwin[[1]], "GWindow")
    chk <- iwin[[1]]$children[[1]]$children[[1]]$children[[4]]
    expect_equal(
        chk$widget$label,
        "One Sample t-test"
    )
    # checking this box specifies hypothesis
    null <- iwin[[1]]$children[[1]]$children[[1]]$children[[6]]
    alt <- iwin[[1]]$children[[1]]$children[[1]]$children[[8]]

    expect_false(enabled(null))
    expect_false(enabled(alt))

    expect_silent(
        svalue(chk) <- TRUE
    )
    expect_true(enabled(null))
    expect_true(enabled(alt))
    expect_is(null, "GEdit")
    expect_is(alt, "GComboBox")
    expect_equal(svalue(null), "0")
    expect_equal(svalue(alt), "two sided")

    svalue(null) <- "100"
    svalue(alt) <- "greater than"

    expect_silent(
        w2 <- iwin[[1]]$children[[1]]$children[[2]]$invoke_change_handler()
    )
    expect_is(w2[[1]], "GWindow")
    expect_is(w2[[1]]$children[[1]], "GText")
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Null Hypothesis: true mean is equal to 100"
    )
    expect_match(
        svalue(w2[[1]]$children[[1]]),
        "Alternative Hypothesis: true mean is greater than 100"
    )
})

