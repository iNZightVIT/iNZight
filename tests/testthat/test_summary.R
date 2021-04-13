context("Get Summary window")

skip_on_cran()

# try(ui$close(), TRUE); devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui()
on.exit(gWidgets2::dispose(ui$win))

ui$setDocument(iNZDocument$new(data = census.at.school.500), reset = TRUE)
if (!interactive()) Sys.sleep(5)

test_that("Get summary window opens", {
    ui$ctrlWidget$V1box$set_value("height")
    sw <- iNZGetSummary$new(ui)
    on.exit(gWidgets2::dispose(sw$win))
    expect_is(sw, "iNZGetSummary")
})


test_that("Buttons for linear regression give correct predicted values", {
    ui$ctrlWidget$V2box$set_value("armspan")
    sw <- iNZGetSummary$new(ui)
    on.exit(gWidgets2::dispose(sw$win))

    expect_is(sw$predBtn, "GButton")
    expect_is(sw$residBtn, "GButton")
    expect_false(enabled(sw$predBtn))
    expect_false(enabled(sw$residBtn))

    svalue(sw$trend_menu$menu_list$linear) <- TRUE
    expect_true(enabled(sw$predBtn))
    expect_true(enabled(sw$residBtn))

    store <- sw$store_values("predict")
    expect_equal(svalue(store$children[[1]]$children[[2]]$children[[2]]), "height.predict")
    expect_silent(store$children[[1]]$children[[3]]$invoke_change_handler())

    expect_equal(
        ui$getActiveData()$height.predict,
        as.numeric(
            predict(
                lm(height ~ armspan, data = census.at.school.500, na.action = na.exclude)
            )
        )
    )
})

if (interactive()) {
    # try(ui$close(), TRUE); devtools::load_all()
    ui <- iNZGUI$new()
    ui$initializeGui(census.at.school.500)
    on.exit(ui$close())
}

test_that("Summary function call can be modified", {
    ui$ctrlWidget$V1box$set_value("height")
    ui$ctrlWidget$V2box$set_index(1L)
    sw <- iNZGetSummary$new(ui)
    on.exit(gWidgets2::dispose(sw$win))

    expect_equal(svalue(sw$code_box), "inzsummary(~height, data = data)\n")
    sw$set_input("inzsummary(~armspan, data = data)")
    expect_silent(sw$run_btn$invoke_change_handler())
    expect_match(svalue(sw$info_text), "Summary of armspan")
})


data(api, package = "survey")
ui$setDocument(iNZDocument$new(data = apiclus1), reset = TRUE)
if (!interactive()) Sys.sleep(2)
ui$getActiveDoc()$getModel()$setDesign(
    list(
        clus1 = "dnum", clus2 = "snum", weights = "pw", nest = FALSE,
        fpc = NULL, type = "survey"
    ),
    gui = ui
)

test_that("Get summary works for survey design", {
    ui$ctrlWidget$V1box$set_value("api00")
    sw <- iNZGetSummary$new(ui)
    on.exit(gWidgets2::dispose(sw$win))
    expect_is(sw, "iNZGetSummary")
})


# ## This is what will need to eventually happen:
# myenv <- new.env()
# myenv$.data <- apiclus1
# myenv$.design <- ui$getActiveDoc()$getModel()$createSurveyObject()
# # call <- expression(inzplot(~api00, design = !!.design))
# call <- construct_call(
#     ui$getActiveDoc()$getSettings(),
#     ui$getActiveDoc()$getModel(),
#     design = quote(!!data.svy),
#     what = "summary")
# eval(call, env)
