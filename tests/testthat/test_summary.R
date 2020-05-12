context("Get Summary window")

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

data(api, package = "survey")
ui$setDocument(iNZDocument$new(data = apiclus1), reset = TRUE)
if (!interactive()) Sys.sleep(2)
ui$getActiveDoc()$getModel()$setDesign(
    clus1 = "dnum", clus2 = "snum", wt = "pw", nest = FALSE,
    fpc = NULL, type = "survey", gui = ui
)

# test_that("Get summary works for survey design", {
#     ui$ctrlWidget$V1box$set_value("api00")
#     sw <- iNZGetSummary$new(ui)
#     on.exit(gWidgets2::dispose(sw$win))
#     expect_is(sw, "iNZGetSummary")
# })


## This is what will need to eventually happen:
# myenv <- new.env()
# myenv$data <- apiclus1
# myenv$design <- ui$getActiveDoc()$getModel()$createSurveyObject()
# call <- expression(iNZPlot(api00, design = !!design))
# eval(call, myenv)
