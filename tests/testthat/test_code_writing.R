# context("Code writing")

# ui <- iNZGUI$new()
# ui$initializeGui()
# on.exit(gWidgets2::dispose(ui$win))
# cas <- census.at.school.500
# attr(cas, "code") <- "read_csv('cas.csv')"
# ui$setDocument(iNZDocument$new(data = cas), reset = TRUE)
# dat <- iNZightTools::filterLevels(
#     ui$getActiveData(),
#     "travel",
#     c("bus", "bike")
# )
# attr(dat, "code") <-
#     gsub("ui\\$getActiveData\\(\\)", "data.filtered", attr(dat, "code"))
# ui$setDocument(iNZDocument$new(data = dat), reset = FALSE)

# ui$rhistory$get()
