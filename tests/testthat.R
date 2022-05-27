library(testthat)
library(iNZight)

options(readr.show_progress = FALSE)

ui <- iNZight()
oprefs <- ui$preferences
ui$preferences <- ui$defaultPrefs()
ui$savePreferences()
ui$close()

on.exit({
    ui <- iNZight()
    ui$preferences <- oprefs
    ui$savePreferences()
    ui$close()
})

test_check("iNZight")
