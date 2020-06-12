context("Dates and times")

skip_if(packageVersion("iNZightTS") < numeric_version("1.5.3"))

# try(ui$close(), TRUE); load_all()
ui <- iNZGUI$new()
ui$initializeGui(
    iNZightTS:::covid %>%
        dplyr::select(c("Date", "Total_Deaths", "Daily_Deaths")) %>%
        dplyr::mutate(Date = as.Date(Date))
)

test_that("Aggregate dates to daily (7 day week)", {

})
