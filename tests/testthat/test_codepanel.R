context("Code Panel")

skip_on_cran()

# load_all("../iNZightPlots")

# try(ui$close()); load_all()
ui <- iNZGUI$new()
ui$initializeGui(census.at.school.500)
on.exit(gWidgets2::dispose(ui$win))
Sys.sleep(2)


test_that("Shows correct code", {
    expect_equal(ui$code_panel$input$get_value(), "")

    ui$ctrlWidget$V1box$set_value("height")
    expect_equal(
        ui$code_panel$input$get_value(),
        "inzplot(~height, data = census.at.school.500)\n"
    )

    ui$ctrlWidget$V2box$set_value("gender")
    expect_equal(
        ui$code_panel$input$get_value(),
        "inzplot(height ~ gender, data = census.at.school.500)\n"
    )

    ui$ctrlWidget$G1box$set_value("travel")
    expect_equal(
        ui$code_panel$input$get_value(),
        "inzplot(height ~ gender | travel, data = census.at.school.500)\n"
    )
})

test_that("Modifying code updates GUI", {
    ui$code_panel$input$set_value("inzplot(height ~ gender, data = census.at.school.500)\n")
    expect_silent(ui$code_panel$run_code())
    expect_equal(ui$ctrlWidget$G1box$get_index(), 1L)

    ui$code_panel$input$set_value("inzplot(height ~ armspan, data = census.at.school.500)\n")
    expect_silent(ui$code_panel$run_code())
    expect_equal(ui$ctrlWidget$V2box$get_value(), "armspan")

    ui$code_panel$input$set_value("inzplot(~armspan, data = census.at.school.500)\n")
    expect_silent(ui$code_panel$run_code())
    expect_equal(ui$ctrlWidget$V2box$get_index(), 1L)
})
