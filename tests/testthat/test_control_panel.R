# Variable controls always use gcombobox (multiple_x / gmultiselect deferred).
# See gWidgets2Rgtk4/docs/todo.md.

cas5k <- iNZightMR::census.at.school.5000
ui <- iNZight(cas5k)

on.exit(ui$close())

test_that("V1 is a combobox", {
    expect_s4_class(ui$ctrlWidget$V1box, "GComboBoxNoEntry")
})
