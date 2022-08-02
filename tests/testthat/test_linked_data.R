iris_species <- data.frame(
    species_id = 1:3,
    species_name = levels(iris$Species),
    type_id = c(1L, 1L, 2L)
)
iris_data <- iris %>%
    dplyr::mutate(
        species_id = as.integer(iris$Species),
        Species = NULL
    )
iris_extra <- data.frame(
    id = 1:2,
    type = c("Fluffy", "Hard")
)
iris_schema <- list(
    iris_data = list(
        links_to = list(
            iris_species = "species_id"
        )
    ),
    iris_species = list(
        links_to = list(
            iris_extra = c("type_id" = "id")
        )
    )
)

t0 <- tempfile(fileext = ".sql")
t1 <- tempfile(fileext = ".csv")
t2 <- tempfile(fileext = ".csv")
t3 <- tempfile(fileext = ".csv")
t4 <- tempfile(fileext = ".inzlnk")
on.exit(unlink(c(t1, t2, t3, t4)))

write.csv(iris_species, file = t1, row.names = FALSE, quote = FALSE)
write.csv(iris_data, file = t2, row.names = FALSE, quote = FALSE)
write.csv(iris_extra, file = t3, row.names = FALSE, quote = FALSE)
writeLines(sprintf(
"files:
    iris_species: %s
    iris_data: %s
    iris_extra: %s
schema:
  iris_data:
    links_to:
      iris_species: species_id
  iris_species:
    links_to:
      iris_extra:
        type_id: id
", t1, t2, t3),
    t4
)

test_that("Linked data can be loaded into iNZight", {
    # con <- DBI::dbConnect(RSQLite::SQLite(), t0)
    # on.exit({
    #     DBI::dbDisconnect(con)
    #     unlink(t0)
    # })

    d <- iNZightTools::load_linked(
        c(iris_species = t1, iris_data = t2, iris_extra = t3),
        schema = iris_schema,
        # con = con,
        name = "iris"
    )

    ui <- iNZight(d)
    on.exit(try(ui$close(), silent = TRUE), add = TRUE)
    expect_is(ui, "iNZGUI")
    expect_equal(ui$getActiveData(lazy = TRUE), d)
})
