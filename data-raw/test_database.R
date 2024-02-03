con <- DBI::dbConnect(RSQLite::SQLite(), "inst/db/test-db.sqlite")

create_database(con, "inst/sql/create_database.sql")

recipe_dirs <- list.files(here::here("data-raw", "test_recipes"),
  full.names = TRUE
)

purrr::walk(recipe_dirs, \(dir) {
  print(dir)
  df.ingredients <- readr::read_csv(file.path(dir, "ingredients.csv"),
    col_types = readr::cols(
      ingredient = readr::col_character(),
      amount = readr::col_double(),
      unit = readr::col_character(),
      preparation = readr::col_character()
    )
  ) |>
    dplyr::mutate(
      ingredient = stringr::str_to_lower(ingredient),
      ingredient = stringr::str_trim(ingredient, side = "both")
    )
  populate_ingredients(df.ingredients, "ingredients", con)
  populate_units(df.ingredients, "units", con)

  df.meta <- readr::read_csv(file.path(dir, "metadata.csv"),
    col_types = readr::cols(
      field = readr::col_character(),
      description = readr::col_character()
    )
  ) |>
    dplyr::filter(!(field %in% c("creation date", "Combined recipe"))) |>
    tidyr::pivot_wider(names_from = field, values_from = description)

  if ("category" %in% colnames(df.meta)) {
    df.cat <- df.meta |>
      dplyr::select(category)

    populate_categories(df.meta, "categories", con)

    df.meta <- df.meta |>
      dplyr::left_join(
        dplyr::tbl(con, "categories"),
        by = "category",
        relationship = "one-to-one",
        copy = TRUE
      ) |>
      dplyr::select(-category)
  }

  recipe_name <- stringr::str_replace_all(basename(dir), "_", " ")
  populate_recipe_metadata(recipe_name, df.meta, "recipe_metadata", con)

  readr::read_delim(file.path(dir, "instructions.tsv"),
    delim = "\t",
    col_types = readr::cols(
      step = readr::col_double(),
      description = readr::col_character()
    )
  ) |>
    dplyr::mutate(
      recipe_name = recipe_name
    ) |>
    dplyr::left_join(
      dplyr::tbl(con, "recipe_metadata") |>
        dplyr::select(recipe_name, recipe_id),
      by = "recipe_name",
      relationship = "many-to-one",
      copy = TRUE
    ) |>
    dplyr::select(-recipe_name) |>
    dplyr::copy_to(con,
      df = _, name = "steps_in_recipes",
      append = TRUE
    )

  df.ingredients |>
    dplyr::mutate(
      recipe_name = recipe_name
    ) |>
    dplyr::left_join(
      dplyr::tbl(con, "recipe_metadata") |>
        dplyr::select(recipe_name, recipe_id),
      by = "recipe_name",
      relationship = "many-to-one",
      copy = TRUE
    ) |>
    dplyr::left_join(
      dplyr::tbl(con, "ingredients"),
      by = "ingredient",
      relationship = "many-to-one",
      copy = TRUE
    ) |>
    dplyr::left_join(
      dplyr::tbl(con, "units"),
      by = "unit",
      relationship = "many-to-one",
      copy = TRUE
    ) |>
    dplyr::select(-c(recipe_name, ingredient, unit)) |>
    dplyr::copy_to(con,
      df = _, name = "ingredients_in_recipes",
      append = TRUE
    )
})

DBI::dbDisconnect(con)
