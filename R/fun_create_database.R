create_database <- function(con, path) {
  readLines(path) |>
    glue::glue_collapse() |>
    glue::glue_sql() |>
    stringr::str_split(";") |>
    dplyr::first() |>
    purrr::walk(\(qry) {
      if (nchar(qry) > 0) {
        DBI::dbExecute(con, glue::glue_sql(qry))
      }
    })
}

populate_ingredients <- function(df, tbl_name, con) {
  df_db <- dplyr::tbl(con, tbl_name)

  n_rows <- df_db |>
    dplyr::pull(ingredient_id)

  if (length(n_rows) == 0) {
    df <- df |>
      dplyr::distinct(ingredient)

    dplyr::copy_to(con,
      df = df, name = tbl_name,
      append = TRUE, temporary = TRUE
    )
  } else {
    df_to_db <- df |>
      dplyr::anti_join(
        df_db,
        by = dplyr::join_by(ingredient),
        copy = TRUE
      ) |>
      dplyr::distinct(ingredient) |>
      dplyr::copy_to(con,
        df = _, name = tbl_name,
        append = TRUE, temporary = TRUE
      )
  }
}

populate_units <- function(df, tbl_name, con) {
  df_db <- dplyr::tbl(con, tbl_name)

  n_rows <- df_db |>
    dplyr::pull(unit_id)

  if (length(n_rows) == 0) {
    df <- df |>
      dplyr::filter(!is.na(unit)) |>
      dplyr::distinct(unit)

    dplyr::copy_to(con,
      df = df, name = tbl_name,
      append = TRUE, temporary = TRUE
    )
  } else {
    df_to_db <- df |>
      dplyr::filter(!is.na(unit)) |>
      dplyr::mutate(
        unit = stringr::str_to_lower(unit),
        unit = stringr::str_trim(unit, side = "both")
      ) |>
      dplyr::anti_join(
        df_db,
        by = dplyr::join_by(unit),
        copy = TRUE
      ) |>
      dplyr::distinct(unit) |>
      dplyr::copy_to(con,
        df = _, name = tbl_name,
        append = TRUE, temporary = TRUE
      )
  }
}

populate_categories <- function(df, tbl_name, con) {
  df_db <- dplyr::tbl(con, tbl_name)

  n_rows <- df_db |>
    dplyr::pull(category_id)

  if (length(n_rows) == 0) {
    df <- df |>
      dplyr::filter(!is.na(category)) |>
      dplyr::distinct(category)

    dplyr::copy_to(con,
      df = df, name = tbl_name,
      append = TRUE, temporary = TRUE
    )
  } else {
    df_to_db <- df |>
      dplyr::filter(!is.na(category)) |>
      dplyr::mutate(
        category = stringr::str_to_lower(category),
        category = stringr::str_trim(category, side = "both")
      ) |>
      dplyr::anti_join(
        df_db,
        by = dplyr::join_by(category),
        copy = TRUE
      ) |>
      dplyr::distinct(category) |>
      dplyr::copy_to(con,
        df = _, name = tbl_name,
        append = TRUE, temporary = TRUE
      )
  }
}



populate_recipe_metadata <- function(name, df, tbl_name, con) {
  df_db <- dplyr::tbl(con, tbl_name) |>
    dplyr::collect()

  n_rows <- df_db |>
    dplyr::pull(recipe_id)

  if (length(n_rows) == 0) {
    df <- df |>
      dplyr::mutate(
        recipe_name = stringr::str_replace_all(name, "_", " "),
        servings = as.numeric(servings)
      )
    df <- df_db |>
      dplyr::slice(0) |>
      dplyr::select(-created_on) |>
      dplyr::bind_rows(df)

    dplyr::copy_to(con,
      df = df, name = tbl_name,
      append = TRUE, temporary = TRUE
    )
  } else {
    df_to_db <- df |>
      dplyr::mutate(
        recipe_name = stringr::str_replace_all(name, "_", " ")
      ) |>
      dplyr::anti_join(
        df_db,
        by = dplyr::join_by(recipe_name),
        copy = TRUE
      ) |>
      dplyr::copy_to(con,
        df = _, name = tbl_name,
        append = TRUE, temporary = TRUE
      )
  }
}
