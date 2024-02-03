append_table_recipe_metadata <- function(df,
                                         con,
                                         meta_tbl_name = "recipe_metadata",
                                         cat_tbl_name = "categories"
                                         ) {

  recipe_id <- df |>
    dplyr::left_join(dplyr::tbl(con, meta_tbl_name),
                     by = dplyr::join_by(recipe_name),
                     copy = TRUE) |>
    dplyr::pull(recipe_id)

  stopifnot("There is already a recipe with the provided name"=length(recipe_id) > 0)
  # get FK
  df_cats <- dplyr::tbl(con, cat_tbl_name)

  recipe_cat <- df |>
    dplyr::pull(category)

  cat_id <- df_cats |>
    dplyr::filter(category == recipe_cat) |>
    dplyr::pull(category_id)

  if (length(cat_id) == 0) {

    df |>
      dplyr::select(category) |>
      dplyr::copy_to(con, df = _, name = cat_tbl_name, append = TRUE)

    cat_id <- df_cats |>
      dplyr::filter(category == recipe_cat) |>
      dplyr::pull(category_id)
  }

  df |>
    dplyr::mutate(
      category_id = cat_id
    ) |>
    dplyr::select(-category) |>
    dplyr::copy_to(con, df = _, name = meta_tbl_name, append = TRUE)

  return(nrow(df))

}


append_table_ingredients_in_recipe <- function(df_ing,
                                               df_meta,
                                               con,
                                               ing_rec_tbl_name = "ingredients_in_recipes",
                                               meta_tbl_name = "recipe_metadata",
                                               ing_tbl_name = "ingredients",
                                               un_tbl_name = "units") {

  recipe_id <- df_meta |>
    dplyr::left_join(dplyr::tbl(con, meta_tbl_name),
                     by = dplyr::join_by(recipe_name),
                     copy = TRUE) |>
    dplyr::pull(recipe_id)


  stopifnot("The provided recipe is not yet in the database"=length(recipe_id) == 1)


  df_db_ing <- dplyr::tbl(con, ing_tbl_name)

  df_ing_ids <- df_ing |>
    dplyr::left_join(df_db_ing,
                     by = dplyr::join_by(ingredient),
                     copy = TRUE) |>
    dplyr::select(ingredient, ingredient_id) |>
    dplyr::distinct()

  if(any(is.na(df_ing_ids$ingredient_id))) {

    df_ing_ids |>
      dplyr::filter(is.na(ingredient_id)) |>
      dplyr::select(ingredient) |>
      dplyr::copy_to(con, df = _, name = ing_tbl_name, append = TRUE)

    df_ing_ids <- df_ing |>
      dplyr::left_join(df_db_ing,
                       by = dplyr::join_by(ingredient),
                       copy = TRUE) |>
      dplyr::select(ingredient, ingredient_id)
  }

  df_db_un <- dplyr::tbl(con, un_tbl_name)

  df_unit_ids <- df_ing |>
    dplyr::left_join(df_db_un,
                     by = dplyr::join_by(unit),
                     copy = TRUE) |>
    dplyr::select(unit, unit_id) |>
    dplyr::distinct()

  if(any(is.na(df_unit_ids$unit_id))) {

    df_unit_ids |>
      dplyr::filter(is.na(unit_id)) |>
      dplyr::select(unit) |>
      dplyr::copy_to(con, df = _, name = un_tbl_name, append = TRUE)

    df_unit_ids <- df_ing |>
      dplyr::left_join(df_db_un,
                       by = dplyr::join_by(unit),
                       copy = TRUE) |>
      dplyr::select(unit, unit_id)
  }

  df_ing |>
    dplyr::left_join(df_unit_ids,
                     by = dplyr::join_by(unit),
                     relationship = "many-to-one") |>
    dplyr::select(-unit) |>
    dplyr::left_join(df_ing_ids,
                     by = dplyr::join_by(ingredient),
                     relationship = "many-to-one") |>
    dplyr::select(-ingredient) |>
    dplyr::mutate(
      recipe_id = recipe_id
    ) |>
    dplyr::copy_to(con, df = _, name = ing_rec_tbl_name, append = TRUE)

  return(nrow(df_ing))

}


append_table_steps_in_recipe <- function(df_steps,
                                               df_meta,
                                               con,
                                               st_rec_tbl_name = "steps_in_recipes",
                                               meta_tbl_name = "recipe_metadata") {

  recipe_id <- df_meta |>
    dplyr::left_join(dplyr::tbl(con, meta_tbl_name),
                     by = dplyr::join_by(recipe_name),
                     copy = TRUE) |>
    dplyr::pull(recipe_id)

  stopifnot("The provided recipe is not yet in the database"=length(recipe_id) == 1)

  df_steps |>
    dplyr::mutate(
      recipe_id = recipe_id
    ) |>
    dplyr::copy_to(con, df = _, name = st_rec_tbl_name, append = TRUE)

  return(nrow(df_steps))


  }
