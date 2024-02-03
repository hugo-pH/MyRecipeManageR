update_database <- function(
    df_ingredients,
    df_steps,
    df_meta,
    con) {

  meta_append <- append_table_recipe_metadata(
    df_meta,
    con = con,
    meta_tbl_name = "recipe_metadata",
    cat_tbl_name = "categories"
  )

  ing_append <-
    append_table_ingredients_in_recipe(
      df_ingredients,
      df_meta = df_meta,
      con = con,
      ing_rec_tbl_name = "ingredients_in_recipes",
      meta_tbl_name = "recipe_metadata",
      ing_tbl_name = "ingredients",
      un_tbl_name = "units"
    )

  st_append <- append_table_steps_in_recipe(
    df_steps,
    df_meta = df_meta,
    con = con,
    st_rec_tbl_name = "steps_in_recipes",
    meta_tbl_name = "recipe_metadata"
  )
  return(TRUE)
}
