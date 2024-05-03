test_that("duplicated new ingredient", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    create_database(con, fs::path_package("sql",
                                          "create_database.sql",
                                          package = "MyRecipeManageR"))

    meta_data <- data.frame(
      recipe_name = "test",
      source = "a source",
      servings = 2,
      duration = "2h",
      category = "x"
    )

    ingredients <- tibble::tribble(
      ~ingredient, ~amount, ~unit, ~preparation,
      "pumpukin", 2, "unit", "",
      "pumpukin", 2, "unit", "",
      "macarroes", 2, "aen", "we"
    )

    steps <- tibble::tribble(
      ~step, ~description,
      1, "foo",
      2, "bar"
    )

    expect_true(
      update_database(
        df_ingredients = ingredients,
        df_steps = steps,
        df_meta =   meta_data,
        con = con
      )
    )

})
