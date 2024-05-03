library(shinytest2)

test_that("Adding a new recipe in the app", {

  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "app"))
  app_dir <- file.path(dir, "app")
  db_path <- file.path(app_dir, "db.sqlite")
  app_file <- file.path(app_dir, "app.R")
  writeLines(
    text = glue::glue("MyRecipeManageR::run_app(db_path = '{db_path}')"),
    con = app_file
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  MyRecipeManageR:::create_database(con,
                                    system.file("sql",
                                                "create_database.sql",
                                                package = "MyRecipeManageR")
  )
  # we need to pre-add some ingredients and units to the database tables
  # because with shinytest2 selectizeInput does not work if there are no entries
  # https://github.com/rstudio/shinytest2/issues/232
  ingr <- data.frame(
    ingredient = c("tomato", "onion", "potato", "curry", "salt")
  )
  dplyr::copy_to(con, df = ingr,
                 name = "ingredients", append = TRUE)

  units <- data.frame(
    unit = c("unit", "g", "teaspon")
  )
  dplyr::copy_to(con, df = units,
                 name = "units", append = TRUE)

  cats <- data.frame(
    category = c("main")
  )

  dplyr::copy_to(con, df = cats,
                 name = "categories", append = TRUE)

  DBI::dbDisconnect(con)

  new_recipe_name <- "a test recipe"

  app <- AppDriver$new(app_dir = app_dir,
                       name = "add_new_recipe", height = 800,
                       width = 700)

  app$set_inputs(main = "Create recipe")
  app$set_inputs(`create_recipe-name` = new_recipe_name, wait_ = FALSE)
  app$set_inputs(`create_recipe-dur` = "3", wait_ = FALSE)
  app$set_inputs(`create_recipe-dur` = "3h", wait_ = FALSE)
  app$set_inputs(`create_recipe-src` = "a website", wait_ = FALSE)
  app$set_inputs(`create_recipe-ser` = 4, wait_ = FALSE)
  app$set_inputs(`create_recipe-cat` = "main", wait_ = FALSE)
  # Add ingredients
  app$set_inputs(`create_recipe-new_recipe_add_ing-ing` = "tomato")
  app$set_inputs(`create_recipe-new_recipe_add_ing-unit` = "g", wait_ = FALSE)
  app$set_inputs(`create_recipe-new_recipe_add_ing-amount` = 4, wait_ = FALSE)
  app$set_inputs(`create_recipe-new_recipe_add_ing-prep` = "cut in half", wait_ = FALSE)
  app$click("create_recipe-new_recipe_add_ing-submit")
  app$set_inputs(`create_recipe-new_recipe_add_ing-ing` = "onion", wait_ = FALSE)
  app$set_inputs(`create_recipe-new_recipe_add_ing-unit` = "unit", wait_ = FALSE)
  app$set_inputs(`create_recipe-new_recipe_add_ing-amount` = 2, wait_ = FALSE)
  app$set_inputs(`create_recipe-new_recipe_add_ing-prep` = "cut in slices", wait_ = FALSE)
  app$click("create_recipe-new_recipe_add_ing-submit")
  # Add steps
  app$set_inputs(`create_recipe-new_recipe_add_step-step` = "1", wait_ = FALSE)
  app$set_inputs(`create_recipe-new_recipe_add_step-desc` = "The first step", wait_ = FALSE)
  app$click("create_recipe-new_recipe_add_step-submit")
  app$set_inputs(`create_recipe-new_recipe_add_step-step` = "2", wait_ = FALSE)
  app$set_inputs(`create_recipe-new_recipe_add_step-desc` = "The second step", wait_ = FALSE)
  app$click("create_recipe-new_recipe_add_step-submit")
  # create recipe
  app$click("create_recipe-save_recipe")

  app$expect_values()

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  recipe_meta_table <- dplyr::tbl(con, "recipe_metadata") |>
    dplyr::collect()

  output_new_recipe_name <- recipe_meta_table |>
    dplyr::pull(recipe_name)
  # the new recipe should be in the recipe_metadata db table
  expect_equal(new_recipe_name, output_new_recipe_name)
  # and this table should only have one entry
  expect_equal(nrow(recipe_meta_table), 1)
})
