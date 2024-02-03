#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        golem::get_golem_options("db_path"))

  onStop(function() {
    DBI::dbDisconnect(con)
  })

  # Reactive object ---------------------------------------------------------
  # Create a reactive object to which we will recursively add all new
  # ingredients and steps
  rv <- reactiveValues()
  rv$data_all_ing <- data.frame(
    ing_name = NULL,
    ing_unit = NULL,
    ing_amount = NULL,
    ing_prep = NULL
  )
  rv$data_all_steps <- data.frame(step = NULL,
                                  description = NULL)
  rv$data_meta <- data.frame(
    recipe_name = NULL,
    source = NULL,
    servings = NULL,
    duration = NULL,
    category = NULL
  )
  rv$refresh <- NULL
  rv$selected_recipe_id <- NULL
  rv <- mod_choose_recipe_server("all_recipes", con = con, rv = rv)

  observeEvent(rv$selected_recipe_id, {

    updateNavbarPage(session = session,
                      inputId =  "main",
                      selected = "see_recipe")
  })

  db_tables <- DBI::dbListTables(con)

  expected_tables <- c("recipe_metadata", "categories", "units",
                       "ingredients_in_recipes", "steps_in_recipes")

  if(!(all(expected_tables %in% db_tables))) {

    p1 <- p("No tables have been defined in the database!")
    p2 <- p("This means the app is unusable :(")
    p3 <- p("Contact the app adimin to solve this issue.")

    shinyalert::shinyalert(
      title = "Oops",
      text = tagList(p1, p2, p3),
      type = "error",
      html = TRUE
    )
  } else {
    mod_show_recipe_server("show_recipe", con = con, rv = rv)
    mod_create_recipe_server("create_recipe", con = con, rv = rv)
    mod_delete_recipe_server("delete_recipe", con = con, rv = rv)
  }

}
