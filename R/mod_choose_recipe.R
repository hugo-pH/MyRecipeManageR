#' show_all_recipes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_choose_recipe_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Select a recipe"),
        reactable::reactableOutput(ns("tbl_recipes"))
      )
    )
  )
}

#' show_all_recipes Server Functions
#'
#' @noRd
mod_choose_recipe_server <- function(id, con, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    db_tables <- DBI::dbListTables(con)

    expected_tables <- c(
      "recipe_metadata", "categories", "units",
      "ingredients_in_recipes", "steps_in_recipes"
    )

    all_recipes <- reactive({
      rv$refresh

      req(all(expected_tables %in% db_tables))

      dplyr::tbl(con, "recipe_metadata") |>
        dplyr::left_join(
          dplyr::tbl(con, "categories"),
          copy = TRUE,
          by = dplyr::join_by(category_id)
        ) |>
        dplyr::select(-category_id) |>
        dplyr::collect() |>
        dplyr::arrange(recipe_name)
    })


    tbl_all_recipes <- reactive({
      validate(
        need(
          all(expected_tables %in% db_tables),
          "No tables have been defined in the database!"
        )
      )

      validate(
        need(
          nrow(all_recipes()) > 0,
          "There are no recipes in the database!"
        )
      )

      all_recipes <- all_recipes()

      recipe <- ns("selected_recipe")
      js <- paste0(
        "function(rowInfo, column) {
        if (window.Shiny) {
          Shiny.setInputValue('", recipe, "' , { index: rowInfo.index + 1 })
        }
      }"
      )

      reactable::reactable(
        all_recipes,
        columns = list(
          recipe_id = reactable::colDef(show = FALSE),
          recipe_name = reactable::colDef(name = "Recipe"),
          source = reactable::colDef(name = "Source"),
          servings = reactable::colDef(name = "Servings"),
          duration = reactable::colDef(name = "Duration"),
          created_on = reactable::colDef(name = "Date of creation"),
          category = reactable::colDef(name = "Category")
        ),
        defaultPageSize = 50,
        onClick = reactable::JS(js),
        highlight = TRUE,
        filterable = TRUE
      )
    })

    output$tbl_recipes <- reactable::renderReactable({
      tbl_all_recipes()
    })

    observeEvent(input$selected_recipe, {
      rv$selected_recipe_id <- all_recipes() |>
        dplyr::slice(input$selected_recipe$index) |>
        dplyr::pull(recipe_id)
    })

    exportTestValues(
      all_recipes = all_recipes() |>
        dplyr::select(-created_on)
    )

    return(rv)
  })
}
