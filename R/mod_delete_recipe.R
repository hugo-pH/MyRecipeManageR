#' delete_recipe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_delete_recipe_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(wellPanel(
    uiOutput(ns("recipes")),
    actionButton(
      inputId = ns("delete"),
      label = "Delete recipe"
    )
  )))
}

#' delete_recipe Server Functions
#'
#' @noRd
mod_delete_recipe_server <- function(id, con, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$recipes <- renderUI({
      rv$refresh
      recipes <- dplyr::tbl(con, "recipe_metadata") |>
        dplyr::distinct(recipe_name) |>
        dplyr::pull(recipe_name)

      selectInput(
        ns("selected_recipe"), "Choose a recipe",
        recipes
      )
    })

    observeEvent(input$delete, {
      showModal(
        modalDialog(
          "Are you sure you want to continue?",
          title = "Deleting files",
          footer = tagList(
            actionButton(ns("cancel"), "Cancel"),
            actionButton(ns("ok"),
              "Delete",
              class = "btn btn-danger"
            )
          )
        )
      )
    })

    observeEvent(input$cancel, {
      removeModal()
    })

    observeEvent(input$ok, {
      # browser()
      name <- input$selected_recipe
      recipe_id <- dplyr::tbl(con, "recipe_metadata") |>
        dplyr::filter(recipe_name == name) |>
        dplyr::pull(recipe_id)

      delete_query <- glue::glue_sql("
                    DELETE
                    FROM recipe_metadata
                    WHERE recipe_id = {recipe_id}
                                     ",
        .con = con
      )
      del <- DBI::dbExecute(con, delete_query)
      rv$refresh <- stats::rnorm(2)
      removeModal()
      if (del == 1) {
        shinyalert::shinyalert(
          title = "Sucess",
          text = glue::glue("Recipe {name} was deleted."),
          type = "success"
        )
      } else {
        shinyalert::shinyalert(
          title = "Oops",
          text = glue::glue("Recipe {name} could not deleted."),
          type = "error"
        )
      }
    })
  })
}
