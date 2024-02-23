#' shopping_list UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_shopping_list_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("recipes")),
    br(),
    downloadButton(outputId = ns("render"), label = "Generate Report:")
  )
}

#' shopping_list Server Functions
#'
#' @noRd
mod_shopping_list_server <- function(id, con, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$recipes <- renderUI({
      rv$refresh
      recipes <- dplyr::tbl(con, "recipe_metadata") |>
        dplyr::distinct(recipe_name) |>
        dplyr::pull(recipe_name)

      selectInput(
        inputId = ns("selected_recipe"),
        label =  "Choose recipes",
        choices = recipes,
        multiple = TRUE
      )
    })

    output$render <- downloadHandler(
      filename = "shopping_list.html",
      content = function(file) {

        file_name <- "shopping_list"

        qmd_pkg <- file.path(app_sys("app/quarto"),
                                paste0(file_name, ".qmd"))
        reports_dir <- golem::get_golem_options("reports_dir")
        qmd_copy <- file.path(reports_dir, paste0(file_name, ".qmd"))
        file.copy(qmd_pkg,
                  reports_dir)

        quarto::quarto_render(
          input = qmd_copy,
          execute_params = list(db_path = golem::get_golem_options("db_path"),
                                recipes = input$selected_recipe)
        )
        file.remove(
          qmd_copy
        )
        file.copy(
          file.path(reports_dir, paste0(file_name, ".html"))
          , file)
      }
    )

  })
}
