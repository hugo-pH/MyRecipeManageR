#' table_ingr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_ingr_ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactable::reactableOutput(ns("tbl_ing_output"))
  )
}

#' table_ingr Server Functions
#'
#' @noRd
mod_table_ingr_server <- function(id, rv, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Tables ------------------------------------------------------------------
    tbl_ing <- reactive({
      rv$refresh
      validate(need(nrow(rv$data_all_ing) > 0, "Add ingredients."))

      edit <- ns("edit_ing")
      row_to_edit <- ns("row_to_edit_ing")
      delete <- ns("delete_ing")
      row_to_delete <- ns("row_to_delete_ing")

      js <- paste0(
        "function(rowInfo, column) {
        if (column.id !== 'delete' && column.id !== 'edit') {
          return
        }
        if (window.Shiny && column.id == 'edit') {
          Shiny.setInputValue('", row_to_edit, "' , { index: rowInfo.index + 1 })
          Shiny.setInputValue('", edit, "' , { event: Math.random() })
        }
        if (window.Shiny && column.id == 'delete') {
          Shiny.setInputValue('", row_to_delete, "' , { index: rowInfo.index + 1 })
          Shiny.setInputValue('", delete, "' , { event: Math.random() })
        }
      }"
      )
      reactable::reactable(
        rv$data_all_ing |>
          tibble::add_column(edit = NA, .before = 1) |>
          tibble::add_column(delete = NA, .after = "preparation"),
        defaultPageSize = 50,
        columns = list(
          ingredient = reactable::colDef(name = "Ingredient", width = 150),
          amount = reactable::colDef(name = "Amount", width = 100),
          unit = reactable::colDef(name = "Unit", width = 75),
          preparation = reactable::colDef(name = "Preparation"),
          delete = reactable::colDef(
            name = "",
            width = 25,
            sortable = FALSE,
            cell = function() {
              shiny::icon("trash",
                class = "fas",
                style = "color: red"
              )
            }
          ),
          edit = reactable::colDef(
            name = "",
            width = 25,
            sortable = FALSE,
            cell = function() {
              shiny::icon("pen-to-square",
                class = "fas",
                style = "color: green"
              )
            }
          )
        ),
        onClick = reactable::JS(js),
        highlight = TRUE
      )
    })

    output$tbl_ing_output <- reactable::renderReactable({
      tbl_ing()
    })

    selected_ing <- reactiveValues(
      row = NULL
    )

    observeEvent(input$edit_ing, {
      mod_form_ingredient_ui(id = ns("update_recipe_edit_ing"), modal = TRUE)
      selected_ing$row <- input$row_to_edit_ing$index
    })

    selected_data <- reactive({
      req(selected_ing$row)
      rv$data_all_ing |>
        dplyr::slice(selected_ing$row)
    })

    modified_ing <- reactive({
      req(selected_ing$row)
      mod_form_ingredient_server(
        id = "update_recipe_edit_ing",
        selected_data = selected_data(),
        con = con
      )
    })

    observeEvent(modified_ing()$submit(), {
      if (is.data.frame(modified_ing()$new_data())) {
        if (!identical(
          modified_ing()$selected_data,
          modified_ing()$new_data()
        )
        ) {
          rv$data_all_ing <- rv$data_all_ing[-selected_ing$row, ]

          rv$data_all_ing <- dplyr::bind_rows(
            rv$data_all_ing,
            modified_ing()$new_data()
          )
        }
      }
      selected_ing$row <- NULL
    })

    observeEvent(input$delete_ing, {
      row_to_delete <- input$row_to_delete_ing$index
      if (nrow(rv$data_all_ing) > 0 & !is.null(row_to_delete)) {
        rv$data_all_ing <- rv$data_all_ing[-row_to_delete, ]
      }
    })

    return(rv)
  })
}
