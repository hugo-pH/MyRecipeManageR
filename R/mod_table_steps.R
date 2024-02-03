#' table_steps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_steps_ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactable::reactableOutput(ns("tbl_steps_output"))

  )
}

#' table_steps Server Functions
#'
#' @noRd
mod_table_steps_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Tables ------------------------------------------------------------------
    tbl_steps <- reactive({
      rv$refresh
      validate(need(nrow(rv$data_all_steps) > 0, "Add steps"))

      edit <- ns("edit_step")
      row_to_edit <- ns("row_to_edit_step")
      delete <- ns("delete_step")
      row_to_delete <- ns("row_to_delete_step")

      js <- paste0(
        "function(rowInfo, column) {
        if (column.id !== 'delete' && column.id !== 'edit') {
          return
        }
        if (window.Shiny && column.id == 'edit') {
          Shiny.setInputValue('",row_to_edit, "' , { index: rowInfo.index + 1 })
          Shiny.setInputValue('",edit, "' , { event: Math.random() })
        }
        if (window.Shiny && column.id == 'delete') {
          Shiny.setInputValue('",row_to_delete, "' , { index: rowInfo.index + 1 })
          Shiny.setInputValue('",delete, "' , { event: Math.random() })
        }
      }"
      )

      reactable::reactable(

        rv$data_all_steps |>
          tibble::add_column(edit = NA, .before = 1) |>
          tibble::add_column(delete = NA, .after = "description"),
        defaultPageSize = 50,
        columns = list(
          description = reactable::colDef(name = "Description"),
          step = reactable::colDef(name = "Step", width = 75,
                                   sortable = TRUE),
          delete = reactable::colDef(
            name = "",
            width = 25,
            sortable = FALSE,
            cell = function() shiny::icon("trash",
                                          class = "fas",
                                          style = "color: red")
          ),
          edit = reactable::colDef(
            name = "",
            width = 25,
            sortable = FALSE,
            cell = function() shiny::icon("pen-to-square",
                                          class = "fas",
                                          style = "color: green")
          )
        ),
        onClick = reactable::JS(js),
        highlight = TRUE
      )
    })

    output$tbl_steps_output <- reactable::renderReactable({
      tbl_steps()
    })

    selected_step <- reactiveValues(
      row = NULL
    )

    observeEvent(input$edit_step, {
      mod_form_step_ui(id = ns("update_recipe_edit_step"),
                       modal = TRUE)
      selected_step$row <- input$row_to_edit_step$index
    })

    selected_data <- reactive({
      req(selected_step$row)
      rv$data_all_steps |>
        dplyr::slice(selected_step$row)
    })

    modified_step <- reactive({
      req(selected_step$row)
      mod_form_step_server(
        id = "update_recipe_edit_step",
        selected_data = selected_data(),
        con = con
      )
    })

    observeEvent(modified_step()$submit(), {

      if(is.data.frame(modified_step()$new_data())) {
        if(!identical(
          modified_step()$selected_data,
          modified_step()$new_data()
          )
        ) {
          current_step <- rv$data_all_steps  |>
            dplyr::slice(selected_step$row) |>
            dplyr::pull(step)
          new_step <- modified_step()$new_data()$step
          rv$data_all_steps <- rv$data_all_steps[-selected_step$row, ]

          if(current_step != new_step) {
            # If the user changed the step (order) we need to adjust
            # the order of the other steps
            direction_of_change <- ifelse(
              new_step > current_step,
              -1,
              1)
            steps_to_change <- seq(new_step,
                                   current_step)
            idxs_to_change <- which(rv$data_all_steps$step %in% steps_to_change)

            rv$data_all_steps$step[idxs_to_change] <- rv$data_all_steps$step[idxs_to_change] + direction_of_change
          }

          rv$data_all_steps <- dplyr::bind_rows(
            rv$data_all_steps,
            modified_step()$new_data()
          ) |>
            dplyr::arrange(step)
        }
      }
      selected_step$row <- NULL
    })

    observeEvent(input$delete_step, {
      # browser()
      row_to_delete <- input$row_to_delete_step$index
      if (nrow(rv$data_all_steps) > 0 & !is.null(row_to_delete)) {
        rv$data_all_steps <- rv$data_all_steps[-row_to_delete, ] |>
          # when a step is deleted, re-calculate order
          dplyr::arrange(step) |>
          dplyr::mutate(
            step = dplyr::row_number()
          )
      }
    })

    return(rv)
  })
}
