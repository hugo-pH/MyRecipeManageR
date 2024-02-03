#' ingredient_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_form_ingredient_ui <- function(id, modal) {
  ns <- NS(id)
  if (modal) {
    showModal(
      modalDialog(
        tagList(
          uiOutput(ns("inputs"))
        ),
        easyClose = TRUE,
        footer = NULL
      )
    )
  } else {
    tagList(
      uiOutput(ns("inputs"))
    )
  }
}

#' ingredient_form Server Functions
#'
#' @noRd
mod_form_ingredient_server <- function(id, selected_data = NULL, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(selected_data)) {
      selected_data <- tibble::tibble(
        ingredient = character(0),
        amount = double(0),
        unit = character(0),
        preparation = character(0)
      )
    }

    # Inputs whose values are based on database values ------------------------
    output$inputs <- renderUI({
      ings <- dplyr::tbl(con, "ingredients") |>
        dplyr::distinct(ingredient) |>
        dplyr::pull(ingredient)

      units <- dplyr::tbl(con, "units") |>
        dplyr::distinct(unit) |>
        dplyr::pull(unit)

      tagList(
        h3("Add ingredient"),
        selectizeInput(
          ns("ing"),
          label = "Choose an ingredient",
          choices = ings,
          multiple = FALSE,
          options = list(create = TRUE),
          selected = selected_data$ingredient
        ),
        selectizeInput(
          ns("unit"),
          label = "Choose an unit",
          choices = units,
          multiple = FALSE,
          options = list(create = TRUE),
          selected = selected_data$unit
        ),
        numericInput(
          inputId = ns("amount"),
          label = "Amount:",
          value = selected_data$amount
        ),
        textInput(
          inputId = ns("prep"),
          label = "Preparation:",
          value = selected_data$preparation
        ),
        actionButton(
          inputId = ns("submit"),
          label = "Submit"
        )
      )
    })

    new_data <- reactive({
      ing_name <- input$ing
      ing_unit <- input$unit
      ing_amount <- as.double(input$amount)
      ing_prep <- ifelse(nchar(input$prep) == 0,
        NA_character_,
        input$prep
      )
      # we want to make sure users provided these inputs
      c("amount", "ing", "unit") |>
        purrr::walk(\(id) {
          shinyFeedback::feedbackDanger(
            id,
            is.na(input[[id]]),
            "Please select a value"
          )
        })
      req(ing_name, ing_unit, ing_amount)
      removeModal()
      # add the inputs to the reactive object
      tibble::tibble(
        ingredient = ing_name,
        amount = ing_amount,
        unit = ing_unit,
        preparation = ing_prep
      )
    }) |>
      bindEvent(input$submit)

    list(
      new_data = reactive(new_data()),
      selected_data = selected_data,
      submit = reactive(input$submit)
    )
  })
}
