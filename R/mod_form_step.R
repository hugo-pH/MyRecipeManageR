#' step_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_form_step_ui <- function(id, modal) {
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

#' step_form Server Functions
#'
#' @noRd
mod_form_step_server <- function(id, selected_data = NULL, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Inputs whose values are based on database values ------------------------
    output$inputs <- renderUI({
      tagList(
        h3("Add step"),
        selectizeInput(
          inputId = ns("step"),
          label = "Step order:",
          choices = 1:20,
          selected = selected_data$step,
          options = list(create = TRUE)
        ),
        textAreaInput(
          inputId = ns("desc"),
          label = "Description:",
          value = selected_data$description,
          width = "100%",
          height = "100%"
        ),
        actionButton(
          inputId = ns("submit"),
          label = "Submit"
        )
      )
    })

    new_data <- reactive({
      step <- input$step |> as.integer()
      description <- input$desc
      # we want to make sure users provided these inputs
      shinyFeedback::feedbackDanger(
        inputId = "step",
        is.na(input$step),
        "Please select a value"
      )
      shinyFeedback::feedbackDanger(
        inputId = "desc",
        nchar(input$desc) == 0,
        "Please select a value"
      )
      req(step, description)
      removeModal()

      tibble::tibble(
        step = step,
        description = description
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
