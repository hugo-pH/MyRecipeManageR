#' show_recipe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_create_recipe_ui <- function(id) {
  ns <- NS(id)

  bslib::page_fillable(
    bslib::layout_columns(
      bslib::card(
        h3("Add metadata"),
        textInput(ns("name"), "Recipe name:"),
        textInput(ns("dur"), "Duration:"),
        textInput(ns("src"), "Source:"),
        numericInput(ns("ser"), "Servings:", value = NA),
        uiOutput(ns("cat_db")),
        actionButton(ns("save_recipe"), "Save recipe")
      ),
      bslib::card(
        mod_form_ingredient_ui(id = ns("new_recipe_add_ing"), modal = FALSE)
      ),
      bslib::card(
        mod_form_step_ui(id = ns("new_recipe_add_step"), modal = FALSE)
      ),
      bslib::card(
        bslib::card_header("Ingredients"),
        mod_table_ingr_ui(ns("ingredients_new_recipe"))
      ),
      bslib::card(
        bslib::card_header("Directions"),
        mod_table_steps_ui(ns("steps_new_recipe"))
      ),
      col_widths = c(
        4, 4, 4,
        6, 6
      ),
      row_heights = c(8, 4)
    )
  )
}

#' show_recipe Server Functions
#'
#' @noRd
mod_create_recipe_server <- function(id, con, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Inputs whose values are based on database values ------------------------
    output$cat_db <- renderUI({
      cats <- dplyr::tbl(con, "categories") |>
        dplyr::distinct(category) |>
        dplyr::pull(category)

      selectizeInput(
        ns("cat"),
        label = "Choose a category",
        choices = cats,
        multiple = FALSE,
        options = list(create = TRUE)
      )
    })

    rv_new_recipe <- reactiveValues(
      data_all_ing = data.frame(),
      data_all_steps = data.frame(),
      data_meta = NULL,
      refresh = NULL
    )

    # Process user input ------------------------------------------------------
    new_ing <- mod_form_ingredient_server(
      id = "new_recipe_add_ing",
      con = con
    )

    observeEvent(new_ing$submit(), {
      if (is.data.frame(new_ing$new_data())) {
        if (nrow(rv_new_recipe$data_all_ing) == 0) {
          rv_new_recipe$data_all_ing <- new_ing$new_data()
        } else {
          rv_new_recipe$data_all_ing <- dplyr::bind_rows(
            rv_new_recipe$data_all_ing,
            new_ing$new_data()
          )
        }
        rv_new_recipe$refresh <- rnorm(2)
      }
    })

    new_step <- mod_form_step_server(
      id = "new_recipe_add_step",
      con = con
    )

    observeEvent(new_step$submit(), {
      if (is.data.frame(new_step$new_data())) {
        if (nrow(rv_new_recipe$data_all_steps) == 0) {
          rv_new_recipe$data_all_steps <- new_step$new_data()
        } else {
          new_step_idx <- new_step$new_data()$step

          rv_new_recipe$data_all_steps <- rv_new_recipe$data_all_steps |>
            dplyr::mutate(
              # Adjust steps order
              step = dplyr::case_when(
                step >= new_step_idx ~ step + 1,
                .default = step
              )
            ) |>
            dplyr::bind_rows(new_step$new_data()) |>
            dplyr::arrange(step)
          rv_new_recipe$refresh <- rnorm(2)
        }
      }
    })

    # Tables ------------------------------------------------------------------
    rv_new_recipe <- mod_table_ingr_server("ingredients_new_recipe", rv_new_recipe, con)
    rv_new_recipe <- mod_table_steps_server("steps_new_recipe", rv_new_recipe)
    # Save recipe -------------------------------------------------------------
    observeEvent(input$save_recipe, {
      shinyFeedback::feedbackDanger(
        "name",
        nchar(input$name) == 0,
        "Please provide a recipe name."
      )
      req(input$name)

      recipes_in_db <- dplyr::tbl(con, "recipe_metadata") |>
        dplyr::pull(recipe_name)

      shinyFeedback::feedbackDanger(
        "name",
        input$name %in% recipes_in_db,
        "The provided recipe name is already present in in the db."
      )

      req(!(input$name %in% recipes_in_db))

      shinyFeedback::feedbackDanger(
        "src", nchar(input$src) == 0,
        "Please provide a source."
      )
      req(input$src)

      shinyFeedback::feedbackDanger(
        "ser",
        is.na(input$ser),
        "Please provide a number of servings."
      )
      req(input$ser)

      if (nrow(rv_new_recipe$data_all_ing) == 0) {
        shinyalert::shinyalert(
          title = "Oops",
          text = "To create a new recipe you need to add at least one ingredient.",
          type = "error"
        )
      }
      req(nrow(rv_new_recipe$data_all_ing) > 0)

      if (nrow(rv_new_recipe$data_all_steps) == 0) {
        shinyalert::shinyalert(
          title = "Oops",
          text = "To create a new recipe you need to add at least one step.",
          type = "error"
        )
      }

      req(nrow(rv_new_recipe$data_all_steps) > 0)

      rv_new_recipe$data_meta <- data.frame(
        recipe_name = input$name,
        source = input$src,
        servings = input$ser,
        duration = input$dur,
        category = input$cat
      )

      update_database(
        df_ingredients = rv_new_recipe$data_all_ing,
        df_steps = rv_new_recipe$data_all_steps,
        df_meta = rv_new_recipe$data_meta,
        con = con
      )

      m <- glue::glue("Recipe {input$name} was added to the database.")
      shinyalert::shinyalert(
        title = "Hooray!",
        text = m,
        type = "success"
      )
      rv$refresh <- stats::rnorm(2)

    })

    exportTestValues(
      new_recipe_ing = rv_new_recipe$data_all_ing,
      new_recipe_steps = rv_new_recipe$data_all_steps,
      new_recipe_meta = rv_new_recipe$data_meta
    )
  })
}
