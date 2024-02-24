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
  waiter::waiter_set_theme(html = waiter::spin_3())

  bslib::page_fillable(
    waiter::useWaiter(),
    bslib::layout_column_wrap(
      bslib::card(
        bslib::card_header("Selecting recipes"),
        uiOutput(ns("recipes"))
      ),
      bslib::card(
        bslib::card_header("Downloadable reports"),
        downloadButton(outputId = ns("render_pdf"), label = "Generate pdf report"),
        downloadButton(outputId = ns("render_html"), label = "Generate html report")
      ),
      bslib::card(
        bslib::card_header("Ingredients"),
        reactable::reactableOutput(ns("tbl_ing_output"))
        ),
      col_widths = c(
        6, 6,
        12
      ),
      row_heights = c(2, 8)
    )
  )
}

#' shopping_list Server Functions
#'
#' @noRd
mod_shopping_list_server <- function(id, con, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    w <- waiter::Waiter$new(
      color = waiter::transparent(.9),
    )

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



    output$render_pdf <- downloadHandler(
      filename = "shopping_list_pdf.pdf",
      content = function(file) {

        if(is.null(input$selected_recipe)){
          shinyalert::shinyalert(
            title = "Oops",
            text = "You did not select any recipe. An empty file will be downloaded.",
            type = "error"
          )
        } else {
          w$show()
        }

        file_name <- "shopping_list_pdf"

        qmd_pkg <- file.path(app_sys("app/quarto"),
                             paste0(file_name, ".qmd"))
        reports_dir <- golem::get_golem_options("reports_dir")
        qmd_copy <- file.path(reports_dir, paste0(file_name, ".qmd"))
        file.copy(qmd_pkg,
                  reports_dir,
                  overwrite = TRUE)

        quarto::quarto_render(
          input = qmd_copy,
          execute_params = list(db_path = golem::get_golem_options("db_path"),
                                recipes = input$selected_recipe)
        )
        file.remove(
          qmd_copy
        )
        file.copy(
          file.path(reports_dir, paste0(file_name, ".pdf"))
          , file)

        if(!is.null(input$selected_recipe)){
          w$hide()
        }
      }
    )

    output$render_html <- downloadHandler(
      filename = "shopping_list_html.html",
      content = function(file) {

        if(is.null(input$selected_recipe)){
          shinyalert::shinyalert(
            title = "Oops",
            text = "You did not select any recipe. An empty file will be downloaded.",
            type = "error"
          )
        } else {
          w$show()
        }

        file_name <- "shopping_list_html"

        qmd_pkg <- file.path(app_sys("app/quarto"),
                                paste0(file_name, ".qmd"))
        reports_dir <- golem::get_golem_options("reports_dir")
        qmd_copy <- file.path(reports_dir, paste0(file_name, ".qmd"))
        file.copy(qmd_pkg,
                  reports_dir,
                  overwrite = TRUE)

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

        if(!is.null(input$selected_recipe)){
          w$hide()
        }
      }
    )

    data_ing <- reactive({

      req(input$selected_recipe)

      selected_recipes <- input$selected_recipe

      dplyr::tbl(con, "recipe_metadata") |>
        dplyr::filter(recipe_name %in% selected_recipes) |>
        dplyr::inner_join(dplyr::tbl(con, "ingredients_in_recipes"),
                          by = "recipe_id",
                          copy = TRUE
        ) |>
        dplyr::inner_join(dplyr::tbl(con, "ingredients"),
                          by = "ingredient_id",
                          copy = TRUE
        ) |>
        dplyr::inner_join(dplyr::tbl(con, "units"),
                          by = "unit_id",
                          copy = TRUE
        ) |>
        dplyr::select(recipe_name, ingredient, amount, unit) |>
        dplyr::collect() |>
        dplyr::summarise(
          .by = c(ingredient, unit),
          amount = sum(amount)
        )
    })

    tbl_ing <- reactive({
      data_ing <- data_ing()
      validate(need(nrow(data_ing) > 0, "Select recipes"))

      reactable::reactable(
        data_ing,
        defaultPageSize = 50,
        columns = list(
          ingredient = reactable::colDef(name = "Ingredient", width = 150),
          amount = reactable::colDef(name = "Amount", width = 100),
          unit = reactable::colDef(name = "Unit", width = 75)
          ),
        highlight = TRUE
      )
    })

    output$tbl_ing_output <- reactable::renderReactable({
      tbl_ing()
    })

  })
}
