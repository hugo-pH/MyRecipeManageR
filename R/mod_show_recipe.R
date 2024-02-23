#' show_recipe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_show_recipe_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
      numericInput(ns("new_ser"), "New number of servings:", value = NULL),
      actionButton(
        inputId = ns("change_ser"),
        label = "Change number of servings"
      ),
        uiOutput(ns("siderbar")),
        actionButton(
          inputId = ns("add_ing"),
          label = "Add ingredient"
        ),
        actionButton(
          inputId = ns("add_step"),
          label = "Add step"
        ),
        actionButton(
          inputId = ns("update_recipe"),
          label = "Update recipe"
        ),
        open = "closed"
      ),
      bslib::layout_columns(
        fill = FALSE,
        bslib::value_box(
          title = "Recipe",
          textOutput(ns("recipe_name")),
          showcase = bsicons::bs_icon("card-list"),
          theme = "light"
        ),
        bslib::value_box(
          title = "Source",
          htmlOutput(ns("source")),
          showcase = bsicons::bs_icon("book"),
          theme = "light"
        ),
        bslib::value_box(
          title = "Date",
          textOutput(ns("created_on")),
          showcase = bsicons::bs_icon("calendar"),
          theme = "light"
        ),
        bslib::value_box(
          title = "Category",
          textOutput(ns("category")),
          showcase = bsicons::bs_icon("border"),
          theme = "light"
        ),
        bslib::value_box(
          title = "Servings",
          textOutput(ns("servings")),
          showcase = bsicons::bs_icon("people-fill"),
          theme = "light"
        ),
        bslib::value_box(
          title = "Duration",
          textOutput(ns("duration")),
          showcase = bsicons::bs_icon("alarm"),
          theme = "light"
        ),
        col_widths = c(
          4, 4, 4,
          4, 4, 4
        )
      ),
      bslib::layout_columns(
        bslib::card(
          bslib::card_header("Ingredients"),
          mod_table_ingr_ui(ns("ingr"))
        ),
        bslib::card(
          bslib::card_header("Directions"),
          mod_table_steps_ui(ns("step"))
        ),
        col_widths = c(6, 6)
      )
    )
  )
}

#' show_recipe Server Functions
#'
#' @noRd
mod_show_recipe_server <- function(id, con, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$source <- renderUI({
      validate(need(nrow(rv$data_meta) > 0, "Select a recipe."))
      # If the source is an url, we create a link.
      # We try to extract the domain from the url

      if (stringr::str_detect(rv$data_meta$source, "http|html|www")) {
        pat <- "^(?:https?:\\/\\/)?(?:[^@\\/\\n]+@)?(?:www\\.)?([^:\\/\\n]+)"
        domain <- stringr::str_extract(rv$data_meta$source,
          pattern = pat, group = 1
        )
        word <- ifelse(nchar(domain) > 5,
          domain,
          "Website"
        )

        source <- a(word, href = rv$data_meta$source, target = "_blank")
      } else {
        source <- rv$data_meta$source
      }
      p(source, style = "font-size: 30px;")
    })
    output$created_on <- renderText({

      validate(need(nrow(rv$data_meta) > 0, "Select a recipe."))
      rv$refresh
      stringr::str_extract(
        string = rv$data_meta$created_on,
        pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}"
      )
    })
    output$recipe_name <- renderText({
      validate(need(nrow(rv$data_meta) > 0, "Select a recipe."))
      rv$refresh
      rv$data_meta$recipe_name
    })
    output$servings <- renderText({
      validate(need(nrow(rv$data_meta) > 0, "Select a recipe."))
      rv$refresh
      rv$data_meta$servings
    })
    output$duration <- renderText({
      validate(need(nrow(rv$data_meta) > 0, "Select a recipe."))
      rv$refresh
      rv$data_meta$duration
    })
    output$servings <- renderText({
      validate(need(nrow(rv$data_meta) > 0, "Select a recipe."))
      rv$refresh
      rv$data_meta$servings
    })

    output$category <- renderText({
      validate(need(nrow(rv$data_meta) > 0, "Select a recipe."))
      rv$refresh
      rv$data_meta$category
    })

    output$siderbar <- renderUI({
      validate(need(nrow(rv$data_meta) > 0, "Select a recipe."))

      rv$refresh

      d <- rv$data_meta
      name <- d$recipe_name
      ser <- d$servings
      dur <- d$duration
      src <- d$source
      date <- d$created_on
      cat <- d$category

      cats <- dplyr::tbl(con, "categories") |>
        dplyr::distinct(category) |>
        dplyr::pull(category)

      tagList(
        textInput(ns("name"), "Recipe name:", value = name),
        textInput(ns("dur"), "Duration:", value = dur),
        textInput(ns("src"), "Source:", value = src),
        numericInput(ns("ser"), "Servings:", value = ser),
        selectizeInput(
          ns("cat"),
          label = "Choose a category",
          choices = cats,
          selected = cat,
          multiple = FALSE,
          options = list(create = TRUE)
        )
      )
    })

    observeEvent(rv$selected_recipe_id, {
      selected_recipe <- rv$selected_recipe_id
      req(!is.null(selected_recipe))
      rv$refresh
      rv$data_meta <- dplyr::tbl(con, "recipe_metadata") |>
        dplyr::filter(recipe_id == selected_recipe) |>
        dplyr::inner_join(dplyr::tbl(con, "categories"),
          by = "category_id",
          copy = TRUE
        ) |>
        dplyr::select(-tidyselect::ends_with("id")) |>
        dplyr::collect()

      rv$data_all_ing <- dplyr::tbl(con, "recipe_metadata") |>
        dplyr::filter(recipe_id == selected_recipe) |>
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
        dplyr::select(ingredient, amount, unit, preparation) |>
        dplyr::collect()

      rv$data_all_steps <- dplyr::tbl(con, "recipe_metadata") |>
        dplyr::filter(recipe_id == selected_recipe) |>
        dplyr::inner_join(dplyr::tbl(con, "steps_in_recipes"),
          by = "recipe_id",
          copy = TRUE
        ) |>
        dplyr::select(step, description) |>
        dplyr::arrange(step) |>
        dplyr::collect()
    })

    observeEvent(input$change_ser, {

      req(rv$selected_recipe_id)
      shinyFeedback::feedbackDanger(
        "new_ser",
        is.na(input$new_ser),
        "Please, provide a new number of servings."
      )
      req(input$new_ser)
      req(nrow(rv$data_all_ing) > 0)

      current_servings <- rv$data_meta$servings
      new_servings <- input$new_ser
      rv$data_all_ing <- rv$data_all_ing |>
        dplyr::mutate(
          amount = (new_servings * amount) / current_servings
        )
    })

    # Tables ------------------------------------------------------------------
    rv <- mod_table_ingr_server("ingr", rv, con)
    rv <- mod_table_steps_server("step", rv)

    # Update recipe -----------------------------------------------------------

    observeEvent(input$update_recipe, {

      servings <- ifelse(!is.na(input$new_ser),
                         input$new_ser,
                         input$ser)

      selected_recipe_id <- rv$selected_recipe_id
      new_name <- input$name
      current_recipe <- rv$data_meta$recipe_name
      new_meta <- data.frame(
        recipe_name = new_name,
        source = input$src,
        servings = servings,
        duration = input$dur,
        category = input$cat
      )

      ## Controls -------------------------------------------------------------
      shinyFeedback::feedbackDanger(
        "name",
        nchar(new_name) == 0,
        "Please provide a recipe name."
      )
      req(input$name)
      # if the name has changed, we check if the new name is already present in
      # the database
      if (current_recipe != new_name) {
        recipes_in_db <- dplyr::tbl(con, "recipe_metadata") |>
          dplyr::pull(recipe_name)
        shinyFeedback::feedbackDanger(
          "name",
          input$name %in% recipes_in_db,
          "The provided recipe name is already present in in the db."
        )
        req(!(input$name %in% recipes_in_db))
      }

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

      shinyFeedback::feedbackDanger(
        "new_ing",
        nrow(rv$data_all_ing) == 0,
        "Please add ingredients."
      )
      req(nrow(rv$data_all_ing) > 0)

      shinyFeedback::feedbackDanger(
        "instr",
        nrow(rv$data_all_steps) == 0,
        "Please add instructions."
      )
      req(nrow(rv$data_all_steps) > 0)

      # Brute force the shit out of this. We delete the full recipe and upload
      # it again with the new changes
      delete_query <- glue::glue_sql("
                  DELETE
                  FROM recipe_metadata
                  WHERE recipe_id = {selected_recipe_id}
                                   ",
        .con = con
      )
      del <- DBI::dbExecute(con, delete_query)

      update_database(
        rv$data_all_ing,
        rv$data_all_steps,
        new_meta,
        con
      )
      m <- glue::glue("Recipe {new_name} was added to the database.")
      shinyalert::shinyalert(
        title = "Hooray!",
        text = m,
        type = "success"
      )

      rv$selected_recipe_id <- dplyr::tbl(con, "recipe_metadata") |>
        dplyr::filter(recipe_name == new_name) |>
        dplyr::pull(recipe_id)

      rv$refresh <- stats::rnorm(2)
    })

    ## Add ingredients ---------------------------------------------------------
    observeEvent(input$add_ing, {
      mod_form_ingredient_ui(id = ns("update_recipe_add_ing"), modal = TRUE)
    })

    new_ing <- mod_form_ingredient_server(
      id = "update_recipe_add_ing",
      con = con
    )

    observeEvent(new_ing$submit(), {
      if (is.data.frame(new_ing$new_data())) {
        if (nrow(rv$data_all_ing) == 0) {
          rv$data_all_ing <- new_ing$new_data()
        } else {
          rv$data_all_ing <- dplyr::bind_rows(
            rv$data_all_ing,
            new_ing$new_data()
          )
          rv$refresh <- rnorm(2)
        }
      }
    })

    ## Add steps ---------------------------------------------------------------
    observeEvent(input$add_step, {
      mod_form_step_ui(id = ns("update_recipe_add_step"), modal = TRUE)
    })

    new_step <- mod_form_step_server(
      id = "update_recipe_add_step",
      con = con
    )

    observeEvent(new_step$submit(), {
      if (is.data.frame(new_step$new_data())) {
        if (nrow(rv$data_all_steps) == 0) {
          rv$data_all_steps <- new_step$new_data()
        } else {
          new_step_idx <- new_step$new_data()$step

          rv$data_all_steps <- rv$data_all_steps |>
            dplyr::mutate(
              # Adjust steps order
              step = dplyr::case_when(
                step >= new_step_idx ~ step + 1,
                .default = step
              )
            ) |>
            dplyr::bind_rows(new_step$new_data()) |>
            dplyr::arrange(step)
          rv$refresh <- rnorm(2)
        }
      }
    })
  })
}
