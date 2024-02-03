#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      id = "main",
      title = "MyRecipeManageR",
      theme = bslib::bs_theme(bootswatch = "flatly",
                              version = 5),
        bslib::nav_panel(title = "Choose recipe",
                 mod_choose_recipe_ui("all_recipes")),
        bslib::nav_panel(title = "See recipe",
                  value = "see_recipe",
                 mod_show_recipe_ui("show_recipe")),
        bslib::nav_panel(title = "Create recipe",
                 mod_create_recipe_ui("create_recipe")),
        bslib::nav_panel(title = "Delete recipe",
                       mod_delete_recipe_ui("delete_recipe"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MyRecipeManageR"
    ),
    shinyFeedback::useShinyFeedback()
  )
}
