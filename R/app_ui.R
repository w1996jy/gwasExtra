#' @title Shiny App UI Function
#' @description This function defines the user interface (UI) for the Shiny app.
#' @param request Shiny request
#' @name app_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @export
#'
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    page_navbar(
      theme = bs_theme(bootswatch = "lumen"),
      title = "gwasExtra",
      nav_panel(
        "Home page",
        icon = bs_icon("bank")
      ),
      nav_menu(
        "Data check",
        icon = bs_icon("card-checklist"),
        data_overview_ui("data_overview")
      ),
      nav_menu(
        "Plot",
        icon = bs_icon("tools"),
        manhattan_plot_ui("manhattan_plot")
      ),
      nav_menu(
        "SNP Extraction",
        icon = bs_icon("wrench"),
        SNP_extraction_ui("SNP_extraction")
      ),
      nav_menu(
        "Gene map",
        icon = bs_icon("check2-all"),
        gene_map_ui("gene_map")
      ),
      nav_menu(
        "Annotate",
        icon = bs_icon("rocket-takeoff"),
        annotate_ui("annotate")
      ),
      nav_panel(
        "Data merge",
        icon = bs_icon("file-earmark-bar-graph"),
        data_merge_ui("data_merge")
      ),
      nav_panel(
        "Help",
        icon = bs_icon("exclamation-circle")
      )
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
#' @name golem_add_external_resources
#' @export
#'
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "gwasExtra"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
