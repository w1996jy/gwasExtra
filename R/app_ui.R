#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    navbarPage(
      title = "Downstream Analysis of GWAS",  # 应用的主标题
      theme = shinythemes::shinytheme("cerulean"),  # 应用主题
      # customLogo(version = "V.1.0.0"),
      tabPanel("Home"),
      navbarMenu(
        "Data check",
        tabPanel("Data Overview", data_overview_ui("data_overview"))
      ),
      navbarMenu(
        "Plot",
        tabPanel("GWAS result Visualization", manhattan_plot_ui("manhattan_plot"))
      ),
      navbarMenu(
        "SNP Extraction",
        tabPanel("Significant SNP Extraction", SNP_extraction_ui("SNP_extraction"))
      ),
      navbarMenu(
        "Gene map",
        tabPanel("Gene map", gene_map_ui("gene_map"))
      ),
      navbarMenu(
        "Annotate",
        tabPanel("Annotate", annotate_ui("annotate"))
      ),
      navbarMenu(
        "Data merge",
        tabPanel("Data merge", data_merge_ui("data_merge"))
      ),
      tabPanel("Help")  # 帮助页面
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
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "gwasExtra"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
