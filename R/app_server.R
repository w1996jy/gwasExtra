#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @name app_server
#' @export
#'
app_server <- function(input, output, session) {
  callModule(data_overview_server, "data_overview")
  callModule(manhattan_plot_server, "manhattan_plot")
  callModule(SNP_extraction_server, "SNP_extraction")
  callModule(gene_map_server, "gene_map")
  callModule(data_merge_server, "data_merge")
}

