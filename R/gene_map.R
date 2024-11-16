#' gene_map UI Module
#' @description gene_map UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title gene_map_ui
#' @name gene_map_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import tidyverse
#' @export
#'
# Define gene_map_ui as a function with an id parameter
gene_map_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Gene map',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("snp_file"), "Upload SNP Data File (CSV):",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          fileInput(ns("gff3_file"), "Upload GFF3 Data File (CSV):",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
          )
        ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table of result",
            sidebar = accordion(
              accordion_panel(
                title = "Parameter",
                numericInput(ns("window_size"), "Enter Window Size (default is 300000):", value = 300000),
                ),
              accordion_panel(
                title = "Run",
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("download_data"), "Download")
              )
              ),
            mainPanel(
              DT::DTOutput(ns("result_table"))
            )
            )
          )
        )
      )
    )
}


#' gene_map server Module
#' @description gene_map server Module
#' @param input description
#' @param output description
#' @param session description
#' @title gene_map_server
#' @name gene_map_server
#' @import shiny
#' @import DT
#' @import data.table
#' @import purrr
#' @export
#'
gene_map_server <- function(input, output, session) {
  result_data <- reactiveVal(NULL)

  observeEvent(input$run, {  # Update event to match button label
    req(input$snp_file)
    req(input$gff3_file)

    # Read SNP data
    snp_data <- fread(input$snp_file$datapath) %>%
      dplyr::select(Chromosome, Position, P)

    # Read GFF3 data
    gff3_data <- fread(input$gff3_file$datapath)

    # Extract gene list with user-defined window size
    gene_list_V2 <- map2_df(snp_data$Position, snp_data$Chromosome, ~{
      gff3_data %>%
        filter(Chromosome == .y & .x - Start <= input$window_size & End - .x <= input$window_size) %>%
        mutate(Position = .x,
               QTL = paste0(.x - input$window_size, "--", .x + input$window_size))
    })

    result_data(gene_list_V2)

    output$result_table <- DT::renderDT({
      datatable(result_data(), options = list(pageLength = 10,
                                              autoWidth = TRUE,
                                              searchable = TRUE))
    })
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste("gene_list_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(result_data())
      fwrite(result_data(), file)
    }
  )
}
