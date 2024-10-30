library(shiny)
library(data.table)
library(tidyverse)
library(purrr)
library(DT)

# Define gene_map_ui as a function with an id parameter
gene_map_ui <- function(id) {
  ns <- NS(id)  # Create a namespace function

  fluidPage(
    titlePanel("SNP and Gene Information Analysis"),

    sidebarLayout(
      sidebarPanel(
        fileInput(ns("snp_file"), "Upload SNP Data File (CSV):",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        fileInput(ns("gff3_file"), "Upload GFF3 Data File (CSV):",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        numericInput(ns("window_size"), "Enter Window Size (default is 30000):", value = 30000),
        actionButton(ns("run"), "Run"),  # Button label
        downloadButton(ns("download_data"), "Download Results")
      ),

      mainPanel(
        DT::DTOutput(ns("result_table"))  # Use DTOutput to display the table
      )
    )
  )
}

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

    output$result_table <- renderDT({
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
