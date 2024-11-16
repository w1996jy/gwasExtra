#' SNP_extraction UI Module
#' @description SNP_extraction UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title SNP_extraction_ui
#' @name SNP_extraction_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
SNP_extraction_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'SNP extraction',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Choose SNP data file (.csv or .txt)", accept = c(".csv", ".txt")),
          numericInput(ns("threshold"), "Threshold Value:", value = 0.0001, step = 0.0001),
          actionButton(ns("extractBtn"), "Extract Data"),
          br(),
          downloadButton(ns("downloadData"), "Download")
          )
        ),
      mainPanel(
        DT::DTOutput(ns("table"))
      )
      )
    )
}

#' SNP_extraction server Module
#' @description SNP_extraction server Module
#' @param input description
#' @param output description
#' @param session description
#' @title SNP_extraction_server
#' @name SNP_extraction_server
#' @import shiny
#' @import DT
#' @import utils
#' @export
#'
SNP_extraction_server <- function(input, output, session) {
  ns <- session$ns
  snp_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    validate(need(ext == "csv" || ext == "txt", "Please upload a .csv or .txt file"))
    read.csv(input$file$datapath)
  })

  below_threshold_data <- reactive({
    req(snp_data())
    # 使用正确的列名替代 "P"
    plot_data <- snp_data()[, c("SNP", "Chromosome", "Position", "trait1")]  # 使用 trait1 作为阈值列
    colnames(plot_data)[4] <- "P"  # 将 trait1 列重命名为 P
    plot_data$P <- as.numeric(plot_data$P)  # 确保 P 列是数字格式
    plot_data[plot_data$P < input$threshold, ]
  })

  output$table <- renderDataTable({
    req(below_threshold_data())
    datatable(below_threshold_data(), options = list(pageLength = 10))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("below_threshold_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(below_threshold_data(), file, row.names = FALSE)
    }
  )
}


