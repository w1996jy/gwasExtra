#' manhattan_plot UI Module
#' @description manhattan_plot UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title manhattan_plot_ui
#' @name manhattan_plot_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import colourpicker
#' @export
#'
manhattan_plot_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Plot',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Choose SNP data file (.csv or .txt)", accept = c(".csv", ".txt"))
          )
        ),
      fluidRow(
        column(
          width = 12,  # Half of the row
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table",
            sidebar = accordion(
              accordion_panel(
                title = "Parameter",
                selectInput(ns("plotType"), "Select Plot Type:",
                            choices = c("Manhattan" = "m", "QQ Plot" = "q", "Circular" = "c")),
                uiOutput(ns("traitSelect")),
                numericInput(ns("threshold"), "Threshold Value:", value = 0.0001, step = 0.0001),
                colourpicker::colourInput(ns("thresholdColor"), "Threshold Line Color:", value = "red"),
                colourpicker::colourInput(ns("highlightColor"), "Highlight Color for Points Below Threshold:", value = "blue")
              ),
            accordion_panel(
              title = "Download",
              numericInput(ns("height"), "Plot Height (in pixels):", value = 600),
              numericInput(ns("width"), "Plot Width (in pixels):", value = 800),
              radioButtons(ns("plotFormat"), "Select Download Format:",
                           choices = c("PNG" = "png", "PDF" = "pdf"),
                           selected = "pdf"),
              downloadButton(ns("downloadPlot"), "Download"))
            ),
            mainPanel(
              plotOutput(ns("plot"))
            )
          )
        )
      )
      )
    )

}

#' manhattan_plot server Module
#' @description manhattan_plot server Module
#' @param input description
#' @param output description
#' @param session description
#' @title manhattan_plot_server
#' @name manhattan_plot_server
#' @import shiny
#' @import DT
#' @import CMplot
#' @import grDevices
#' @export
#'
utils::globalVariables(c("Chromosome", "P"))

manhattan_plot_server <- function(input, output, session) {
  snp_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    validate(need(ext == "csv" || ext == "txt", "Please upload a .csv or .txt file"))
    read.csv(input$file$datapath)
  })

  output$traitSelect <- renderUI({
    req(snp_data())
    trait_columns <- colnames(snp_data())[4:ncol(snp_data())]  # Traits assumed to start from 4th column
    selectInput(inputId = session$ns("trait"), label = "Select Trait:", choices = trait_columns)
  })

  output$plot <- renderPlot({
    req(snp_data(), input$trait)
    input$plotBtn  # Ensure plot only renders on button click

    plot_data <- snp_data()[, c("SNP", "Chromosome", "Position", input$trait)]
    colnames(plot_data)[4] <- "P"
    low_snp <- plot_data$SNP[plot_data$P < input$threshold]

    CMplot(plot_data,
           plot.type = input$plotType,
           height = input$height / 100,
           width = input$width / 100,
           file.output = FALSE,
           threshold = input$threshold,
           threshold.col = input$thresholdColor,
           highlight = low_snp,
           highlight.col = input$highlightColor)
  })

  # 添加下载图片的处理
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("manhattan_plot_", Sys.Date(), ".", input$plotFormat, sep = "")
    },
    content = function(file) {
      if (input$plotFormat == "png") {
        png(file, width = input$width, height = input$height)
        on.exit(dev.off())
      } else {
        pdf(file, width = input$width / 100, height = input$height / 100)
        on.exit(dev.off())
      }

      plot_data <- snp_data()[, c("SNP", "Chromosome", "Position", input$trait)]
      colnames(plot_data)[4] <- "P"
      low_snp <- plot_data$SNP[plot_data$P < input$threshold]

      CMplot(plot_data,
             plot.type = input$plotType,
             height = input$height / 100,
             width = input$width / 100,
             file.output = FALSE,
             threshold = input$threshold,
             threshold.col = input$thresholdColor,
             highlight = low_snp,
             highlight.col = input$highlightColor)
    }
  )
}
