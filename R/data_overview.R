#' data_overview UI Module
#' @description data_overview UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title data_overview_ui
#' @name data_overview_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import DT
#' @export
#'
data_overview_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Data check',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file"), "Upload SNP Data File", accept = c(".csv")),
          actionButton(ns("load_data"), "Load Data")
          )
        ),
      fluidRow(
        column(
          width = 6,  # Half of the row
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Table",
            mainPanel(
              DT::DTOutput(ns("summary_table"))
            )
          )
        ),
        column(
          width = 6,  # Half of the row
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Summary",
            mainPanel(
              DT::DTOutput(ns("summary_stats"))  # 使用 DT 显示统计结果
            )
          )
        )
      )
      )
    )
}
#' data_overview server Module
#' @description data_overview server Module
#' @param input input
#' @param output output
#' @param session session
#' @title data_overview_server
#' @name data_overview_server
#' @import shiny
#' @import DT
#' @import utils
#' @export
#'
data_overview_server <- function(input, output, session) {
  data <- reactiveVal(NULL)

  observeEvent(input$load_data, {
    req(input$file)  # Ensure the file is uploaded
    uploaded_data <- read.csv(input$file$datapath)

    output$summary_table <- DT::renderDT({
      req(uploaded_data)
      datatable(uploaded_data, options = list(pageLength = 10))  # 默认显示6行
    })

    output$summary_stats <- DT::renderDT({
      req(uploaded_data)
      summary_stats <- summary(uploaded_data)  # Calculate summary statistics
      summary_df <- as.data.frame(summary_stats)  # Convert to data frame for DT
      datatable(summary_df, options = list(pageLength = 10))  # 默认显示6行
    })
  })
}
