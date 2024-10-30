library(DT)

data_overview_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Data Overview"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload SNP Data File", accept = c(".csv")),
        actionButton(ns("load_data"), "Load Data")
      ),
      mainPanel(
        DT::DTOutput(ns("summary_table")),
        DT::DTOutput(ns("summary_stats"))  # 使用 DT 显示统计结果
      )
    )
  )
}

data_overview_server <- function(input, output, session) {
  data <- reactiveVal(NULL)

  observeEvent(input$load_data, {
    req(input$file)  # Ensure the file is uploaded
    data(read.csv(input$file$datapath))  # Read the data

    output$summary_table <- renderDT({
      req(data())
      datatable(data(), options = list(pageLength = 6))  # 默认显示6行
    })

    output$summary_stats <- renderDT({
      req(data())
      summary_stats <- summary(data())  # Calculate summary statistics
      summary_df <- as.data.frame(summary_stats)  # Convert to data frame for DT
      datatable(summary_df, options = list(pageLength = 6))  # 默认显示6行
    })
  })
}
