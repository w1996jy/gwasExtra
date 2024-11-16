#' data_merge UI Module
#' @description data_merge UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title data_merge_ui
#' @name data_merge_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @export
#'
data_merge_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Data merge',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          fileInput(ns("file1"), "Upload the first dataset (CSV or XLSX)", accept = c(".csv", ".xlsx")),
          fileInput(ns("file2"), "Upload the second dataset (CSV or XLSX)", accept = c(".csv", ".xlsx"))
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
                selectInput(ns("join_type"), "Select Join Type", choices = c("left_join", "right_join", "inner_join", "full_join", "anti_join", "semi_join")),
                uiOutput(ns("key_columns1")),  # UI output for first dataset key column
                uiOutput(ns("key_columns2")),  # UI output for second dataset key column
                ),
              accordion_panel(
                title = "Download",
                downloadButton(ns("download_data"), "Download")  # Download button
              )
              ),
            mainPanel(
              DT::DTOutput(ns("result"))  # Use DTOutput for displaying results
            )
            )
          )
        )
      )
    )
}

#' data_merge server Module
#' @description data_merge server Module
#' @param input description
#' @param output description
#' @param session description
#' @title data_merge_server
#' @name data_merge_server
#' @import shiny
#' @import DT
#' @import dplyr
#' @import tools
#' @import utils
#' @import readxl
#' @importFrom stats setNames
#' @export
#'
data_merge_server <- function(input, output, session) {

  data1 <- reactive({
    req(input$file1)

    # Determine the file type and read accordingly
    ext <- tools::file_ext(input$file1$name)
    if (ext == "csv") {
      read.csv(input$file1$datapath)
    } else if (ext == "xlsx") {
      read_excel(input$file1$datapath)  # Use readxl to read Excel files
    } else {
      stop("Unsupported file type")
    }
  })

  data2 <- reactive({
    req(input$file2)

    # Determine the file type and read accordingly
    ext <- tools::file_ext(input$file2$name)
    if (ext == "csv") {
      read.csv(input$file2$datapath)
    } else if (ext == "xlsx") {
      read_excel(input$file2$datapath)  # Use readxl to read Excel files
    } else {
      stop("Unsupported file type")
    }
  })

  output$key_columns1 <- renderUI({
    req(data1())
    selectInput(session$ns("by_column1"), "Select Join Key Column from Dataset 1", choices = names(data1()), selected = names(data1())[1])
  })

  output$key_columns2 <- renderUI({
    req(data2())
    selectInput(session$ns("by_column2"), "Select Join Key Column from Dataset 2", choices = names(data2()), selected = names(data2())[1])
  })

  joined_data <- reactive({
    req(data1(), data2(), input$by_column1, input$by_column2, input$join_type)
    by_column1 <- input$by_column1
    by_column2 <- input$by_column2

    # Use the selected key columns for joining
    switch(input$join_type,
           "left_join" = left_join(data1(), data2(), by = setNames(by_column2, by_column1)),
           "right_join" = right_join(data1(), data2(), by = setNames(by_column2, by_column1)),
           "inner_join" = inner_join(data1(), data2(), by = setNames(by_column2, by_column1)),
           "full_join" = full_join(data1(), data2(), by = setNames(by_column2, by_column1)),
           "anti_join" = anti_join(data1(), data2(), by = setNames(by_column2, by_column1)),
           "semi_join" = semi_join(data1(), data2(), by = setNames(by_column2, by_column1))
    )
  })

  output$result <- DT::renderDT({
    req(joined_data())
    datatable(joined_data(), options = list(pageLength = 10, autoWidth = TRUE))  # Use DT to render data
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste("merged_data_", Sys.Date(), ".csv", sep = "")  # Specify download file name
    },
    content = function(file) {
      write.csv(joined_data(), file, row.names = FALSE)  # Save merged data as CSV
    }
  )
}

