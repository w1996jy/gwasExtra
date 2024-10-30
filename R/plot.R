library(shiny)
library(CMplot)
library(colourpicker)
library(DT)
library(shinyWidgets)

# 定义 UI
manhattan_plot_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Choose SNP data file (.csv or .txt)", accept = c(".csv", ".txt")),
        selectInput(ns("plotType"), "Select Plot Type:",
                    choices = c("Manhattan" = "m", "QQ Plot" = "q", "Circular" = "c")),
        uiOutput(ns("traitSelect")),
        numericInput(ns("threshold"), "Threshold Value:", value = 0.0001, step = 0.0001),
        colourpicker::colourInput(ns("thresholdColor"), "Threshold Line Color:", value = "red"),
        colourpicker::colourInput(ns("highlightColor"), "Highlight Color for Points Below Threshold:", value = "blue"),
        numericInput(ns("height"), "Plot Height (in pixels):", value = 600),
        numericInput(ns("width"), "Plot Width (in pixels):", value = 800),
        actionButton(ns("plotBtn"), "Plot"),
        downloadButton(ns("downloadData"), "Download Table"),
        radioButtons(ns("plotFormat"), "Select Download Format:",
                     choices = c("PNG" = "png", "PDF" = "pdf"),
                     selected = "png"),  # 默认选择PNG
        downloadButton(ns("downloadPlot"), "Download Plot")  # 下载图像按钮
      ),
      mainPanel(
        plotOutput(ns("plot")),
        DT::DTOutput(ns("table"))
      )
    )
  )
}

# 定义服务器逻辑
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

  below_threshold_data <- reactive({
    req(snp_data(), input$trait)
    plot_data <- snp_data()[, c("SNP", "Chromosome", "Position", input$trait)]
    colnames(plot_data)[4] <- "P"
    plot_data[plot_data$P < input$threshold, ]
  })

  output$table <- renderDataTable({
    req(below_threshold_data())
    datatable(below_threshold_data(), options = list(pageLength = 10))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("below_threshold_points_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(below_threshold_data(), file, row.names = FALSE)
    }
  )

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
        png(file, width = input$width, height = input$height)  # 使用用户输入的宽度和高度
        on.exit(dev.off())  # 确保设备关闭
      } else {
        pdf(file, width = input$width / 100, height = input$height / 100)  # 使用用户输入的宽度和高度
        on.exit(dev.off())  # 确保设备关闭
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

# 启动 Shiny 应用
ui <- fluidPage(
  manhattan_plot_ui("manhattan_plot")
)

server <- function(input, output, session) {
  callModule(manhattan_plot_server, "manhattan_plot")
}

shinyApp(ui = ui, server = server)
library(shiny)
library(CMplot)
library(colourpicker)
library(DT)
library(shinyWidgets)

# 定义 UI
manhattan_plot_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Choose SNP data file (.csv or .txt)", accept = c(".csv", ".txt")),
        selectInput(ns("plotType"), "Select Plot Type:",
                    choices = c("Manhattan" = "m", "QQ Plot" = "q", "Circular" = "c")),
        uiOutput(ns("traitSelect")),
        numericInput(ns("threshold"), "Threshold Value:", value = 0.0001, step = 0.0001),
        colourpicker::colourInput(ns("thresholdColor"), "Threshold Line Color:", value = "red"),
        colourpicker::colourInput(ns("highlightColor"), "Highlight Color for Points Below Threshold:", value = "blue"),
        numericInput(ns("height"), "Plot Height (in pixels):", value = 600),
        numericInput(ns("width"), "Plot Width (in pixels):", value = 800),
        actionButton(ns("plotBtn"), "Plot"),
        downloadButton(ns("downloadData"), "Download Table"),
        br(),br(),
        radioButtons(ns("plotFormat"), "Select Download Format:",
                     choices = c("PNG" = "png", "PDF" = "pdf"),
                     selected = "png"),  # 默认选择PNG
        downloadButton(ns("downloadPlot"), "Download Plot")  # 下载图像按钮
      ),
      mainPanel(
        plotOutput(ns("plot")),
        DT::DTOutput(ns("table"))
      )
    )
  )
}

# 定义服务器逻辑
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

  below_threshold_data <- reactive({
    req(snp_data(), input$trait)
    plot_data <- snp_data()[, c("SNP", "Chromosome", "Position", input$trait)]
    colnames(plot_data)[4] <- "P"
    plot_data[plot_data$P < input$threshold, ]
  })

  output$table <- renderDataTable({
    req(below_threshold_data())
    datatable(below_threshold_data(), options = list(pageLength = 10))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("below_threshold_points_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(below_threshold_data(), file, row.names = FALSE)
    }
  )

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
        png(file, width = input$width, height = input$height)  # 使用用户输入的宽度和高度
        on.exit(dev.off())  # 确保设备关闭
      } else {
        pdf(file, width = input$width / 100, height = input$height / 100)  # 使用用户输入的宽度和高度
        on.exit(dev.off())  # 确保设备关闭
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
