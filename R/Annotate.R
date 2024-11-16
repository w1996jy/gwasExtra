#' annotate UI Module
#' @description annotate UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title annotate_ui
#' @name annotate_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @export
#'
annotate_ui <- function(id) {
  ns <- NS(id)  # 创建命名空间
  nav_panel(
    title = 'Annotate',
    icon = bs_icon("play-circle"),
    tags$head(
      tags$style(HTML("
        .full-width-image {
          width: 100%;
          height: auto;
          object-fit: cover;
          margin: 0;
          padding: 0;
        }
      "))
    ),
    img(src = "https://github.com/w1996jy/gwasExtraFile/blob/main/pic/PixPin_2024-10-29_22-30-06.png?raw=true",
        class = "full-width-image")  # 添加类名以应用 CSS 样式
    )
}
