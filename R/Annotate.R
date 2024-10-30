annotate_ui <- function(id) {
  ns <- NS(id)  # 创建命名空间

  fluidPage(
    tags$head(
      tags$style(HTML("
        .full-width-image {
          width: 100%; /* 设置宽度为100% */
          height: auto; /* 高度自适应 */
          object-fit: cover; /* 保持图片比例 */
          margin: 0; /* 去掉默认边距 */
          padding: 0; /* 去掉默认内边距 */
        }
      "))
    ),
    img(src = "https://github.com/w1996jy/gwasExtraFile/blob/main/pic/PixPin_2024-10-29_22-30-06.png?raw=true",
        class = "full-width-image")  # 添加类名以应用 CSS 样式
  )
}
