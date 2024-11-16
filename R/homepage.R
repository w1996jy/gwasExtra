#' homepage UI Module
#' @description homepage UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title homepage_ui
#' @name homepage_ui
#' @import shiny
#' @import bslib
#' @export
#'
homepage_ui <- function(id) {
  fluidPage(
    # Software Introduction Section
    div(style = "font-family: 'Times New Roman'; width: 50%; margin: auto; margin-bottom: 20px; font-size: 16px;",
        h2("Software Introduction", style = "font-size: 20px;"),
        p("gwasExtraFile is a powerful Shiny application designed to help users visualize GWAS results, filter SNP loci, and perform gene localization analysis. Through its intuitive graphical interface, the application enables researchers to efficiently analyze genome-wide association data, quickly identify SNP loci associated with traits, and further localize potential functional genes. By combining the advantages of data processing and visualization, gwasExtraFile provides a convenient tool for genomic research.")
    ),

    # Flowchart Section
    div(style = "text-align: center; margin-top: 20px;",
        img(src = "https://github.com/w1996jy/gwasExtraFile/blob/main/pic/fig1.png?raw=true",
            alt = "Flowchart of gwasExtraFile",
            style = "width: 50%;")
    )
  )
}

