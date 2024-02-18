# Return ggplot2 plot for tab 3
#' @import ggplot2
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggrepel geom_text_repel
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom dplyr filter
pltReduGatePseudoSort <- function(tblDat, tblBrush, markerBrush, xLab, yLab){

  qBase <-
    ggplot(
      data = tblDat,
      aes(
        x = .data[["x_var"]],
        y = .data[["y_var"]]
      )
    ) +
    labs(
      x = xLab,
      y = yLab
    ) +
    geom_point(size = 0.5, alpha = 0.8, color = "#999999") +
    getReduDimTheme()

  if(nrow(tblBrush) != 0){
    plot(
      qBase +
        geom_point(
          data = markerBrush,
          aes(
            x = .data[["x_var"]],
            y = .data[["y_var"]]
          ),
          color = "#0000FF"
        )
    )
  }else{
    plot(qBase)
  }
}
