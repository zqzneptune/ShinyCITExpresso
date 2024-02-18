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

pltOverallCrossModal <- function(tblDat, xLab, yLab, fnCol, corLm){

  qBase <-
    ggplot(data = tblDat,
           aes(x = scale(.data[["x_var"]], center = FALSE),
               y = scale(.data[["y_var"]], center = FALSE))) +
    geom_point(aes(color = .data[["group"]])) +
    scale_colour_manual(values = colorRampPalette(brewer.pal(8, fnCol))(length(unique(tblDat[["group"]])))) +
    labs(
      x = xLab,
      y = yLab
    ) +
    theme_classic() +
    theme(legend.position = "none",
          text =
            element_text(size = 14),
          plot.title =
            element_text(size = 24))
  if(corLm){
    plot(
      qBase +
        geom_smooth(
          method = lm,
          se = FALSE,
          fullrange = FALSE,
          linetype = "dashed",
          color = "#000000"
        )
    )
  }else{
    plot(qBase)
  }

}

#' @importFrom plyr ddply
#' @importFrom dplyr summarise
#' @importFrom stats cor
pltWrapCrossModal <- function(tblDat, xLab, yLab, fnCol, corLm){

  corsVal <-
    ddply(
      .data = tblDat,
      .variables = "group",
      .fun = summarise,
      cor = round(cor(.data[["x_var"]], .data[["y_var"]]), 3)
    )
  qBase <-
    ggplot(data = tblDat,
           aes(x = scale(.data[["x_var"]], center = FALSE),
               y = scale(.data[["y_var"]]), center = FALSE)) +
    geom_point(aes(color = .data[["group"]])) +
    scale_colour_manual(values = colorRampPalette(brewer.pal(8, fnCol))(length(unique(tblDat[["group"]])))) +
    facet_wrap( ~ .data[["group"]]) +
    theme_classic() +
    theme(legend.position = "none",
          text =
            element_text(size = 14),
          plot.title =
            element_text(size = 24),
          axis.title =
            element_blank())
  qXrange <- #https://stackoverflow.com/questions/7705345/how-can-i-extract-plot-axes-ranges-for-a-ggplot2-object
    ggplot_build(qBase)$layout$panel_params[[1]]$x.range
  qYrange <-
    ggplot_build(qBase)$layout$panel_params[[1]]$y.range
  corsX <-
    (qXrange[2] - qXrange[1])/2
  corsY <-
    qYrange[2]*0.9
  qCorsVal <-
    qBase +
    geom_text(
      data = corsVal,
      aes(
        label = paste("r=", .data[["cor"]])),
      x = corsX,
      y = corsY
    )

  if(corLm){
    plot(
      qCorsVal +
        geom_smooth(
          method = lm,
          se = FALSE,
          fullrange = FALSE,
          linetype = "dashed",
          color = "#000000"
        )
    )
  }else{
    plot(qCorsVal)
  }

}
