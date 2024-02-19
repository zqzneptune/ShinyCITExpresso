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
plotGateReduPseudoSort <- function(tblRaw, gnp, listNameColData){
  x_var <-
    colnames(tblRaw)[1]
  y_var <-
    colnames(tblRaw)[2]
  rat <-
    (max(tblRaw[[x_var]]) - min(tblRaw[[x_var]])) / (max(tblRaw[[y_var]]) - min(tblRaw[[y_var]]))

  if(gnp %in% listNameColData[["Group"]]){
    datCell <-
      tblRaw %>%
      group_by(.data[[gnp]]) %>%
      summarise(`xCell` = mean(.data[[x_var]]),
                `yCell` = mean(.data[[y_var]]))

    qBase <-
      ggplot(data = tblRaw,
             aes(x = .data[[x_var]],
                 y = .data[[y_var]],
                 color = .data[[gnp]])) +
      geom_point(size = 0.5, alpha = 0.8) +
      labs(title = gnp) +
      getReduDimTheme() +
      coord_fixed(ratio = rat) +
      scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Spectral"))(length(unique(tblRaw[[gnp]])))) +
      geom_text_repel(
        data = datCell,
        aes(.data[["xCell"]], .data[["yCell"]], label = .data[[gnp]]),
        color = "grey10",
        bg.color = "grey95",
        bg.r = 0.15,
        size = 6,
        seed = 42)

  }else if(gnp %in% listNameColData[["Charac"]]){
    colorPalette <-
      getColorScheme()
    datTbl <-
      tblRaw %>%
      filter(!is.na(.data[[gnp]]))

    qBase <-
      qBase +
      geom_point(size = 0.5, alpha = 0.3, color = "#EEEEEE") +
      geom_point(
        data =
          datTbl,
        aes(x = .data[[x_var]],
            y = .data[[y_var]],
            color = .data[[gnp]]),
        size = 0.1) +
      scale_color_gradientn(colours = colorPalette[["Jet"]])
  }
  plot(qBase)
}


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
