# Return ggplot2 plot for tab: Overview
#' @import ggplot2
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggrepel geom_text_repel
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom dplyr filter
pltOverviewGrp <- function(tblRaw, gnp, visp = "all", fnCol = "Spectral"){
  x_var <-
    colnames(tblRaw)[1]
  y_var <-
    colnames(tblRaw)[2]

  rat <-
    (max(tblRaw[[x_var]]) - min(tblRaw[[x_var]])) / (max(tblRaw[[y_var]]) - min(tblRaw[[y_var]]))

  if(visp == "all"){
    datCell <-
      tblRaw %>%
      group_by(.data[[gnp]]) %>%
      summarise(`xCell` = mean(.data[[x_var]]),
                `yCell` = mean(.data[[y_var]]))

    ggplot(data = tblRaw,
           aes(x = .data[[x_var]],
               y = .data[[y_var]],
               color = .data[[gnp]])) +
      geom_point(size = 0.5, alpha = 0.8) +
      labs(title = gnp) +
      getReduDimTheme(pltType = "grp") +
      coord_fixed(ratio = rat) +
      scale_colour_manual(values = colorRampPalette(brewer.pal(8, fnCol))(length(unique(tblRaw[[gnp]])))) +
      geom_text_repel(
        data = datCell,
        aes(.data[["xCell"]], .data[["yCell"]], label = .data[[gnp]]),
        color = "grey10",
        bg.color = "grey95",
        bg.r = 0.15,
        size = 6,
        seed = 42)
  }else{
    datTbl <-
      tblRaw %>%
      filter(.data[[gnp]] == visp)

    datCell <-
      datTbl %>%
      group_by(.data[[gnp]]) %>%
      summarise(`xCell` = mean(.data[[x_var]]),
                `yCell` = mean(.data[[y_var]]))

    ggplot(data = tblRaw,
           aes(x = .data[[x_var]],
               y = .data[[y_var]])) +
      geom_point(size = 0.5, alpha = 0.8, color = "#CCCCCC") +
      labs(title = gnp) +
      getReduDimTheme() +
      coord_fixed(ratio = rat) +
      geom_point(
        data =
          datTbl,
        aes(x = .data[[x_var]],
            y = .data[[y_var]]),
        size = 0.1,
        alpha = 0.8,
        color = "#b30000") +
      geom_text_repel(
        data = datCell,
        aes(x = .data[["xCell"]], y = .data[["yCell"]], label = .data[[gnp]]),
        color = "grey10",
        bg.color = "grey95",
        bg.r = 0.15,
        size = 6,
        seed = 42)
  }
}

pltOverviewChar <- function(tblRaw, scoreName){
  colorPalette <-
    getColorScheme()
  x_var <-
    colnames(tblRaw)[1]
  y_var <-
    colnames(tblRaw)[2]

  rat <-
    (max(tblRaw[[x_var]]) - min(tblRaw[[x_var]])) / (max(tblRaw[[y_var]]) - min(tblRaw[[y_var]]))

  datTbl <-
    tblRaw %>%
    filter(!is.na(.data[[scoreName]]))

  ggplot(data = tblRaw,
         aes(x = .data[[x_var]],
             y = .data[[y_var]])) +
    labs(title = scoreName) +
    geom_point(size = 0.5, alpha = 0.3, color = "#EEEEEE") +
    getReduDimTheme(pltType = "char") +
    coord_fixed(ratio = rat) +
    guides(color = guide_colorbar(barwidth = 15)) +
    geom_point(
      data =
        datTbl,
      aes(x = .data[[x_var]],
          y = .data[[y_var]],
          color = .data[[scoreName]]),
      size = 0.1) +
    scale_color_gradientn(colours = colorPalette[["Jet"]])
}

pltCharGrpOverview <- function(tblRaw, grpName, charName, fnCol){
  datExp <-
    tblRaw
  p <-
    ggplot(data = datExp,
           aes(x = .data[["group"]],
               y = .data[["chara"]],
               fill = .data[["group"]])) +
    labs(
      x = grpName,
      y = charName
    ) +
    scale_fill_manual(values =
                        rep(colorRampPalette(brewer.pal(8, fnCol))(length(unique(datExp[["group"]]))), 3)) +
    getGrpTheme()
  return(p)
}
