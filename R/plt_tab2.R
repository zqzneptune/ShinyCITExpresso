# Return ggplot2 plot for tab 2
#' @import ggplot2
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggrepel geom_text_repel
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom dplyr filter


pltReduFeatureViz <- function(tblRaw, geneName, colFn){
  colorPalette <-
    getColorScheme()
  x_var <-
    colnames(tblRaw)[1]
  y_var <-
    colnames(tblRaw)[2]
  rat <-
    (max(tblRaw[[x_var]]) - min(tblRaw[[x_var]])) / (max(tblRaw[[y_var]]) - min(tblRaw[[y_var]]))

  ggplot(data = tblRaw,
         aes(x = .data[[x_var]],
             y = .data[[y_var]],
             color = .data[["gene"]])) +
    labs(title = geneName) +
    geom_point(size = 0.5) +
    scale_color_gradientn(
      colors = colorPalette[[colFn]],
      na.value = "#EEEEEE"
    ) +
    getReduDimTheme() +
    guides(color = guide_colorbar(barwidth = 15)) +
    coord_fixed(ratio = rat)
}


pltGrpFeatureViz <- function(tblRaw, grpName, fnCol, fnTitle){
  datExp <-
    tblRaw[tblRaw[["group"]] %in% grpName, ]

  datExp[["group"]] <-
    factor(datExp[["group"]], levels = grpName)

  p <-
    ggplot(data = datExp,
           aes(x = .data[["group"]],
               y = .data[["exprs"]],
               fill = .data[["group"]])) +
      labs(
        title = fnTitle,
        x = "",
        y = "Normalized Expression Level"
      ) +
      scale_fill_manual(values =
                          rep(colorRampPalette(brewer.pal(8, fnCol))(length(unique(datExp[["group"]]))), 3)) +
    getGrpTheme()
  return(p)
}
