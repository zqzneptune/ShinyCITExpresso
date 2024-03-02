# Return ggplot2 plot for tab 3
#' @import ggplot2
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace_all
#' @importFrom dplyr full_join
#' @importFrom tidyr spread
#' @importFrom stats as.dendrogram
#' @importFrom shiny validate
#'
pltGrpMultiMk <- function(rawData){
  if(ncol(rawData) >2){
    datData <-
      tidyr::gather(
        data = rawData[!is.na(rawData[["groups"]]), ],
        key = "gene",
        value = "expression",
        -1
      ) %>%
      dplyr::mutate(
        `groups` =
          stringr::str_replace_all(
            string = `groups`,
            pattern = ":",
            replacement = "_"
          ),
        `gene` =
          stringr::str_replace_all(
            string = `gene`,
            pattern = ":",
            replacement = "_"
          )
      )


    avgCnt <-
      datData %>%
      dplyr::filter(`expression` != 0 ) %>%
      dplyr::group_by(`groups`, `gene`) %>%
      dplyr::summarise(`avgExp` = mean(`expression`), .groups = "drop_last") %>%
      dplyr::mutate(`gropGene` =
                      paste(`groups`, `gene`, sep = ":"))


    propCnt <-
      datData %>%
      dplyr::mutate(`expression` =
                      ifelse(`expression` != 0, 1, 0)) %>%
      dplyr::group_by(`groups`, `gene`) %>%
      dplyr::summarise(`propExp` = mean(`expression`), .groups = "drop_last") %>%
      dplyr::mutate(`gropGene` =
                      paste(`groups`, `gene`, sep = ":"))

    tblStats <-
      dplyr::full_join(
        avgCnt[, c("gropGene", "avgExp")],
        propCnt[, c("gropGene", "propExp")],
        by = "gropGene"
      ) %>%
      tidyr::separate(
        data = .,
        col = "gropGene",
        into = c("groups", "gene"),
        sep = ":"
      )


    tblAvgCnt <-
      tidyr::spread(
        data = tblStats[, c("groups", "gene", "avgExp")],
        key = `gene`,
        value = `avgExp`
      )
    mAvgCnt <-
      as.matrix(tblAvgCnt[, -1])

    rownames(mAvgCnt) <-
      tblAvgCnt[["groups"]]
    dPlot <-
      tblStats
    if(nrow(mAvgCnt) > 1){
      mAvgCnt[is.na(mAvgCnt)] <- 0
      hcRow <-
        as.dendrogram(hclust(dist(t(mAvgCnt))))
      hcCol <-
        as.dendrogram(hclust(dist(mAvgCnt)))

      dPlot[["groups"]] <-
        factor(dPlot[["groups"]], levels = labels(hcCol))
      dPlot[["gene"]] <-
        factor(dPlot[["gene"]], levels = labels(hcRow))
    }


    ggplot(
      dPlot,
      aes(
        x = .data[["groups"]],
        y = .data[["gene"]],
        size = .data[["propExp"]],
        color = .data[["avgExp"]]
      )
    ) +
      geom_point() +
      scale_size_continuous(
        name = "Fraction of cells \nin group",
        range = c(0, 10),
        limits = c(0, 1),
        breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)
      ) +
      scale_color_gradientn(
        name = "Average expression \nin group",
        # limits = c(0, 10),
        colours = c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')
      ) +
      getGrpTheme() +
      theme(
        axis.ticks =  element_line(colour = "black", linewidth = 1),
        axis.title =  element_blank(),#element_text(face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        legend.key = element_rect(colour = NA, fill = NA))
  }

}
