# Return ggplot2 themes applied globally
#' @import ggplot2
#'
getReduDimTheme <- function(){
  return(theme_classic() +
           theme(
             text = element_text(size = 18, family = "Helvetica"),
             panel.background = element_rect(fill = "white", colour = NA),
             axis.line = element_line(colour = "black"),
             axis.ticks = element_line(colour = "black"),
             axis.text = element_text(size = 18),
             plot.title = element_text(hjust = 0, face = "bold"),
             legend.position = "bottom",
             legend.title =
               element_blank(),
             legend.key = element_rect(colour = NA, fill = NA),
             axis.text.x = element_blank(), axis.ticks.x = element_blank(),
             axis.text.y = element_blank(), axis.ticks.y = element_blank()
           ))
}

getGrpTheme <- function(){
  return(
    theme_classic() +
      theme(
        text = element_text(size = 18, family = "Helvetica"),
        panel.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 18),
        plot.title = element_text(hjust = 0, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
  )

}

# Return global color scheme

getColorScheme <- function(){
  colorPalette <-
    lapply(list(c("#00007F", "blue", "#007FFF", "cyan",
                "#7FFF7F", "yellow", "#FF7F00", "red",
                "#7F0000"),
              c("grey85", "#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84",
                "#FC8D59", "#EF6548", "#D7301F", "#B30000", "#7F0000"
              ),
              c("#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
                "#FEE090", "#FDAE61", "#F46D43", "#D73027"
              ),
              c(
                "#FDE725", "#AADC32", "#5DC863", "#27AD81", "#21908C",
                "#2C728E", "#3B528B", "#472D7B", "#440154"
              )), function(x){
                return(colorRampPalette(x)(20))
              })
  names(colorPalette) <-
    c("Jet", "White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple")
  return(colorPalette)
}

#' @import MultiAssayExperiment
#' @import SingleCellExperiment
#' @importFrom BiocGenerics Reduce

getListColData <- function(mae){
  listColData <-
    Reduce(append, lapply(experiments(mae), function(sce){
      return(colData(sce)@listData)
    }))
  listColData <-
    append(listColData, colData(mae)@listData)
  listColData <-
    listColData[!duplicated(names(listColData))]
  return(listColData)
}



getNameColData <- function(listColData){
  listNames <-
    list()
  clsColData <-
    unlist(lapply(listColData, class))

  listNames[["Group"]] <-
    names(clsColData)[clsColData %in% c("factor", "character")]

  listNames[["Charac"]] <-
    names(clsColData)[clsColData %in% c("integer", "numeric")]

  listNames[["Traj"]] <-
    names(clsColData)[grepl("pseudotime", names(clsColData))]
  return(listNames)
}


getListReducedDim <- function(mae){
  listReducedDim <-
    Reduce(append, lapply(experiments(mae), function(sce){
      return(reducedDims(sce))
    }))

  listReducedDim <-
    listReducedDim[!duplicated(names(listReducedDim))]
  return(listReducedDim)
}

getPrfReducedName <- function(avaReducedName){
  prfReducedName <-
    avaReducedName[grepl("(umap|pca|tsne|t-sne|fa|fr)", avaReducedName, ignore.case = TRUE)]

  if(prfReducedName == ""){
    prfReducedName <-
      avaReducedName[1]
  }
  return(prfReducedName)
}

#' @importFrom SummarizedExperiment assay
getListAssayMarkers <- function(mae){
  listAssayMarkers <-
    lapply(experiments(mae), function(sce){
      return(rownames(SummarizedExperiment::assay(sce)))
    })

}

getFeatureExpression <- function(mae, layer, feature){
  return(assay(mae[[layer]])[feature, ])
}

getTblReducedDim <- function(listReducedDim, fnReduceDim){
  tblRaw <-
    as.data.frame(listReducedDim[[fnReduceDim]][, c(1, 2)])
  colnames(tblRaw) <-
    paste(fnReduceDim, c(1, 2))
  return(tblRaw)
}

#' @importFrom magrittr %>%
#' @importFrom dplyr filter
rvZeroDat <- function(tblRaw){
  tblDat <-
    tblRaw %>%
    dplyr::filter(.data[["x_var"]] != 0) %>%
    dplyr::filter(.data[["y_var"]] != 0)
  return(tblDat)
}