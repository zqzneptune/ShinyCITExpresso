#'
#' @import shiny
#' @import bslib
#' @import bsicons


getUItabl2 <- function(){
  colorPalette <-
    getColorScheme()
  return(
    nav_panel(
      title = "FeatureViz",
      value = "featureviz",

      fluidRow(
        column(
          width = 4,
          selectizeInput(
            "layerFeatureViz",
            choices = NULL,
            label = "Select modality"
          )
        ),

        column(
          width = 8,
          selectizeInput(
            "mkFeatureViz",
            choices = NULL,
            label = "Select, or type feature name (e.g. TP53)"
          )
        )
      ),

      card(
        card_header(
          "by Reduced Dimensions"
        ),

        card_body(
          fluidRow(
            column(
              width = 9,
              plotOutput(
                "reduFeatureViz",
                height = "560px"
              ),
            ),
            column(
              width = 3,
              selectInput(
                inputId = "layoutReduFeatureViz",
                choices = NULL,
                label = "Layout"
              ),
              hr(),
              selectizeInput(
                "colorReduFeatureViz",
                choices =
                  NULL,
                label = "Color Palette"
              ),
              actionButton("plotReduFeatureViz", "Plot Redu")
            )
          )
        ),
        full_screen = TRUE
      ),

      card(
        card_header(
          "by Groups (e.g. clusters, batches, cell-types)"
        ),
        card_body(
          fluidRow(
            column(
              width = 3,
              selectInput(
                "groupByGrpFeatureViz",
                choices = NULL,
                label = "Group Name"
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "gnpNamesGrpFeatureViz",
                label = "Select/deselect to reorder",
                choices = NULL,
                multiple = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                inputId = "pltTypeGrpFeatureViz",
                label = "Plot type:",
                choices =
                  c(
                    "Box plot" = "box",
                    "Violin plot" = "violin"
                  ),
                selected = "box"
              )
            ),
            column(
              width = 2,
              actionButton("plotGrpFeatureViz", "Plot Group")
            )
          ),
          hr(),
          plotOutput(
            "grpFeatureViz",
            height = "560px"
          )
        ),
        full_screen = TRUE
      )

    )

  )
}
