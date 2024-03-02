#'
#' @import shiny
#' @import bslib
#' @import bsicons


getUItabl5 <- function(){
  colorPalette <-
    getColorScheme()
  return(
    nav_panel(
      title = "MultiMk",
      value = "multimk",
      icon = icon("blender"),

      fluidRow(
        column(
          width = 2,
          selectInput(
            "groupMultiMk",
            choices = NULL,
            label = "Group Name"
          ),
        ),
        column(
          width = 2,
          selectizeInput(
            "layerMultiMk",
            choices = NULL,
            label = "Select modality"
          )
        ),

        column(
          width = 6,
          selectizeInput(
            "mkMultiMk",
            width = "100%",
            choices = NULL,
            multiple = TRUE,
            label = "Select, or type feature name"
          )
        ),
        column(
          width = 2,
          actionButton("plotGrpMultiMk", "Plot")
        )
      ),

      card(
        card_header(
          "Features across Groups"
        ),
        card_body(
          plotOutput(
            "grpMultiMk",
            height = "660px"
          )
        )
      )

    )

  )
}
