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

      layout_columns(
        col_widths = breakpoints(
          sm = c(12, 12),
          md = c(12, 12),
          lg = c(3, 9)
        ),


        card(
          card_header(
            "Choose feature:"
          ),
          card_body(
            selectizeInput(
              "layerFeatureViz",
              choices = NULL,
              label = "Select modality"
            ),
            selectizeInput(
              "mkFeatureViz",
              choices = NULL,
              label = "Select, or type feature name (e.g. TP53)"
            )
          )
        ),

        card(
          height = "750px",
          card_header(
            "by Reduced Dimensions",
            popover(
              title = "Settings",
              bs_icon("gear"),
              selectizeInput(
                "colorReduFeatureViz",
                choices =
                  NULL,
                label = "Color Palette"
              ),

              hr(),
              selectInput(
                inputId = "layoutReduFeatureViz",
                choices = NULL,
                label = "Layout"
              ),
            )
          ),
          card_body(
            plotOutput("reduFeatureViz")
          ),
          full_screen = TRUE
        )

      ),

      card(
        height = "750px",
        card_header(
          "by Groups (e.g. clusters, batches, cell-types)",
          popover(
            title = "Settings",
            bs_icon("gear"),
            p("Plot type:"),
            radioButtons(
              inputId = "pltTypeGrpFeatureViz",
              label = "",
              choices =
                c(
                  "Box plot" = "box",
                  "Violin plot" = "violin"
                ),
              selected = "box"
            )
          )
        ),
        card_body(
          fluidRow(
            column(
              width = 4,
              selectInput(
                "groupByGrpFeatureViz",
                choices = NULL,
                label = "Group Name"
              )
            ),
            column(
              width = 8,
              selectInput(
                inputId = "gnpNamesGrpFeatureViz",
                label = "Select/deselect to reorder",
                choices = NULL,
                multiple = TRUE
              )
            )
          ),
          plotOutput("grpFeatureViz")
        ),
        full_screen = TRUE
      )

    )

  )
}
