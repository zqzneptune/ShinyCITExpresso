#'
#' @import shiny
#' @import bslib
#' @import bsicons


getUItabl4 <- function(){
  return(
    nav_panel(
      title = "PseudoSort",
      value = "pseudosort",

      layout_columns(
        col_widths = breakpoints(
          sm = c(12, 12),
          md = c(12, 12),
          lg = c(4, 8)
        ),

        card(
          card_header(
            "Reduced Dimensions",
            popover(
              title = "Settings",
              bs_icon("gear"),
              selectInput(
                "fnColPseudoSort",
                choices = NULL,
                label = "Variable Name"
              ),
              selectInput(
                inputId = "layoutPseudoSort",
                choices = NULL,
                label = "Layout"
              ),
            )
          ),

          card_body(
            plotOutput(
              "gateReduPseudoSort",
              brush = "gateReduPseudoSortBrush"
            )
          ),
          fill = FALSE
        ),

        card(
          card_header(
            "Gating Features (click and drag the ReduDim plot after gate selections)"
          ),
          card_body(
            fluidRow(
              column(
                width = 6,
                p("X:"),
                selectizeInput(
                  "xLayerPseudoSort",
                  choices = NULL,
                  selected = "",
                  label = "Modality"
                ),
                selectizeInput(
                  "xMarkerPseudoSort",
                  choices = NULL,
                  selected = "",
                  label = "Feature"
                )
              ),
              column(
                width = 6,
                p("Y:"),
                selectizeInput(
                  "yLayerPseudoSort",
                  choices = NULL,
                  selected = "",
                  label = "Modality"
                ),
                selectizeInput(
                  "yMarkerPseudoSort",
                  choices = NULL,
                  selected = "",
                  label = "Feature"
                )
              )
            ),
            plotOutput(
              "reduGatePseudoSort",
              height = "660px"
            )
          )
        )

      )
    )
  )
}
