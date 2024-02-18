#'
#' @import shiny
#' @import bslib
#' @import bsicons


getUItabl3 <- function(){
  return(
    nav_panel(
      title = "CrossModal",
      value = "crossmodal",
      fluidRow(
        column(
          width = 6,
          h6("Choose feature X:"),
          fluidRow(
            column(
              width = 4,
              selectizeInput(
                "xLayerCrossModal",
                choices = NULL,
                selected = "",
                label = "Select modality"
              )
            ),
            column(
              width = 8,
              selectizeInput(
                "xMarkerCrossModal",
                choices = NULL,
                selected = "",
                label = "Select, or type in one feature name (e.g. TP53)"
              )
            )
          )

        ),
        column(
          width = 6,
          h6("Choose feature Y:"),
          fluidRow(
            column(
              width = 4,
              selectizeInput(
                "yLayerCrossModal",
                choices = NULL,
                selected = "",
                label = "Select modality"
              )
            ),
            column(
              width = 8,
              selectizeInput(
                "yMarkerCrossModal",
                choices = NULL,
                selected = "",
                label = "Select, or type in one feature name (e.g. TP53)"
              )
            )
          )
        )
      ),

      card(
        card_header(
          "Cross modality feature correlation",
          popover(
            title = "Settings",
            bs_icon("gear"),
            checkboxInput(
              "corLmCrossModal",
              label = "Add trend line?",
              value = FALSE,
              width = NULL),

            checkboxInput(
              "revZeCrossModal",
              label = "Remove zeros?",
              value = FALSE,
              width = NULL)
          )
        ),
        card_body(
          fluidRow(
            column(
              width = 4,
              plotOutput("overallCrossModal")
            ),

            column(
              width = 8,
              selectInput(
                "groupByCrossModal",
                choices = "",
                label = "by Group (e.g. clusters, batches, cell-types)"
              ),
              plotOutput("wrapCrossModal", height = "660px")
            )
          )
        )
      )
    )

  )
}
