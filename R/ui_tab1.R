#'
#' @import shiny
#' @import bslib
#' @import bsicons


getUItabl1 <- function(){
  return(
    nav_panel(
      title = "Overview",
      value = "overview",
      icon = icon("dice-d20"),

      layout_columns(
        col_widths = breakpoints(
          sm = c(12, 12),
          md = c(12, 12),
          lg = c(6, 6)
        ),

        card(
          card_header(
            "Groups (category)",
            popover(
              title = "Visualize by",
              bs_icon("gear"),
              selectInput(
                "groupByOverviewGrp",
                choices = NULL,
                label = "Group Name"
              ),

              selectInput(
                inputId = "visByOverviewGrp",
                choices = NULL,
                label = "Highlight"
              ),
              hr(),
              selectInput(
                inputId = "layoutOverviewGrp",
                choices = NULL,
                label = "Layout"
              ),
            )
          ),
          card_body(
            plotOutput("overviewGrp")
          ),
          full_screen = TRUE,
          height = "560px"
        ),

        card(
          card_header(
            "Characteristic (continous)",
            popover(
              title = "Choose Characteristic",
              bs_icon("gear"),
              selectizeInput(
                "scoreOverviewChar",
                choices = NULL,
                label = ""
              ),

              hr(),
              selectInput(
                inputId = "layoutOverviewChar",
                choices = NULL,
                label = "Layout"
              ),
            )
          ),
          card_body(
            plotOutput("overviewChar")
          ),
          full_screen = TRUE
        )
      ),

      card(
        card_header(
          "Characteristics vs Groups"
        ),
        card_body(
          fluidRow(
            column(
              width = 5,
              selectInput(
                "groupCharGrpOverview",
                choices = NULL,
                label = "X: Group Name"
              )
            ),
            column(
              width = 5,
              selectizeInput(
                "scoreCharGrpOverview",
                choices = NULL,
                label = "Y: Characteristics"
              )
            ),
            column(
              width = 2,
              radioButtons(
                inputId = "pltTypeCharGrpOverview",
                label = "Plot type:",
                choices =
                  c(
                    "Box plot" = "box",
                    "Violin plot" = "violin"
                  ),
                selected = "box"
              )
            )
          ),
          hr(),
          plotOutput(
            "charGrpOverview",
            height = "560px"
          )
        )
      )
    )

  )
}
