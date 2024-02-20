#' @title Run the ShinyCITExpresso
#' @description Launches a Shiny application using the provided
#' MultiAssayExperiment object.
#' @param mae A MultiAssayExperiment object with experimentlist of
#' SingleCellExperiment objects.
#' @param appTitle A character vector to show on as the title of the app.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import waiter

#' @export
#' @examples
#' \dontrun{
#' run_app(mae = mae)
#' }
#'
run_app <- function(mae, appTitle = "Demo Data") {
  msgMAE <-
    getMAEmsg(mae)

  ShinyCITExpresso_ui <-
    fixedPage(
      ## UI settings ####
      waiter_set_theme(html = spin_3(), color = "#FFFFFF"),
      autoWaiter(),
      theme = bs_theme(
        version = 5,
        bootswatch = "yeti",
        bg = "#FFFFFF",
        fg = "#000000",
        primary = "#0199F8",
        secondary = "#333333",
        base_font = font_google("Bitter"),
        code_font = font_google("Lora")
      ),

      tags$head(
        tags$style(
          ".card {
            box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
            transition: 0.3s;
          }

          .card:hover {
            box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
          }"
        )
      ),

      page_navbar(
        title = "ShinyCITExpresso",
        header = div(h1(appTitle), p(msgMAE)),

        ### tab 1 ####
        getUItabl1(),

        ### tab 2 ####
        getUItabl2(),

        ### tab 3 ####
        getUItabl3(),

        ### tab 4 ####
        getUItabl4(),


      )


    )

  # Server Logic ####
  ShinyCITExpresso_server <- function(input, output, session) {

    # server settings ####
    listColData <-
      getListColData(mae)

    listNameColData <-
      getNameColData(listColData)

    grpCol <-
      listNameColData[["Group"]]

    charCol <-
      listNameColData[["Charac"]]

    listReducedDim <-
      getListReducedDim(mae)

    avaReducedName <-
      names(listReducedDim)

    prfReducedName <-
      getPrfReducedName(avaReducedName)

    listAssayMarkers <-
      getListAssayMarkers(mae)

    lnAssay <-
      names(listAssayMarkers)

    colorPalete <-
      getColorScheme()

    # tab 1: Groups ####
    updateSelectizeInput(
      session,
      "groupByOverviewGrp",
      selected =
        grpCol[1],
      choices =
        grpCol
    )

    updateSelectizeInput(
      session,
      "layoutOverviewGrp",
      selected =
        prfReducedName,
      choices =
        avaReducedName
    )

    observe({
      req(input$groupByOverviewGrp)

      gnp <-
        input$groupByOverviewGrp

      fnGrp <-
        as.character(unique(listColData[[gnp]]))

      updateSelectizeInput(
        session,
        "visByOverviewGrp",
        selected =
          "all",
        choices =
          c("all",
            fnGrp)
      )
    })

    output$overviewGrp <-
      renderPlot({
        req(
          input$groupByOverviewGrp,
          input$visByOverviewGrp,
          input$layoutOverviewGrp
        )

        gnp <-
          input$groupByOverviewGrp
        visp <-
          input$visByOverviewGrp

        tblRaw <-
          getTblReducedDim(listReducedDim, input$layoutOverviewGrp)

        tblRaw[[gnp]] <-
          listColData[[gnp]]

        pltOverviewGrp(tblRaw, gnp, visp)

      })

    # tab 1: Characteristics ####
    updateSelectizeInput(
      session,
      "scoreOverviewChar",
      selected =
        charCol[1],
      choices = charCol
    )

    updateSelectizeInput(
      session,
      "layoutOverviewChar",
      selected =
        prfReducedName,
      choices =
        avaReducedName
    )

    output$overviewChar <- renderPlot({
      req(input$scoreOverviewChar, input$layoutOverviewChar)

      scoreName <-
        input$scoreOverviewChar

      tblRaw <-
        getTblReducedDim(listReducedDim, input$layoutOverviewChar)

      tblRaw[[scoreName]] <-
        listColData[[scoreName]]

      pltOverviewChar(tblRaw, scoreName)

    })

    # tab 2: choose feature ####
    updateSelectizeInput(
      session,
      "layerFeatureViz",
      selected =
        lnAssay[1],
      choices =
        lnAssay
    )

    observeEvent(input$layerFeatureViz,{
      if(!is.null(input$mkFeatureViz)){
        updateSelectizeInput(
          session,
          "mkFeatureViz",
          choices =
            NULL
        )
      }

      updateSelectizeInput(
        session,
        "mkFeatureViz",
        choices =
          listAssayMarkers[[input$layerFeatureViz]],
        selected =
          listAssayMarkers[[input$layerFeatureViz]][1]
      )
    })

    # tab 2: by Reduced Dimensions ####
    updateSelectizeInput(
      session,
      "colorReduFeatureViz",
      selected =
        names(colorPalete)[1],
      choices =
        names(colorPalete)
    )

    updateSelectizeInput(
      session,
      "layoutReduFeatureViz",
      selected =
        prfReducedName,
      choices =
        avaReducedName
    )

    getReduFeatureViz <-
      eventReactive(input$plotReduFeatureViz, {
        tblRaw <-
          getTblReducedDim(listReducedDim, input$layoutReduFeatureViz)

        tblRaw[["gene"]] <-
          getFeatureExpression(
            mae = mae,
            layer = input$layerFeatureViz,
            feature = input$mkFeatureViz
          )

        pltReduFeatureViz(
          tblRaw = tblRaw,
          geneName =
            paste(input$layerFeatureViz,
                  input$mkFeatureViz,
                  sep = ": "),
          colFn = input$colorReduFeatureViz
        )
      })

    output$reduFeatureViz <- renderPlot({
      getReduFeatureViz()
    })

    # tab 2: by Group ####
    updateSelectizeInput(
      session,
      "groupByGrpFeatureViz",
      selected =
        grpCol[1],
      choices =
        grpCol
    )
    observeEvent(input$groupByGrpFeatureViz,{
      if(!is.null(input$gnpNamesGrpFeatureViz)){
        updateSelectizeInput(
          session,
          "gnpNamesGrpFeatureViz",
          choices =
            NULL
        )
      }
      updateSelectizeInput(
        session,
        "gnpNamesGrpFeatureViz",
        choices =
          unique(listColData[[input$groupByGrpFeatureViz]]),
        selected =
          unique(listColData[[input$groupByGrpFeatureViz]])
      )
    })

    getGrpFeatureViz <-
      eventReactive(input$plotGrpFeatureViz, {
        tblRaw <-
          data.frame(
            `group` =
              listColData[[input$groupByGrpFeatureViz]],
            `exprs` =
              getFeatureExpression(
                mae = mae,
                layer = input$layerFeatureViz,
                feature = input$mkFeatureViz
              )
          )

        qBase <-
          pltGrpFeatureViz(
            tblRaw = tblRaw,
            geneName =
              paste(input$layerFeatureViz,
                    input$mkFeatureViz,
                    sep = "\n"),
            grpName = input$gnpNamesGrpFeatureViz,
            fnCol = "Spectral",
            fnTitle = input$groupByGrpFeatureViz
          )
        if(input$pltTypeGrpFeatureViz == "box"){
          plot(qBase + geom_boxplot(outlier.shape = NA))
        }
        if(input$pltTypeGrpFeatureViz == "violin"){
          plot(qBase + geom_violin())
        }
      })

    output$grpFeatureViz <- renderPlot({
      getGrpFeatureViz()
    })

    # tab 3: choose X ####
    updateSelectizeInput(
      session,
      "xLayerCrossModal",
      selected =
        lnAssay[1],
      choices =
        lnAssay
    )
    observeEvent(input$xLayerCrossModal,{
      if(!is.null(input$xMarkerCrossModal)){
        updateSelectizeInput(
          session,
          "xMarkerCrossModal",
          choices =
            NULL
        )
      }
      updateSelectizeInput(
        session,
        "xMarkerCrossModal",
        choices =
          listAssayMarkers[[input$xLayerCrossModal]],
        selected =
          listAssayMarkers[[input$xLayerCrossModal]][1]
      )
    })
    # tab 3: choose Y ####
    updateSelectizeInput(
      session,
      "yLayerCrossModal",
      selected =
        lnAssay[1],
      choices =
        lnAssay
    )
    observeEvent(input$yLayerCrossModal,{
      if(!is.null(input$yMarkerCrossModal)){
        updateSelectizeInput(
          session,
          "yMarkerCrossModal",
          choices =
            NULL
        )
      }
      updateSelectizeInput(
        session,
        "yMarkerCrossModal",
        choices =
          listAssayMarkers[[input$yLayerCrossModal]],
        selected =
          listAssayMarkers[[input$yLayerCrossModal]][2]
      )
    })

    # tab 3: Cross Modal ####
    updateSelectizeInput(
      session,
      "groupByCrossModal",
      selected =
        grpCol[1],
      choices =
        grpCol
    )
    getScatterDat <-
      reactive({

        tblRaw <-
          data.frame(
            `x_var` =
              getFeatureExpression(
                mae = mae,
                layer = input$xLayerCrossModal,
                feature = input$xMarkerCrossModal
              ),
            `y_var` =
              getFeatureExpression(
                mae = mae,
                layer = input$yLayerCrossModal,
                feature = input$yMarkerCrossModal
              ),
            `group` =
              listColData[[input$groupByCrossModal]]
          )

        if(input$revZeCrossModal){
          rvZeroDat(tblRaw)
        }else{
          tblRaw
        }
      })

    getOverallCrossModalplt <-
      eventReactive(input$plotCrossModal, {
        pltOverallCrossModal(
          tblDat =
            getScatterDat(),
          xLab =
            paste(
              input$xLayerCrossModal,
              input$xMarkerCrossModal,
              sep = "\n"
            ),
          yLab =
            paste(
              input$yLayerCrossModal,
              input$yMarkerCrossModal,
              sep = "\n"
            ),
          fnCol = "Spectral",
          corLm = input$corLmCrossModal
        )
      })

    output$overallCrossModal <- renderPlot({
      getOverallCrossModalplt()
    })


    getWrapCrossModalplt <-
      eventReactive(input$plotCrossModal, {
        pltWrapCrossModal(
          tblDat =
            getScatterDat(),
          xLab =
            paste(
              input$xLayerCrossModal,
              input$xMarkerCrossModal,
              sep = "\n"
            ),
          yLab =
            paste(
              input$yLayerCrossModal,
              input$yMarkerCrossModal,
              sep = "\n"
            ),
          fnCol = "Spectral",
          corLm = input$corLmCrossModal
        )
      })


    output$wrapCrossModal <- renderPlot({
      getWrapCrossModalplt()
    })



    # tab 4: Reduced Dimensions ####

    updateSelectizeInput(
      session,
      "fnColPseudoSort",
      selected =
        c(grpCol, charCol)[1],
      choices =
        c(grpCol, charCol)
    )

    updateSelectizeInput(
      session,
      "layoutPseudoSort",
      selected =
        prfReducedName,
      choices =
        avaReducedName
    )

    output$gateReduPseudoSort <- renderPlot({
      req(
        input$fnColPseudoSort,
        input$layoutPseudoSort
      )

      gnp <-
        input$fnColPseudoSort

      tblRaw <-
        getTblReducedDim(listReducedDim, input$layoutPseudoSort)

      tblRaw[[gnp]] <-
        listColData[[gnp]]

      if(gnp %in% grpCol){
        pltOverviewGrp(tblRaw, gnp, visp = "all")
      }else if(gnp %in% charCol){
        pltOverviewChar(tblRaw, gnp)
      }

    })

    # tab 4: gate X ####
    updateSelectizeInput(
      session,
      "xLayerPseudoSort",
      selected =
        lnAssay[1],
      choices =
        lnAssay
    )
    observeEvent(input$xLayerPseudoSort,{
      if(!is.null(input$xMarkerPseudoSort)){
        updateSelectizeInput(
          session,
          "xMarkerPseudoSort",
          choices =
            NULL
        )
      }
      updateSelectizeInput(
        session,
        "xMarkerPseudoSort",
        choices =
          listAssayMarkers[[input$xLayerPseudoSort]],
        selected =
          listAssayMarkers[[input$xLayerPseudoSort]][1]
      )
    })

    # tab 4: gate Y ####
    updateSelectizeInput(
      session,
      "yLayerPseudoSort",
      selected =
        lnAssay[1],
      choices =
        lnAssay
    )
    observeEvent(input$yLayerPseudoSort,{
      if(!is.null(input$yMarkerPseudoSort)){
        updateSelectizeInput(
          session,
          "yMarkerPseudoSort",
          choices =
            NULL
        )
      }
      updateSelectizeInput(
        session,
        "yMarkerPseudoSort",
        choices =
          listAssayMarkers[[input$yLayerPseudoSort]],
        selected =
          listAssayMarkers[[input$yLayerPseudoSort]][2]
      )
    })

    # tab 4: Gating Features ####
    getBrushScatterDat <-
      reactive({
        req(
          input$xLayerPseudoSort,
          input$xMarkerPseudoSort,
          input$yLayerPseudoSort,
          input$yMarkerPseudoSort
        )

        tblRaw <-
          data.frame(
            `x_var` =
              getFeatureExpression(
                mae = mae,
                layer = input$xLayerPseudoSort,
                feature = input$xMarkerPseudoSort
              ),
            `y_var` =
              getFeatureExpression(
                mae = mae,
                layer = input$yLayerPseudoSort,
                feature = input$yMarkerPseudoSort
              )
          )
        tblRaw
      })

    getBrushDat <-
      reactive({
        datReducedims <-
          getTblReducedDim(listReducedDim, input$layoutPseudoSort)

        dfBrush <-
          brushedPoints(
            df = datReducedims,
            brush = input$gateReduPseudoSortBrush,
            allRows = TRUE,
            xvar = colnames(datReducedims)[1],
            yvar = colnames(datReducedims)[2])
        dfBrush[dfBrush[["selected_"]] == TRUE, ]
      })

    getMarkerBrushDat <-
      reactive({
        datReducedims <-
          getTblReducedDim(listReducedDim, input$layoutPseudoSort)
        tblDat <-
          getBrushScatterDat()
        tblBrush <-
          getBrushDat()
        markerDat <-
          cbind(datReducedims, tblDat)
        markerBrush <-
          markerDat[rownames(markerDat) %in% rownames(tblBrush), ]
      })


    getReduGatePseudoSort <-
      eventReactive(input$gateReduPseudoSortBrush, {
        pltReduGatePseudoSort(
          tblDat =
            getBrushScatterDat(),
          tblBrush =
            getBrushDat(),
          markerBrush =
            getMarkerBrushDat(),
          xLab =
            paste(
              input$xLayerPseudoSort,
              input$xMarkerPseudoSort,
              sep = "\n"
            ),
          yLab =
            paste(
              input$yLayerPseudoSort,
              input$yMarkerPseudoSort,
              sep = "\n"
            )
        )
      })


    output$reduGatePseudoSort <- renderPlot({
      getReduGatePseudoSort()
    })


  }


  shinyApp(ui = ShinyCITExpresso_ui,
           server = ShinyCITExpresso_server)
}
