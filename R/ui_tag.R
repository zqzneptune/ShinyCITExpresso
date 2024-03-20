getTagHead <- function(){
  return(
    tags$head(
      tags$meta(
        name = "author",
        content = "Qingzhou Zhang"
      ),
      tags$meta(
        name = "keywords",
        content = "shiny, CITE-seq, scRNA-seq, visualization"
      ),
      tags$meta(
        name = "description",
        content = "A ShinyApp for visualization and explorering Cellular Indexing of Transcriptomes and Epitopes by Sequencing (CITE-seq) data"
      ),

      tags$link(
        rel = "icon",
        href = file.path("www", "favicon.ico"),
        type = "image/x-icon"

      ),

      tags$link(
        rel = "apple-touch-icon-precomposed",
        href = file.path("www", "apple-touch-icon.png"),
        sizes = "114x114"

      ),

      tags$link(
        rel = "apple-touch-icon-precomposed",
        href = file.path("www", "android-chrome-192x192.png"),
        sizes = "72x72"

      ),

      tags$link(
        rel = "apple-touch-icon-precomposed",
        href = file.path("www", "favicon-32x32.png")
      ),

      tags$style(
        ".card {
            box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
            transition: 0.3s;
          }

          .card:hover {
            box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
          }"
      )
    )
  )
}
