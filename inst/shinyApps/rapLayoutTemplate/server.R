library(shiny)
library(magrittr)
library(raptools)

server <- function(input, output, session) {

  # html rendering function for re-use
  htmlRenderRmd <- function(srcFile) {
    # set param needed for report meta processing
    params <- list(tableFormat="html")
    system.file(srcFile, package="raptools") %>%
      knitr::knit() %>%
      markdown::markdownToHTML(.,
                               options = c('fragment_only',
                                           'base64_images',
                                           'highlight_code')) %>%
      shiny::HTML()
  }

  makeHist <- function(bins, makeTable = FALSE) {
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = bins +1)
    t <- hist(x, breaks = bins, col = 'darkgray', border = 'white')
    if (makeTable) {
      data.frame(GruppeMin=t$breaks[1:length(t$mids)],
                 GruppeMax=t$breaks[2:(length(t$mids)+1)], N=t$counts)
    } else {
      t
    }
  }

  output$formFarge <- renderUI({
    htmlRenderRmd("formOgFarge.Rmd")
  })

  output$sampleUcControl <- renderUI({
    selectInput(inputId = "sampleUc", label = "Sample user ctrl",
                choices = c("How", "it", "will", "look"))
  })

  output$distPlot <- renderPlot({
    makeHist(bins = input$bins)
  })

  output$distTable <- renderTable({
    makeHist(bins = input$bins, makeTable = TRUE)
  })

  output$vurdering2niva <- renderUI({
    htmlRenderRmd("vurdering2niva.Rmd")
  })

  output$distPlot2 <- renderPlot({
    makeHist(bins = input$bins)
  })

  output$distTable2 <- renderTable({
    makeHist(bins = input$bins2, makeTable = TRUE)
  })

  output$vurdering3niva <- renderUI({
    htmlRenderRmd("vurdering3niva.Rmd")
  })
}
