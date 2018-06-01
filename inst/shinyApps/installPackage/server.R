# Define server logic for installing packages

library(httr)

server <- function(input, output, session) {

  installPackage <- eventReactive(input$install, {
    rapbase::installGithubPackage(input$package, input$branch)
  })

  # Generate http-content
  output$out <- renderPrint({
    cat(installPackage())
  })
}
