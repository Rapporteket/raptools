# Define server logic for installing packages

library(httr)

server <- function(input, output, session) {

  installPackage <- observeEvent(input$install, {
    withCallingHandlers({
      shinyjs::html("text", "")
      rapbase::installGithubPackage(input$package, input$branch)
    },
    message = function(m) {
      shinyjs::html(id = "text", html = m$message, add = TRUE)
    })
  })

}
