# Define server logic for installing packages

library(httr)

server <- function(input, output, session) {

  installPackage <- observeEvent(input$install, {
    withCallingHandlers({
      shinyjs::html("sysMessage", "")
      shinyjs::html("funMessage", "")
      shinyjs::html("funMessage", rapbase::installGithubPackage(input$package, input$branch))
    },
    message = function(m) {
      shinyjs::html(id = "sysMessage", html = m$message, add = TRUE)
    })
  })

}
