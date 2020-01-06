# Define server logic for installing packages

library(httr)
library(shinyjs)

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

  # test environ vars
  output$confPath <- renderPrint(paste("Config path:", Sys.getenv("R_RAP_CONFIG_PATH")))

  # Various calls for session data from rapbase and systemn settings
  output$callUser <- renderText({
    paste("rapbase::getUserName(session):",
          rapbase::getUserName(session))
  })
  output$callGroups <- renderText({
    paste("rapbase::getUserGroups(session):",
          rapbase::getUserGroups(session))
  })
  output$callReshId <- renderText({
    paste("rapbase::getUserReshId(session):",
          rapbase::getUserReshId(session))
  })
  output$callRole <- renderText({
    paste("rapbase::getUserRole(session):",
          rapbase::getUserRole(session))
  })

  output$callEmail <- renderText({
    paste("rapbase::getUserEmail(session):",
          rapbase::getUserEmail(session))
  })

  output$callFullName <- renderText({
    paste("rapbase::getUserFullName(session):",
          rapbase::getUserFullName(session))
  })

  output$callPhone <- renderText({
    paste("rapbase::getUserPhone(session):",
          rapbase::getUserPhone(session))
  })

  output$envInstance <- renderText({
    Sys.getenv("R_RAP_INSTANCE")
  })

  output$envConfigPath <- renderText({
    Sys.getenv("R_RAP_CONFIG_PATH")
  })

  output$locale <- renderText({
    Sys.getlocale()
  })

  # widget
  output$appUserName <- renderText(getUserFullName(session))
  output$appOrgName <- renderText(paste(getUserReshId(session),
                                        getUserRole(session),
                                        sep = ", "))

  #------------Logwatcher-----
  output$logPivottTable <- rpivotTable::renderRpivotTable(
    rpivotTable::rpivotTable(raptools::getLogData(input$selectLog))
  )
}
