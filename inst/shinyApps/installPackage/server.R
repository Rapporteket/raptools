#library(dplyr)
#library(httr)
library(lubridate)
library(magrittr)
library(rapbase)
library(raptools)
library(rpivotTable)
library(shinyalert)
library(shinyjs)

server <- function(input, output, session) {

  # Params
  instance <- Sys.getenv("R_RAP_INSTANCE")
  configPath <- Sys.getenv("R_RAP_CONFIG_PATH")


  # widget
  output$appUserName <- renderText(getUserFullName(session))
  output$appOrgName <- renderText(paste(getUserReshId(session),
                                        getUserRole(session),
                                        sep = ", "))
  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })


  # Info
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
    instance
  })

  output$envConfigPath <- renderText({
    configPath
  })

  output$locale <- renderText({
    Sys.getlocale()
  })


  # Install packages
  if (instance == "PRODUCTION") {
    checklist <- prodChecklist
  }
  if (instance == "QA") {
    checklist <- qaChecklist
  }

  output$branchSelector <- renderUI(
    switch (instance,
      DEV = textInput(inputId = "branch", label = "Grein:"),
      TEST = textInput(inputId = "branch", label = "Grein:"),
      QA = selectInput(inputId = "branch", label = "Grein:",
                       choices = c("master", "rel")),
      PRODUCTION = selectInput(inputId = "branch", label = "Grein:",
                               choices = c("master"))
    )
  )

  output$checklist <- renderUI(
    if (exists('checklist')) {
      checkboxGroupInput(inputId = "manControl",
                         label = "Sjekk at du faktisk har:",
                         choices = checklist)
    } else {
      NULL
    }
  )

  output$installButton <- renderUI(
    if (exists('checklist')) {
      if (length(input$manControl) == length(checklist)) {
        actionButton(inputId = "install", label = "Install")
      } else {
        NULL
      }
    } else {
      actionButton(inputId = "install", label = "Install")
    }

  )

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


  #------------Logwatcher-----
  output$logSelector <- renderUI(
    shiny::selectInput(
      inputId = "selectLog",
      label = "Log:",
      choices = list(
        "Application level" = "app",
        "Report level"="report")
    )
  )

  logData <- reactive(
    raptools::getLogData(req(input$selectLog)) %>%
      dplyr::mutate(
        time = as.POSIXct(time),
        year = lubridate::year(time),
        month = lubridate::month(time),
        day = lubridate::day(time),
        weekday = lubridate::wday(
          time,
          week_start = 1,
          abbr =FALSE))
  )
  output$logPivottTable <- rpivotTable::renderRpivotTable(
    rpivotTable::rpivotTable(logData())
  )
}
