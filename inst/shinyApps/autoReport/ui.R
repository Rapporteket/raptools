#library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    title="Rutinemessig rapportering",
    theme = "bootstrap.css",

    # Application title

    tabPanel(
      "Oversikt",
      sidebarLayout(
        sidebarPanel(
          uiOutput("regControls"),
          uiOutput("repControls"),
          tags$hr(),
          actionButton("saveAll", "Lagre alle data")
        ),
        mainPanel(
          plotOutput("calendar")
        )
      )
    ),
    tabPanel(
      "Lag ny",
      sidebarLayout(
        sidebarPanel(
          uiOutput("pkgControls"),
          #selectInput("newReg", "Register:",
          #            c("raptools", "noric", "intensiv")),
          textInput("syn", "Tekstlig beskrivelse (emnefelt epost):",
                    "Rutinemessig rapport ang..."),
          uiOutput("regFunControls"),
          uiOutput("regFunParamsControls"),
          uiOutput("regFunParamsValueControls"),
          actionButton("setParam", "Sett parameterverdi"),
          tags$hr(),
          textInput("email", "Epostmottaker:"),
          uiOutput("handleEmailControls"),
          tags$hr(),
          selectInput("interval", "Intervall:",
                      list(dag="DSTday", uke="week", mnd="month",
                           kvartal="quarter", aar="year"),
                      selected = "month"),
          dateInput("from", "Start dato:"),
          actionButton("setDays", "Sett kjøredager"),
          tags$hr(),
          textInput("repId", "Rapport id:", "UnikIdentifikatorAvRapporten"),
          uiOutput("addReportControls")
        ),
        mainPanel(
          tags$h4("Yaml snippet for ny rapport"),
          verbatimTextOutput("yamlNewReg")

        )
      )
    ),
    tabPanel(
      "Slett",
      sidebarLayout(
        sidebarPanel(
          uiOutput("delRegControls"),
          uiOutput("delRepControls"),
          actionButton("doDelRep", "SLETT!")
        ),
        mainPanel(
          tags$h4("Yaml for rapport som skal slettes:"),
          verbatimTextOutput("delSummary")
        )
      )
    )
  )
)
