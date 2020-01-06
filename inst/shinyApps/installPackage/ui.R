library(rapbase)
library(rpivotTable)

addResourcePath('rap', system.file('www', package='rapbase'))
appTitle = "Swiss army knife"


ui <- tagList(
  shinyjs::useShinyjs(),
  navbarPage(
    title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                appTitle),
    windowTitle = appTitle,
    theme = "rap/bootstrap.css",

    tabPanel("Install package from GitHub",
      # Sidebar layout with a input and output definitions ----
      sidebarLayout(
      # Sidebar panel for inputs ----
        sidebarPanel(
          selectInput(inputId = "package",
                      label = "Package:",
                      choices = c("intensiv", "muskel", "Nakke", "nger",
                                  "nordicscir", "norgast", "NORIC", "nordummy",
                                  "rapbase", "rapgen", "raplog",
                                  "hisreg", "norspis", "nra", "smerte",
                                  "rygg")),
          selectInput(inputId = "branch",
                      label = "Branch:",
                      choices = c("shinyfy", "rel", "master", "yt")),
          actionButton(inputId = "install",
                       label = "Install")
        ),
        mainPanel(
        # output
          p(em("System message:")),
          verbatimTextOutput("sysMessage"),
          p(em("Function message:")),
          verbatimTextOutput("funMessage"),
          p(em("Test env var:")),
          verbatimTextOutput("confPath"),
          appNavbarUserWidget(user = uiOutput("appUserName"),
                              organization = uiOutput("appOrgName"),
                              addUserInfo = TRUE),
          tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"))

        )
      )
    ),
    tabPanel("System info",
      mainPanel(
        # return from rapbase functions
        h4("Test 'rapbase' functions using the session object:"),
        textOutput("callUser"),
        textOutput("callGroups"),
        textOutput("callReshId"),
        textOutput("callRole"),
        textOutput("callEmail"),
        textOutput("callFullName"),
        textOutput("callPhone"),
        h4("Environment var R_RAP_INSTANCE:"),
        textOutput("envInstance"),
        h4("Environmental var R_RAP_CONFIG_PATH:"),
        textOutput("envConfigPath"),
        h4("Locale settings:"),
        textOutput("locale")
      )
    ),
    shiny::tabPanel("Log",
      shiny::mainPanel(
        fluidRow(
          shiny::selectInput(
            inputId = "selectLog",
            label = "Log:",
            choices = list(
              "Application level" = "app",
              "Report level"="report")
        )
       ),
       fluidRow(shiny::column(12,
         rpivotTable::rpivotTableOutput("logPivottTable"))
       )
      )
    )
  ) # navbarPage
) # tagList

