library(magrittr)
library(rapbase)
library(rpivotTable)
library(shinyjs)

addResourcePath('rap', system.file('www', package='rapbase'))
appTitle = "Swiss army knife"


ui <- tagList(
  shinyjs::useShinyjs(),
  navbarPage(
    title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                appTitle),
    windowTitle = appTitle,
    theme = "rap/bootstrap.css",

    tabPanel("Informasjon",
             mainPanel(
               # info text
               system.file("info.Rmd", package = "raptools") %>%
                 knitr::knit() %>%
                 markdown::markdownToHTML(., options = c('fragment_only',
                                                         'base64_images',
                                                         'highlight_code'),
                                          encoding = "utf-8") %>%
                 shiny::HTML(),
               hr(),
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
               textOutput("locale"),
               appNavbarUserWidget(user = uiOutput("appUserName"),
                                   organization = uiOutput("appOrgName"),
                                   addUserInfo = TRUE),
               tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"))
             )
    ),
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
          uiOutput("branchSelector"),
          uiOutput("installButton")
        ),
        mainPanel(
          uiOutput("checklist"),
          p(em("System message:")),
          verbatimTextOutput("sysMessage"),
          p(em("Function message:")),
          verbatimTextOutput("funMessage")
        )
      )
    ),
    shiny::tabPanel("Log",
      shiny::mainPanel(
        fluidRow(
          shiny::uiOutput("logSelector")
        ),
        fluidRow(shiny::column(12,
          rpivotTable::rpivotTableOutput("logPivottTable"))
        )
      )
    )
  ) # navbarPage
) # tagList

