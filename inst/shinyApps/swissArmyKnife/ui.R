library(magrittr)
library(rapbase)
library(rpivotTable)
library(shinyalert)
library(shinyjs)

addResourcePath("rap", system.file("www", package = "rapbase"))
appTitle <- "Swiss army knife"


ui <- tagList(
  shinyjs::useShinyjs(),
  navbarPage(
    title = div(a(includeHTML(system.file("www/logo.svg",
                                          package = "rapbase"))),
                appTitle),
    windowTitle = appTitle,
    theme = "rap/bootstrap.css",

    tabPanel("Start",
             useShinyalert(),
             mainPanel(
               # info text
               system.file("info.Rmd", package = "raptools") %>%
                 knitr::knit() %>%
                 markdown::markdownToHTML(., options = c("fragment_only",
                                                         "base64_images",
                                                         "highlight_code"),
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
               tags$head(tags$link(rel = "shortcut icon",
                                   href = "rap/favicon.ico"))
             )
    ),
    tabPanel("Installasjon",
      # Sidebar layout with a input and output definitions ----
      sidebarLayout(
      # Sidebar panel for inputs ----
        sidebarPanel(
          shiny::uiOutput("repoSelector"),
          uiOutput("branchSelector"),
          shiny::checkboxInput(
            inputId = "upgradeDeps",
            label = "Oppdater avhengige pakker",
            value = TRUE),
          uiOutput("installButton")
        ),
        mainPanel(
          uiOutput("doc"),
          uiOutput("checklist"),
          p(em("System message:")),
          verbatimTextOutput("sysMessage"),
          p(em("Function message:")),
          verbatimTextOutput("funMessage")
        )
      )
    ),
    shiny::tabPanel("Bruksstatistikk",
      shiny::mainPanel(
        fluidRow(
          shiny::uiOutput("logSelector")
        ),
        fluidRow(shiny::column(12,
            rpivotTable::rpivotTableOutput("logPivottTable"))
        )
      )
    ),
    #--------Autoreport----------
    shiny::tabPanel(
      "Autoutsending",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          uiOutput("autoReportSidebar")
          ),
          shiny::mainPanel(
            shiny::tabsetPanel(
              id = "autoreport",
            shiny::tabPanel(
              "Oversikt", value = "oversikt",
              plotOutput("calendar")
            ),
            shiny::tabPanel(
              "Lag ny", value = "lagNy",
              tags$h4("Yaml snippet for ny rapport"),
              verbatimTextOutput("yamlNewReg")
            ),
            shiny::tabPanel(
              "Slett", value = "slett",
              tags$h4("Yaml for rapport som skal slettes:"),
              verbatimTextOutput("delSummary")
            )
          )
        )
      )
    ),
    #---------Config------
    shiny::tabPanel("Informasjon",
      shiny::tabsetPanel(
        shiny::tabPanel("Shiny Server app log",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::uiOutput("shinyServerAppLogControls")
            ),
            shiny::mainPanel(
              shiny::uiOutput("shinyServerLog")
            )
          )
        ),
        shiny::tabPanel("rapbaseConfig",
          shiny::verbatimTextOutput("rapbaseConfig")
        )
      )
    )
  ) # navbarPage
) # tagList
