# Define UI for user simulation app ----
ui <- fluidPage(

  # App title ----
  titlePanel("User data simulation"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      textInput(inputId = "baseUrl",
                label = "base url:",
                value = "http://127.0.0.1"),
      selectInput(inputId = "app",
                label = "app:",
                choices = c("", "/makeUrl", "/intensiv", "/nakke", "/noric",
                            "/muskel", "/norgast")),
      selectInput(inputId = "user",
                  label = "user:",
                  choices = c("testUser1", "testUser2")),
      checkboxGroupInput(inputId = "groups",
                         label = "groups:",
                         choices = c("intensiv", "nakke", "muskel", "norgast"),
                         selected = NULL),
      selectInput(inputId = "resh_id",
                  label = "resh_id:",
                  choices = c("muskel, UNN" = "101719", "norgast, UNN" = "601225",
                              "123456" = "123456", "789012" = "789012",
                              "101619" = "101619", "102966" = "102966",
                              "Intensiv, Ullevål kir int" = "109773",
                              "Intensiv, Haukeland" = "112044")),
      selectInput(inputId = "role",
                  label = "role:",
                  choices = c("LU", "SC")),
      tags$h4("Settings in Shiny Server Config"),
      textInput(inputId = "proxyUser",
                label = "proxyUser:",
                value = "X-USER"),
      textInput(inputId = "proxyGroups",
                label = "proxyGroups:",
                value = "X-GROUPS"
                 ),
      textInput(inputId = "whitelistResh",
                label = "whitelistResh:",
                value = "resh_id"),
      textInput(inputId = "whitelistRole",
                label = "whitelistRole:",
                value = "role")
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      tags$h2("Real scenario"),
      tags$br(),
      # Output tabular overview
      tableOutput("tab"),

      tags$h2("Link app with simulated user data"),
      uiOutput("url"),

      # output http request
      # h2("Test of simulation if base url is myself (this app)"),
      # verbatimTextOutput("httpRequest"),

      # return from rapbase functions
      h4("Use above url when linked to myself (this app) to test 'rapbase' functions:"),
      textOutput("callUser"),
      textOutput("callGroups"),
      textOutput("callReshId"),
      textOutput("callRole")

    )
  )
)
