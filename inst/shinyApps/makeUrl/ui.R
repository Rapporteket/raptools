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
                choices = c("/makeUrl", "/intensiv", "/nakke")),
      selectInput(inputId = "user",
                  label = "user:",
                  choices = c("testUser1", "testUser2")),
      checkboxGroupInput(inputId = "groups",
                         label = "groups:",
                         choices = c("intensiv", "nakke"),
                         selected = NULL),
      selectInput(inputId = "resh_id",
                  label = "resh_id:",
                  choices = c("123456", "789012")),
      selectInput(inputId = "role",
                  label = "role:",
                  choices = c("LU", "LC")),
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
      h2("Test of simulation if base url is localhost"),
      verbatimTextOutput("httpRequest")

    )
  )
)
