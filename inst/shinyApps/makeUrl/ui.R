# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Parameters"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      textInput(inputId = "baseUrl",
                label = "Base url:",
                value = "http://127.0.0.1:4481/"),
      selectInput(inputId = "app",
                label = "app:",
                choices = c("intensiv", "nakke")),
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

      # Output tabular overview
      tableOutput("tab"),

      # Output: verbatime text for url
      tags$h2("Link"),
      tags$br(),
      uiOutput("url"),

      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),

      # Output: HTML table with requested number of observations ----
      tableOutput("view")
    )
  )
)
