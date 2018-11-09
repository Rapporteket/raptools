#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(title = "RAPPORTEKET UI TEMPLATE", theme = "bootstrap.css",
  tabPanel("FigType 1",
    tabsetPanel(
      tabPanel("Report 1a",
        sidebarLayout(
          sidebarPanel(),
          mainPanel(
            textOutput("testSessionObj")
          )
        )
      ),
      tabPanel("Report 1b",
        sidebarLayout(
          sidebarPanel(uiOutput("sampleUcControl")),
          mainPanel(plotOutput("distPlot"))
        )
      )
    )
  ),
  tabPanel("FigType 2",
    tabsetPanel(
      tabPanel("Report 2a",
        sidebarLayout(
          sidebarPanel(),
          mainPanel()
        )
      ),
      tabPanel("Report 2b",
        sidebarLayout(
          sidebarPanel(),
          mainPanel()
        )
      ),
      tabPanel("Report 2c",
        sidebarLayout(
          sidebarPanel(),
          mainPanel()
        )
      )
    )
  ),
  tabPanel("FigType 3",
    tabsetPanel(
      tabPanel("Report 3a",
        sidebarLayout(
          sidebarPanel(),
          mainPanel()
        )
      ),
      tabPanel("Report 3b",
        sidebarLayout(
          sidebarPanel(),
          mainPanel()
        )
      ),
      tabPanel("Report 3c",
        sidebarLayout(
          sidebarPanel(),
          mainPanel()
        )
      ),
      tabPanel("Report 3d",
        sidebarLayout(
          sidebarPanel(),
          mainPanel()
        )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$testSessionObj <- renderText({
    paste("username:", rapbase::getShinyUserName(session, testCase = TRUE),
          "groups:", rapbase::getShinyUserGroups(session, testCase = TRUE),
          "role:", rapbase::getShinyUserRole(session, testCase = TRUE),
          "reshId:", rapbase::getShinyUserReshId(session, testCase = TRUE))
  })

  output$sampleUcControl <- renderUI({
    selectInput(inputId = "sampleUc", label = "Sample user ctrl",
                choices = c("How", "it", "will", "look"))
  })

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = 10)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application
shinyApp(ui = ui, server = server)

