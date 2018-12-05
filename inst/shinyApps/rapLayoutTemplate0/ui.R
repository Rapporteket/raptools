
library(shiny)
library(rapbase)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "rapLayoutTemplate"

# Define UI for application that draws a histogram
ui <- tagList(
  shinythemes::themeSelector(),
  navbarPage(
    title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"),
                regTitle),
    windowTitle = regTitle,
    tabPanel("Form og farge",
      sidebarLayout(
        sidebarPanel(
          textInput(inputId="test", label="Eksempel", value = "Hvasomhelst")
        ),
        mainPanel(
          htmlOutput("formFarge", inline = TRUE)
        )
      )
    ),
    tabPanel("Navigasjon i 2 nivå",
      sidebarLayout(
        sidebarPanel(
          sliderInput(inputId = "bins",
                      label = "Antall grupper:",
                      min = 1,
                      max = 50,
                      value = 30)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Figur", plotOutput("distPlot")),
            tabPanel("Tabell", tableOutput("distTable")),
            tabPanel("Vurdering",
                     htmlOutput("vurdering2niva", inline = TRUE))
          )
        )
      )
    ),
    tabPanel("Navigasjon i 3 nivå",
      tabsetPanel(
        tabPanel("Report 2a",
          sidebarLayout(
            sidebarPanel(
              sliderInput(inputId = "bins2",
                          label = "Antall grupper:",
                          min = 1,
                          max = 50,
                          value = 30)
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Figur", plotOutput("distPlot2")),
                tabPanel("Tabell", tableOutput("distTable2")),
                tabPanel("Vurdering",
                         htmlOutput("vurdering3niva", inline = TRUE))
              )
            )
          )
        ),
        tabPanel("Report 2b",
          sidebarLayout(
            sidebarPanel("Med hensikt ingen visning"),
            mainPanel(
              tabsetPanel(
                tabPanel("Figur", "Med hensikt ingen visning"),
                tabPanel("Tabell", "Med hensikt ingen visning"),
                tabPanel("Vurdering", "Med hensikt ingen visning")
              )
            )
          )
        ),
        tabPanel("Report 2c",
          sidebarLayout(
            sidebarPanel("Med hensikt ingen visning"),
            mainPanel(
              tabsetPanel(
                tabPanel("Figur", "Med hensikt ingen visning"),
                tabPanel("Tabell", "Med hensikt ingen visning"),
                tabPanel("Vurdering", "Med hensikt ingen visning")
              )
            )
          )
        )
      )
    )
  )
)
