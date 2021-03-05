
library(shiny)
library(rapbase)

addResourcePath("rap", system.file("www", package = "rapbase"))
regTitle <- "rapLayoutTemplate"

# Define UI for application that draws a histogram
ui <- tagList(
  shinythemes::themeSelector(),
  navbarPage(
    title = div(img(src = "rap/logo.svg", alt = "Rapporteket",
                    height = "26px"),
                regTitle),
    windowTitle = regTitle,

    tabPanel("Form og farge",
      sidebarLayout(
        sidebarPanel(width = 3,
          textInput(inputId = "test", label = "Eksempel",
                    value = "Hvasomhelst")
        ),
        mainPanel(
          htmlOutput("formFarge", inline = TRUE)
        )
      )
    ),
    tabPanel("Navigasjon i 2 nivå",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput(inputId = "var",
                      label = "Variabel:",
                      c("mpg", "disp", "hp", "drat", "wt", "qsec")),
          sliderInput(inputId = "bins",
                      label = "Antall grupper:",
                      min = 1,
                      max = 10,
                      value = 5)
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
        tabPanel("Fordeling av mpg",
          sidebarLayout(
            sidebarPanel(width = 3,
              sliderInput(inputId = "binsMpg",
                          label = "Antall grupper:",
                          min = 1,
                          max = 10,
                          value = 5)
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Figur", plotOutput("distPlotMpg")),
                tabPanel("Tabell", tableOutput("distTableMpg"))
              )
            )
          )
        ),
        tabPanel("Fordeling av hp",
          sidebarLayout(
            sidebarPanel(width = 3,
              sliderInput(inputId = "binsHp",
                          label = "Antall grupper:",
                          min = 1,
                          max = 10,
                          value = 5)
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Figur", plotOutput("distPlotHp")),
                tabPanel("Tabell", tableOutput("distTableHp"))
              )
            )
          )
        ),
        tabPanel("Fordeling av wt",
          sidebarLayout(
            sidebarPanel(width = 3,
              sliderInput(inputId = "binsWt",
                          label = "Antall grupper:",
                          min = 1,
                          max = 10,
                          value = 5)
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Figur", plotOutput("distPlotWt")),
                tabPanel("Tabell", tableOutput("distTableWt"))
              )
            )
          )
        ),
        tabPanel("Vurdering",
          sidebarLayout(
            sidebarPanel("Med hensikt ingen visning", width = 3),
            mainPanel(htmlOutput("vurdering3niva", inline = TRUE))
          )
        )
      )
    ),
    tags$script(HTML("var header = $('.navbar> .container-fluid');
                       header.append('<div style=\"float:right\">Company name<br>text here</div>');
                       console.log(header)"))
  )
)
