# Define UI for package install app ----
ui <- fluidPage(

  shinyjs::useShinyjs(),

  # App title ----
  titlePanel("Install package from GitHub"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId = "package",
                label = "Package:",
                choices = c("nordummy", "rapbase")),
      selectInput(inputId = "branch",
                  label = "Branch:",
                  choices = c("rel", "master")),
      actionButton(inputId = "install",
                   label = "Install")
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # output
      p(em("System message:")),
      verbatimTextOutput("sysMessage"),
      p(em("Function message:")),
      verbatimTextOutput("funMessage")

    )
  )
)
