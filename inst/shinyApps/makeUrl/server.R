# Define server logic providing details and summary url
server <- function(input, output) {

  makeFrame <- reactive({
    data.frame(para = c("App", "user", "groups", "resh_id", "role"),
               value = c(input$app,
                         input$user,
                         paste(input$groups, collapse = ","),
                         input$resh_id, input$role),
               type = c("base url", "proxy auth", "proxy auth", "proxy head", "proxy head"),
               shiny_conf = c(paste0("server {... location { /", input$app, " ...} ...}"),
                              paste0("auth_proxy ", input$proxyUser, ";"),
                              paste0("auth_proxy ", input$proxyGroups, ";"),
                              paste0("server {... whitelist_headers ",
                                     input$whitelistResh,"; ...}"),
                              paste0("server {... whitelist_headers ",
                                     input$whitelistRole, "; ...}")),
               shiny_session = c("na",
                                 "session$user",
                                 "session$groups",
                                 paste0("session$request$HTTP_",
                                        toupper(input$whitelistResh)),
                                 paste0("session$request$HTTP_",
                                        toupper(input$whitelistRole))
                                 )
    )
  })

  compileUrl <- reactive({
    paste0(input$baseUrl, "/", input$app, "?",
           paste0(input$proxyUser, "=", input$user, "?",
                  input$proxyGroups, "=", paste(input$groups, collapse = ","),
                  "?",
                  input$whitelistResh, "=", input$resh_id, "?",
                  input$whitelistRole, "=", input$role))
  })

  # Generate table
  output$tab <- renderTable({
    makeFrame()
  })

  # Generate concatenated url
  output$url <- renderUI({
    url <- compileUrl()
    tags$a(url, href=url)
  })

}
