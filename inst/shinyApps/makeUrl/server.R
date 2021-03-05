# Define server logic providing details and summary url

library(httr)

server <- function(input, output, session) {

  observe({
    cd <- session$clientData
    updateTextInput(session, inputId = "baseUrl",
                    value = paste0("http://", cd$url_hostname, ":",
                                   cd$url_port))
    })

  makeFrame <- reactive({
    data.frame(para = c("app", "user", "groups", "resh_id", "role"),
               value = c(input$app,
                         input$user,
                         paste(input$groups, collapse = ","),
                         input$resh_id, input$role),
               type = c("base url", "proxy auth", "proxy auth", "proxy head",
                        "proxy head"),
               shiny_conf = c(paste0("server {... location { /", input$app,
                                     " ...} ...}"),
                              paste0("auth_proxy ", input$proxyUser, ";"),
                              paste0("auth_proxy ", input$proxyGroups, ";"),
                              paste0("server {... whitelist_headers ",
                                     input$whitelistResh, "; ...}"),
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
    paste0(input$baseUrl, input$app, "/?",
           paste0(input$proxyUser, "=", input$user, "&",
                  input$proxyGroups, "=", paste(input$groups, collapse = ","),
                  "&",
                  input$whitelistResh, "=", input$resh_id, "&",
                  input$whitelistRole, "=", input$role))
  })

  # Generate table
  output$tab <- renderTable({
    makeFrame()
  })

  # Generate concatenated url
  output$url <- renderUI({
    url <- compileUrl()
    tags$a(url, href = url)
  })

  # Generate http-content
  output$httpRequest <- renderPrint({
    parseQueryString(session$clientData$url_search)
  })

  # Various calls for session data from rapbase
  output$callUser <- renderText({
    paste("rapbase::getUserName():",
          rapbase::getUserName())
  })
  output$callGroups <- renderText({
    paste("rapbase::getUserGroups():",
          rapbase::getUserGroups())
  })
  output$callReshId <- renderText({
    paste("rapbase::getUserReshId():",
          rapbase::getUserReshId())
  })
  output$callRole <- renderText({
    paste("rapbase::getUserRole():",
          rapbase::getUserRole())
  })

}
