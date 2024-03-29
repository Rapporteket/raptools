library(dplyr)
library(lubridate)
library(magrittr)
library(rapbase)
library(raptools)
library(rpivotTable)
library(shinyalert)
library(shinyjs)
library(ggplot2)
library(raptools)
library(yaml)

server <- function(input, output, session) {

  # session persistent data
  ## from env
  instance <- Sys.getenv("R_RAP_INSTANCE")
  configPath <- Sys.getenv("R_RAP_CONFIG_PATH")
  ## set network proxy
  conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml",
                             packageName = "rapbase")
  if (!is.null(conf$network$proxy$http)) {
    proxyUrl <- conf$network$proxy$http
  } else {
    proxyUrl <- NULL
  }
  ## from github api
  path <- "orgs/rapporteket/repos?per_page=100"
  repo <- githubApi(path = path, proxyUrl = proxyUrl)$content
  repo <- repo %>%
    dplyr::select(name, default_branch, pushed_at) %>%
    dplyr::filter(name != "raptools") %>%
    dplyr::arrange(desc(pushed_at))
  ## from file
  f <- file.info(
    list.files("/var/log/shiny-server", full.names = TRUE)
  )
  f <- f %>%
    dplyr::arrange(desc(mtime)) %>%
    dplyr::slice_head(n = 50)
  shinyLogFile <- rownames(f)
  names(shinyLogFile) <- basename(rownames(f))
  shinyLogFile <- shinyLogFile[names(shinyLogFile) != "access.log"]

  # widget
  output$appUserName <- renderText(getUserFullName(session))
  output$appOrgName <- renderText(paste(getUserReshId(session),
                                        getUserRole(session),
                                        sep = ", "))
  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session,
                                                 callerPkg = "raptools")
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })


  # Gjenbrukbar funksjon for å bearbeide Rmd til html
  htmlRenderRmd <- function(srcFile, params = list()) {
    system.file(srcFile, package = "raptools") %>%
      knitr::knit() %>%
      markdown::markdownToHTML(.,
                               options = c("fragment_only",
                                           "base64_images",
                                           "highlight_code")) %>%
      shiny::HTML()
  }


  # Info
  # Various calls for session data from rapbase and systemn settings
  output$callUser <- renderText({
    paste("rapbase::getUserName(session):",
          rapbase::getUserName(session))
  })
  output$callGroups <- renderText({
    paste("rapbase::getUserGroups(session):",
          rapbase::getUserGroups(session))
  })
  output$callReshId <- renderText({
    paste("rapbase::getUserReshId(session):",
          rapbase::getUserReshId(session))
  })
  output$callRole <- renderText({
    paste("rapbase::getUserRole(session):",
          rapbase::getUserRole(session))
  })

  output$callEmail <- renderText({
    paste("rapbase::getUserEmail(session):",
          rapbase::getUserEmail(session))
  })

  output$callFullName <- renderText({
    paste("rapbase::getUserFullName(session):",
          rapbase::getUserFullName(session))
  })

  output$callPhone <- renderText({
    paste("rapbase::getUserPhone(session):",
          rapbase::getUserPhone(session))
  })

  output$envInstance <- renderText({
    instance
  })

  output$envConfigPath <- renderText({
    configPath
  })

  output$locale <- renderText({
    Sys.getlocale()
  })


  # Install packages
  if (instance == "PRODUCTION") {
    checklist <- prodChecklist
    doc <- "prod_install.Rmd"
  }
  if (instance == "QA") {
    checklist <- qaChecklist
    doc <- "qa_install.Rmd"
  }

  repoBranch <- shiny::reactive({
    shiny::req(input$repo)
    path <- paste0("repos/rapporteket/", input$repo, "/branches")
    branch <- githubApi(path, proxyUrl)$content
    dplyr::filter(branch, .data$name != "gh-pages")$name
  })

  repoRelease <- shiny::reactive({
    shiny::req(input$repo)
    path <- paste0("repos/rapporteket/", input$repo, "/releases")
    rel <- githubApi(path, proxyUrl)$content
    rel$tag_name
  })

  checklistSelected <- shiny::reactive({
    shiny::req(input$repo)
    NULL
  })

  output$repoSelector <- shiny::renderUI(
    shiny::selectInput(inputId = "repo", label = "Pakke:", choices = repo$name)
  )

  output$branchSelector <- renderUI(
    switch(instance,
      DEV = shiny::selectInput(inputId = "branch", label = "Grein:",
                               choices = c(repoBranch(), repoRelease())),
      TEST = shiny::selectInput(inputId = "branch", label = "Grein:",
                                choices = c(repoBranch(), repoRelease())),
      QA = shiny::selectInput(
        inputId = "branch", label = "Grein",
        choices = c(repoBranch(), repoRelease())),
      PRODUCTION = shiny::selectInput(inputId = "branch", label = "Versjon:",
                                      choices = repoRelease())
    )
  )

  output$doc <- shiny::renderUI({
    shiny::req(input$repo)
    if (instance %in% c("QA", "PRODUCTION")) {
      htmlRenderRmd(doc, params = list(repo = input$repo,
                                       branch = input$branch))
    } else {
      NULL
    }
  })

  output$checklist <- renderUI(
    if (exists("checklist")) {
      checkboxGroupInput(inputId = "manControl",
                         label = "Sjekk at du faktisk har:",
                         choices = checklist,
                         selected = checklistSelected())
    } else {
      NULL
    }
  )

  output$installButton <- renderUI(
    if (exists("checklist")) {
      if (length(input$manControl) == length(checklist) &&
          input$branch != "") {
        actionButton(inputId = "install", label = "Install")
      } else {
        NULL
      }
    } else {
      actionButton(inputId = "install", label = "Install")
    }

  )

  installPackage <- observeEvent(input$install, {
    branch <- input$branch
    withCallingHandlers({
      shinyjs::html("sysMessage", "")
      shinyjs::html("funMessage", "")
      shinyjs::html("funMessage",
                    raptools::installGithubPackage(
                      input$repo, branch, input$upgradeDeps, TRUE))
    },
    message = function(m) {
      shinyjs::html(id = "sysMessage", html = m$message, add = TRUE)
    })
  })


  #------------Logwatcher-----
  output$logSelector <- renderUI(
    shiny::selectInput(
      inputId = "selectLog",
      label = "Log:",
      choices = list(
        "Application level" = "app",
        "Report level" = "report")
    )
  )

  logData <- reactive(
    raptools::getLogData(req(input$selectLog)) %>%
      dplyr::mutate(
        time = as.POSIXct(time),
        year = lubridate::year(time),
        month = lubridate::month(time),
        day = lubridate::day(time),
        weekday = lubridate::wday(
          time,
          week_start = 1,
          abbr = FALSE))
  )
  output$logPivottTable <- rpivotTable::renderRpivotTable(
    rpivotTable::rpivotTable(logData())
  )
  #-------------Autoreport UI------
  output$autoReportSidebar <- renderUI({
    if (input$autoreport == "oversikt") {
      shiny::tagList(
        uiOutput("regControls"),
        uiOutput("repControls"),
        tags$hr(),
        actionButton("saveAll", "Lagre alle data")
      )
    } else if (input$autoreport == "lagNy") {
      shiny::tagList(
        uiOutput("pkgControls"),
        textInput(
          "syn", "Tekstlig beskrivelse (emnefelt epost):",
          "Rutinemessig rapport ang..."),
        uiOutput("regFunControls"),
        uiOutput("regFunParamsControls"),
        uiOutput("regFunParamsValueControls"),
        actionButton("setParam", "Sett parameterverdi"),
        tags$hr(),
        textInput("email", "Epostmottaker:"),
        uiOutput("handleEmailControls"),
        tags$hr(),
        selectInput(
          "interval", "Intervall:",
          list(
            dag = "DSTday", uke = "week", mnd = "month",
            kvartal = "quarter", aar = "year"),
          selected = "month"),
        dateInput("from", "Start dato:"),
        actionButton("setDays", "Sett kjøredager"),
        tags$hr(),
        textInput("repId", "Rapport id:", "UnikIdentifikatorAvRapporten"),
        uiOutput("addReportControls")
      )
    }else if (input$autoreport == "slett") {
      shiny::tagList(
        uiOutput("delRegControls"),
        uiOutput("delRepControls"),
        actionButton("doDelRep", "SLETT!")
      )
    }
  })

  #------AR server----------------
  # insert "" for a name/symbol value in one-level lists
  padList <- function(list, padding = "NO DEFAULT, PLEASE SET!") {
    if (is.pairlist(list)) {
      list <- as.list(list)
    }
    for (i in seq_len(length(names(list)))) {
      if (is.name(list[[i]])) {
        list[[i]] <- padding
      }
    }
    list
  }

  ### REACTIVES

  ## Data

  r <- reactiveValues(rd = readAutoReportData())
  newReportConfList <- reactiveValues()
  newReportConfList <- list()
  newReportConfList$params <- NULL
  # for setting param value pairs
  pv <- reactiveValues(p = NULL, v = NULL)
  # for email address vector
  sendTo <- reactiveValues(email = vector())
  # for setting run days
  auto <- reactiveValues(days = NULL)


  ## Functions

  parseNewConfig <- reactive({
    newReportConfList$synopsis <- input$syn
    newReportConfList$package <- input$newReg
    newReportConfList$fun <- input$regFun
    # set param value pairs
    if (input$setParam > 0) {
      params <- pv$v
      names(params) <- pv$p
      paramsListVector <- list()
      for (i in seq_len(length(params))) {
        paramsListVector[[i]] <- as.list(params[i])
      }
      newReportConfList$params <- paramsListVector
    }
    # set owner
    newReportConfList$owner <- getUserName(session)
    # set email
    newReportConfList$email <- sendTo$email
    # set run days
    if (input$setDays > 0) {
      newReportConfList$runDayOfYear <- auto$days
    }
    newReportConfList
  })


  # select config by reg
  rds <- reactive({
    if (input$reg == "Alle") {
      r$rd
    } else {
      rapbase::filterAutoRep(r$rd, by = "package", input$reg)
    }
  })

  # select days
  rdoy <- reactive({
    if (length(input$rep) == 0) {
      unlist(sapply(r$rd, "[[", "runDayOfYear"), use.names = FALSE)
    } else {
      if (input$rep == "Alle") {
        dat <- rds()
        if (length(dat) > 1) {
          unlist(sapply(dat, "[[", "runDayOfYear"), use.names = FALSE)
        } else {
          dat[[1]]$runDayOfYear
        }
      } else {
        rds()[[input$rep]]$runDayOfYear
      }
    }
  })

  # get global max of reports per day
  getMaxReps <- reactive({
    if (length(r$rd) > 1) {
      dat <- unlist(sapply(r$rd, "[[", "runDayOfYear"), use.names = FALSE)
    } else {
      dat <- r$rd[[1]]$runDayOfYear
    }
    max(as.vector(table(dat)))
  })


  ### OBSERVERS

  # store all data
  observeEvent(input$saveAll, {
    writeAutoReportData(config = r$rd)
  })

  # reset parameter value pairs to default each time a new param is selected
  observeEvent(input$regFun, {
    if (input$regFun != "Velg pakke først...") {
      pv$p <- names(padList(formals(input$regFun)))
      pv$v <- padList(formals(input$regFun))
      newReportConfList$params <- NULL
    }
  })

  observeEvent(input$setParam, {
    # new input will all be of class character. Hence, actual numbers need to
    # be converted
    if (is.numeric(type.convert(input$regFunParamsValue))) {
      pv$v[input$regFunParams] <- type.convert(input$regFunParamsValue)
    } else {
      pv$v[input$regFunParams] <- input$regFunParamsValue
    }
  })

  observeEvent(input$doAddEmail, {
    sendTo$email <- c(sendTo$email, input$email)
  })

  observeEvent(input$doDelEmail, {
    sendTo$email <- sendTo$email[!sendTo$email == input$email]
  })

  observeEvent(input$setDays, {
    auto$days <- makeRunDayOfYearSequence(startDay = input$from,
                                          interval = input$interval)
  })

  observeEvent(input$addReport, {
    r$rd[[eval(input$repId)]] <- parseNewConfig()
  })

  observeEvent(input$doDelRep, {
    ind <- names(r$rd) == input$delRep
    tmp <- r$rd[!ind]
    r$rd <- tmp
  })


  ### OUTPUT

  ## Oversikt

  # dynamic select registries present
  output$regControls <- renderUI({
    selectInput("reg", "Register", c("Alle", getRegs(r$rd)))
  })

  # dynamic select Rapporteket packages present
  output$pkgControls <- renderUI({
    selectInput("newReg", "Register:", getRapPackages())
  })

  # dynamic select reports present
  output$repControls <- renderUI({
    if (length(input$reg) == 0) {
      selectInput("rep", "Rapport", c("Alle"))
    } else {
      if (input$reg == "Alle") {
        selectInput("rep", "Rapport", c("Alle"))
      } else {
        selectInput("rep", "Rapport", c("Alle", names(rds())))
      }
    }
  })

  output$calendar <- renderPlot({
    plot(calendarAutoReport(runDayOfYear = rdoy(),
                            pointRangeMax = getMaxReps()))
  })


  ## Lag ny

  # dynamic select function in package
  output$regFunControls <- renderUI({
    require(req(input$newReg), character.only = TRUE)
    f <- unlist(as.list(lsf.str(paste("package", input$newReg, sep = ":"))))
    selectInput("regFun", "Velg rapportfunksjon:", f)

  })

  # dynamic select params in function
  output$regFunParamsControls <- renderUI({
    if (req(input$newReg) == "") {
      choices <- c("Velg pakke og funksjon først...")
    } else {
      choices <- c(names(formals(input$regFun)))
    }
    selectInput("regFunParams", "Velg funksjonsparameter:",
                choices = choices)
  })

  # dynamic set params in function
  output$regFunParamsValueControls <- renderUI({
    if (req(input$newReg) == "") {
      value <- "Velg pakke, funksjon og parameter først..."
    } else {
      value <- pv$v[pv$p == input$regFunParams]
    }
    textInput("regFunParamsValue", "Sett verdi for parameter valgt over",
              value = value)
  })

  # dynamic add or delete email
  output$handleEmailControls <- renderUI({
    if (input$email == "") {
      tags$strong("Angi epostadresse over")
    } else {
      if (input$email %in% sendTo$email) {
        actionButton("doDelEmail", "Slett epostmottaker")
      } else {
        actionButton("doAddEmail", "Legg til epostmottaker")
      }
    }
  })

  # dynamic set text input with updated entries in yaml
  output$yamlNewReg <- renderText({
    if (req(input$newReg) == "") {
      ""
    } else {
      as.yaml(parseNewConfig())
    }
  })

  output$addReportControls <- renderUI({
    if (input$repId %in% names(r$rd) |
        length(grep("[^[:alnum:]]", input$repId)) > 0 |
        input$repId == "") {
      tags$strong(paste("Rapport id må være unik, bokstaver og/eller tall og",
                        "uten mellomrom for å registrere en ny rapport!"))
    } else {
      actionButton("addReport", "Registrer rapporten")
    }
  })


  ## Slett

  output$delRegControls <- renderUI({
    selectInput("delReg", "Velg register:", getRegs(r$rd))
  })

  output$delRepControls <- renderUI({
    if (length(input$delReg) > 0) {
      val <- names(rapbase::filterAutoRep(r$rd, by = "package", input$delReg))
    } else {
      val <- names(r$rd)
    }
    selectInput("delRep", "Velg rapport:", val)
  })

  output$delSummary <- renderText({
    as.yaml(r$rd[input$delRep])
  })


  #----------Server information------
  output$shinyServerAppLogControls <- shiny::renderUI({
    shiny::selectInput(inputId = "shinyServerAppLog", label = "Velg loggfil:",
                       choices = as.list(shinyLogFile))
  })

  output$shinyServerLog <- shiny::renderUI({
    shiny::req(input$shinyServerAppLog)
    rawText <- readLines(input$shinyServerAppLog)
    splitText <- stringi::stri_split(rawText, regex = "\\n")
    lapply(splitText, shiny::p)
  })

  output$rapbaseConfig <- shiny::renderText({
    raptools::getConfigTools(fileName = "rapbaseConfig")
  })



}
