library(shiny)
library(ggplot2)
library(raptools)
library(yaml)

# For some reason Shiny Server does not get the server locale settings right.
# To display dates correct, enforce locale here:
Sys.setlocale("LC_TIME", "nb_NO.UTF-8")

shinyServer(function(input, output, session) {

  ### FUNCTIONS ###

  # insert "" for a name/symbol value in one-level lists
  padList <- function(list, padding = "NO DEFAULT, PLEASE SET!") {
    if (is.pairlist(list)){
      list <- as.list(list)
    }
    for (i in 1:length(names(list))) {
      if (is.name(list[[i]])) {
        list[[i]] <- padding
      }
    }
    list
  }

  ### REACTIVES

  ## Data

  r <- reactiveValues(rd=readAutoReportData())
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
      for (i in 1:length(params)){
        paramsListVector[[i]] <- as.list(params[i])
      }
      newReportConfList$params <- paramsListVector
    }
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
      selectByReg(r$rd, input$reg)
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
    # set end to a year from start
    start <- as.POSIXlt(input$from)
    end <- start
    end$year <- end$year + 1
    # skip last day
    end$yday <- end$yday - 1
    s <- seq(from = start, to = end, by = input$interval)
    auto$days <- unique(as.integer(format(s, "%j")))
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
    require(input$newReg, character.only = TRUE)
    f <- unlist(as.list(lsf.str(paste("package", input$newReg, sep = ":"))))
    selectInput("regFun", "Velg rapportfunksjon:", f)

  })

  # dynamic select params in function
  output$regFunParamsControls <- renderUI({
    if (input$newReg=="") {
      choices <- c("Velg pakke og funksjon først...")
    } else {
      choices <- c(names(formals(input$regFun)))
    }
    selectInput("regFunParams", "Velg funksjonsparameter:",
                choices = choices)
  })

  # dynamic set params in function
  output$regFunParamsValueControls <- renderUI({
    if (input$newReg == "") {
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
    if (input$newReg == "") {
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
      val <- names(selectByReg(r$rd, input$delReg))
    } else {
      val <- names(r$rd)
    }
    selectInput("delRep", "Velg rapport:", val)
  })

  output$delSummary <- renderText({
    as.yaml(r$rd[input$delRep])
  })
})
