#' Create and add report to config
#'
#' Adds an entry to the system configuration of reports to run at given
#' intervalls. After generating the configuration from the new entry
#' the function load the current system configuration, adds the new
#' entry and saves the updated system configuration.
#'
#' @param synopsis String with description of the report and to be used in
#' subject field of email distributed reports
#' @param package String with package name also correspondig to registry
#' @param fun String providing name of function to be called for generating
#' report
#' @param paramNames String vector where each element corresponds to the input
#' parameter to be used in the above function
#' @param paramValues String vector with corresponding values to paramNames
#' @param email String with email address to recipient of email containg the
#' report
#' @param runDayOfYear Integer vector with day numbers of the year when the
#' report is to be run
#' @param dryRun Logical defining if global auto report config actually is to
#' be updated. If set to TRUE the actual config (all of it) will be printed to
#' the console. FALSE by default
#'
#' @seealso \code{\link{deleteAutoReport}}
#' @export

createAutoReport <- function(synopsis, package, fun, paramNames, paramValues,
                             owner, email, runDayOfYear, dryRun = FALSE) {

  # make unique id by (hashing) combination of owner and timestamp
  ts <- as.character(as.integer(as.POSIXct(Sys.time())))
  autoRepId <- digest::digest(paste0(owner, ts))

  # make current entry, first named list of param names and values pairs
  l <- list()
  params <- paramValues
  names(params) <- paramNames
  paramsListVector <- list()
  for (i in 1:length(params)){
    paramsListVector[[i]] <- as.list(params[i])
  }

  l$synopsis <- synopsis
  l$package <- package
  l$fun <- fun
  l$params <- paramsListVector
  l$owner <- owner
  l$email <- email
  l$runDayOfYear <- runDayOfYear

  rd <- readAutoReportData()

  rd[[eval(autoRepId)]] <- l

  if (dryRun) {
    print(rd)
  } else {
    writeAutoReportData(config = rd)
  }


}

#' Delete existing report from config
#'
#' @param autoReportId String providing the auto report unique id
#'
#' @seealso \code{\link{createAutoReport}}
#' @export

deleteAutoReport <- function(autoReportId) {

  rd <- readAutoReportData()
  # just stop with an error if report does not exist
  stopifnot(!is.null(rd[[autoReportId]]))
  ind <- names(rd) == autoReportId
  rd <- rd[!ind]
  writeAutoReportData(config = rd)

}

#' Make a sequence of day numbers from av given date and interval
#'
#' This function provides an even sequence of day numbers spanning 365/366
#' days from the start date and interval provided. Mainly to be used in
#' setting up automated reports at Rapporteket
#'
#' @param startDay Start date of sequence. May be provided as a string,
#' \emp{e.g.} \"2019-03-17\" or as class \"Date\". Defaults to today
#' @param interval String representing a valid seq.POSIXt interval such as
#' \"DSTday\", \"week\", \"month\", \"quarter\" or \"year\")
#'
#' @return Integer vector of day numbers
#' @export
#'
#' @examples
#' makeRunDayOfYearSequence(interval = "month")
#'
makeRunDayOfYearSequence <- function(startDay = Sys.Date(), interval) {

  # set end to a year from start
  start <- as.POSIXlt(startDay)
  end <- start
  end$year <- end$year + 1
  s <- seq(from = start, to = end, by = interval)
  # skip redundant end value
  if (length(s) > 1) {
    s <- s[1:length(s)-1]
  }
  unique(as.integer(format(s, "%j")))
}
