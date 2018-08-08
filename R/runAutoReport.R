#' Simple test of automated report
#'
#' Simple test of automated reporting from definitions provided in a yaml
#' config file
#'
#' @param aNum a number
#' @param aChar a character
#' @param anExp an expression
#'
#' @return A simple message listing the contents of the arguments
#' @export
#'
#' @examples
#' .testAutoReport()

.testAutoReport <- function(aNum = 1, aChar = "a", anExp = Sys.Date()) {

  msg <- paste("These are the arguments provided:\n",
               "aNum:", as.character(aNum), ",\n",
               "aChar:", aChar, ",\n",
               "anExp:", as.character(anExp), "\n")

  message(msg)

}

#' Run reports as defined in yaml config
#'
#' Usually to be called by a scheduler, e.g. cron. If the proveded day(s) of
#' year matches the report is run as specified in config
#'
#' @param dayNumber Integer day of year where January 1st is 1. Defaults to
#' current day, i.e. as.POSIXlt(Sys.Date())$yday+1
#'
#' @return By itself nothing except whatever the called functions might provide
#' @export
#'
#' @examples
#' \dontrun{
#' runAutoReport()
#' }

runAutoReport <- function(dayNumber = as.POSIXlt(Sys.Date())$yday+1) {

  reps <- readAutoReportData()

  for (i in 1:length(reps)) {
    rep <- reps[[i]]
    if (dayNumber %in% rep$runDayOfYear) {
      do.call(what = paste0(rep$packageName, rep$functionCall,
                            collapse = "::"),
              args = rep$params)
    }
  }
}
