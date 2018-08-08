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


#' Provide explicit reference to function for do.call
#'
#' @param x string with explicit reference, i.e. 'package::function'
#'
#' @return value of the exported 'function' in 'package'
#' @export

.getFun <- function(x) {

  if(length(grep("::", x))>0) {
    parts<-strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    x
  }

}



#' Run reports as defined in yaml config
#'
#' Usually to be called by a scheduler, e.g. cron. If the provided day of
#' year matches those of the config the report is run as otherwise specified in
#' config
#'
#' @param dayNumber Integer day of year where January 1st is 1. Defaults to
#' current day, i.e. as.POSIXlt(Sys.Date())$yday+1 (POSIXlt yday is base 0)
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
    # get explicit referenced function
    f <- .getFun(paste0(rep$package, "::", rep$fun))
    if (dayNumber %in% rep$runDayOfYear) {
      do.call(what = f, args = rep$params)
    }
  }
}
