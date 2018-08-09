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

  msg <- paste("This is a simple test of automated reports. Arguments provided:\n",
               "aNum:", as.character(aNum), ",\n",
               "aChar:", aChar, ",\n",
               "anExp:", as.character(anExp), "\n")
  fileName <- paste0(tempfile(), ".txt")
  con <- file(fileName, "w")
  cat(msg, file = fileName)
  close(con)

  fileName

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



#' Run reports as defined in yaml config and ship content by email
#'
#' Usually to be called by a scheduler, e.g. cron. If the provided day of
#' year matches those of the config the report is run as otherwise specified in
#' config. Functions called upon are expected to return a path to a file that
#' can be attached to an email. The email itself is defined and sent to
#' recipients defined in the config
#'
#' @param dayNumber Integer day of year where January 1st is 1. Defaults to
#' current day, i.e. as.POSIXlt(Sys.Date())$yday+1 (POSIXlt yday is base 0)
#' @param dryRun Logical defining if emails are to be sent. If TRUE a message
#' with reference to the payload file is given but no emails will actually be
#' sent. Default is FALSE
#'
#' @return Emails with corresponding file attachment. If dryRun == TRUE just a
#' message
#' @export
#'
#' @examples
#' \dontrun{
#' runAutoReport()
#' }

runAutoReport <- function(dayNumber = as.POSIXlt(Sys.Date())$yday+1,
                          dryRun = FALSE) {

  # get config
  reps <- readAutoReportData()

  # standard text for email body
  stdTxt <- readr::read_file(system.file("autoReportStandardEmailText. txt",
                                         package = "rapbase"))

  for (i in 1:length(reps)) {
    rep <- reps[[i]]
    # get explicit referenced function
    f <- .getFun(paste0(rep$package, "::", rep$fun))
    if (dayNumber %in% rep$runDayOfYear) {
      attFile <- do.call(what = f, args = rep$params)
      if (dryRun) {
        message(paste("No emails sent. Attachment is", attFile))
      } else {
        # prepare email
        from <- "<rapporteket@skde.no>"
        to <- rep$email
        subject <- rep$synopsis
        body <- list(stdTxt, sendmailR::mime_part(attFile))
        # ship the shite
        sendmailR::sendmail(from, to, subject, body,
                            control = list(smtpServer="localhost"))

      }
    }
  }
}
