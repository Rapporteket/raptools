#'Get app or report log
#'
#'@param log a string that determines which log to retrieve.
#'It can be either "report" or "app"
#'
#'@return reportLog or appLog depending on the input
#'
#'@export
#'
#'
getLogData <- function(log) {
  stopifnot(log == "report" | log == "app")
  pth <- Sys.getenv("R_RAP_CONFIG_PATH")
  if (pth == "") {
    stop("cant find R_RAP_CONFIG_PATH")
  }

  fileLog <- switch(log,
    "report" = paste0(pth, "/reportLog.csv"),
    "app" = paste0(pth, "/appLog.csv")
  )
  is_logging <-  fileLog %>% file.path() %>% file.exists()
  if (!is_logging) {
    stop("cant find the log")
  }

  logData <- utils::read.csv(fileLog,
                             header = TRUE,
                             stringsAsFactors = FALSE)
  return(invisible(logData))
}
