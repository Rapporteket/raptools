#'Get app or report log data
#'
#'@param log a string that determines which log to retrieve. It can be either "report" or "app"
#'
#'@return reportLog or appLog depending on the input
#'
#'@export
#'
#'@example
#'getLogData("report")
#'getLogData("app")

getLogData <- function(log) {
  stopifnot(log == "report" | log == "app")
  pth <- Sys.getenv("R_RAP_CONFIG_PATH")
  fileLog <- switch (log,
          "report" = paste0(pth, "/reportLog.csv"),
          "app" = paste0(pth, "/appLog.csv")
  )
  logData <- read.csv(fileLog,header = TRUE, stringsAsFactors = FALSE)

  return(invisible(logData))
}
