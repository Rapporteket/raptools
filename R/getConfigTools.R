#'Gets configuration files
#'
#'@param fileName a string that determines which config file to retrieve.
#'the default is "rapbaseConfig"
#'
#'@return config file based on the input
#'
#'@export
#'
#'
getConfigTools <- function(fileName = "rapbaseConfig") {
  stopifnot(fileName == "rapbaseConfig")
  pth <- Sys.getenv("R_RAP_CONFIG_PATH")
  if (pth == "") {
    stop("cant find R_RAP_CONFIG_PATH")
  }
  pthConfig <- paste0(pth, "/",fileName, ".yml")

  has_Config <-  pthConfig %>%
    file.path() %>%
    file.exists()

  if (!has_Config ) {
    stop("cant find the Config file")
  }

  config <- yaml::as.yaml(yaml::read_yaml(pthConfig))

  return(invisible(config))
}
