#' Read automated report metadata
#'
#' @param fileName String defining name of the yaml configuration file. Default
#' 'autoReport.yml'
#' @param packageName String defining the package in which the above
#' configuration file resides. A configuration file within an R-package is
#' only used in case the environmental variable 'R_RAP_CONFIG_PATH' is not
#' defined (empty)
#'
#' @return a list of yaml data
#' @export
#'
#' @examples
#' readAutoReportData()

readAutoReportData <- function(fileName = "autoReport.yml", packageName = "raptools") {

  path <- Sys.getenv("R_RAP_CONFIG_PATH")

  if (path == "") {
    stopifnot(file.exists(system.file(fileName, package = packageName)))
    config_file <- system.file(fileName, package = packageName)
  } else {
    stopifnot(file.exists(file.path(path, fileName)))
    config_file <- file.path(path, fileName)
  }

  yaml::yaml.load_file(config_file)

}

#' Write automated report metadata
#'
#' @inheritParams readAutoReportData
#' @param config a list of yaml configuration
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' config <- readAutoReportData()
#' writeAutoReportData(config = config)
#' }

writeAutoReportData <- function(fileName = "autoReport.yml", config,
                                packageName = "raptools") {

  path <- Sys.getenv("R_RAP_CONFIG_PATH")

  if (path == "") {
    # for now, just write into installed package
    yaml::write_yaml(config, system.file(fileName, packageName))
  } else {
    # here, we need to make some sort of backup prior to write. Postphoned
    yaml::write_yaml(config, normalizePath(paste0(path, "/", fileName)))
  }
}
