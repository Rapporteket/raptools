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
    if (!file.exists(file.path(path, fileName))) {
      warning(paste("No configuration file found in", path,
                    "A new will be made from package default"))
      file.copy(system.file(fileName, package = packageName), path)
    }
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
    con <- file(system.file(fileName, package = packageName), "w")
  } else {
    oriFile <- file.path(path, fileName)
    # in case we screw-up, make a backup
    tmpTag <- as.character(as.integer(as.POSIXct(Sys.time())))
    nameParts <- strsplit(fileName, "[.]")[[1]]
    bckFileName <- paste0(nameParts[1], tmpTag, ".", nameParts[-1])
    bckFilePath <- file.path(path, "bck")
    file.copy(from = oriFile, to = bckFilePath, overwrite = TRUE)
    file.rename(from = file.path(bckFilePath, fileName),
                to = file.path(bckFilePath, bckFileName))
    # to maintain some order, remove files older than 30 days
    files <- file.info(list.files(bckFilePath, full.names = TRUE))
    rmFiles <- rownames(files[difftime(Sys.time(), files[, "mtime"],
                              units = "days") > 30, ])
    file.remove(rmFiles)
    con <- file(oriFile, "w")
  }
  yaml::write_yaml(config, con)
  close(con)
}


#' Select data on one registry from config (list)
#'
#' Pick all config corresponding to a given registry. Registry name is not
#' given as such, but rather as its corresponding R package name. Hence, a
#' registry must be given as the name of its R package
#'
#' @param config list of configuration for automated reports
#' @param reg string giving the exact name of the R package for the registry
#'
#' @return list witn config for registry reg
#' @export

selectByReg <- function(config, reg) {

  ind <- integer()
  for (i in 1:length(config)) {
    if (config[[i]]$package == reg) {
      ind <- c(ind, i)
    }
  }
  c(config[ind])
}



#' Provide vector of registries (\emph{i.e.} their R packages) in config
#'
#' @param config list of configuration for automated reports
#'
#' @return character vector of rgistry (package) names
#' @export

getRegs <- function(config) {
  regs <- vector(mode = "character")
  for (i in 1:length(config)) {
    reg <- config[[i]]$package
    if (!(reg %in% regs)) {
      regs <- c(regs, reg)
    }
  }
  regs
}
