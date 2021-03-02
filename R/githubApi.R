#' Make calls to the github API
#'
#' @param path Character string with path to the API resource
#' @param proxyUrl Character string defining a network proxy in the form
#' host:port. Default is NULL in which case the API call will not use a proxy
#'
#' @return A list of class githubApi containing the parsed content, API
#' resource path and the response object
#' @export
#'
#' @examples
#' githubApi("repos/Rapporteket/raptools/branches")

githubApi <- function(path, proxyUrl = NULL) {
  url <- httr::modify_url("https://api.github.com", path = path)
  user_agent <- httr::user_agent("https://github.com/Rapporteket/raptools")

  if (!is.null(proxyUrl)) {
    httr::set_config(httr::use_proxy(url = proxyUrl))
  }

  resp <- httr::GET(url, user_agent)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = TRUE)

  if (httr::status_code(resp) != 200) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "githubApi"
  )
}
