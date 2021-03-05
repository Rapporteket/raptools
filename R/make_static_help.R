#' Make static html of R doc in package
#'
#' @param pkg string providing name of (installed) package containing target
#' docs
#' @param links set NULL to avoid (local) intra-doc linking. Defaults to
#' tools::findHTMLlinks()
#'
#' @return (a set of) html file(s) in system.file('html', package = pkg)
#' @export
#'
#' @examples
#' static_help("raptools", links = NULL)
#' static_help("raptools")

static_help <- function(pkg, links = tools::findHTMLlinks()) {
  wd <- getwd()
  helpdir <- system.file("html", package = pkg)
  setwd(helpdir)
  message("Generated help files will be placed in ", helpdir)
  pkgRdDB <- eval(
    parse(
      text = "tools:::fetchRdDB(file.path(find.package(pkg),'help', pkg))"))
  force(links); topics <- names(pkgRdDB)
  for (p in topics) {
    tools::Rd2HTML(pkgRdDB[[p]],
                   paste(p, "html", sep = "."),
                   package = pkg,
                   Links = links,
                   no_links = is.null(links))
  }
  setwd(wd) # Get back to the current working directory
}
