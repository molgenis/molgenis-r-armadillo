#' Install package
#'
#' Installs a user defined package.
#'
#' @param path the path to the package
#' @param profile the selected profile
#'
#' @export
armadillo.install_packages <- function(path, profile="default") {

  if(missing(path) || path == "") {
    stop("You need to specify the full path of the package; e.g. 'C:/User/test.tar.gz'")
  }
  
  auth_token <- getOption("MolgenisArmadillo.auth.token", TRUE)
  auth_endpoint <- getOption("MolgenisArmadillo.auth.endpoint", TRUE)
  
  if(is.null(auth_endpoint) || is.null(auth_token)) {
    stop("Please login using; 'armadillo.login('http://armadillo', 'http://minio')'")
  }
  
  headers <- httr::add_headers("Authorization" = paste0("Bearer ", auth_token))
  
  handle <- httr::handle(auth_endpoint)
  
  file <- httr::upload_file(path)
  if(!file.exists()) {
    stop("Please specificy a valid path for your package.")
  }
  
  response <- httr::POST(
    handle = handle,
    query = list(profile = profile),
    path = "/install-package",
    body = file,
    config = c(headers, httr::content_type("multipart"))
  )
  .handle_request_error(response)
  if (response$status == 404) {
    stop("Something went wrong")
  }
  
}