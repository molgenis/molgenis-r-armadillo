#' Install package
#'
#' Installs a user defined package.
#'
#' @param paths the path to the package, can be a vector of string
#' @param profile the selected profile
#'
#' @export
armadillo.install_packages <- function(paths, profile="default") {
  if(missing(paths) || paths == "") {
    stop("You need to specify the full path of the package; e.g. 'C:/User/test.tar.gz'")
  }
  
  auth_token <- getOption("MolgenisArmadillo.auth.token", NULL)
  armadillo_endpoint <- getOption("MolgenisArmadillo.armadillo.endpoint", NULL)
  
  if(is.null(armadillo_endpoint) || is.null(auth_token)) {
    stop("Please login using; 'armadillo.login('http://armadillo', 'http://minio')'")
  }
  
  headers <- httr::add_headers("Authorization" = paste0("Bearer ", auth_token))
  
  handle <- httr::handle(armadillo_endpoint)
  
  response <- httr::POST(
    handle = handle,
    path = "/select-profile",
    body = profile,
    config = headers
  )
  
  .handle_request_error(response)
  
  if (response$status == 404 && profile != "default") {
    stop(paste0("Profile not found: '", profile, "'"))
  }
  
  if(length(paths) > 1) {
    for(path in paths) {
      message(paste0("Installing package [ ' ", path, " ' ]'"))
      .install_package(handle, headers, path, profile) 
      message(paste0("Package [ ' ", path, " ' ] installed'"))
    }
  } else {
    message(paste0("Installing package [ ' ", paths, "' ]'"))
    .install_package(handle, headers, paths, profile) 
    message(paste0("Package [ ' ", paths, " ' ] installed'"))
  }
  
}

#' Install a R-package
#'
#' @param handle the connection with the server
#' @param path a path to one R-package
#' @param profile the profile where it needs to be installed on
#'
#' @noRd
.install_package <- function(handle, headers, path, profile) {
  
  
  file <- httr::upload_file(path)
  
  response <- httr::POST(
    handle = handle,
    path = "/install-package",
    body = list(file = file),
    config = c(headers, httr::content_type("multipart/form-data"))
  )
  .handle_request_error(response)
}