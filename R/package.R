#' Install package
#'
#' Installs a user defined package.
#'
#' @param paths the path(s) to the package(s), can be a vector or a string
#' @param profile the selected profile
#'
#' @export
armadillo.install_packages <- function(paths, profile = "default") { # nolint
  msg <- paste0(
    "You need to specify the full path(s) of the package(s);",
    "e.g. 'C:/User/test.tar.gz'")
  .is_empty(msg, paths)

  connection <- .get_armadillo_connection()

  response <- httr::POST(
    handle = connection$handle,
    path = "/select-profile",
    body = profile,
    config = connection$headers
  )

  .handle_request_error(response)

  if (response$status_code == 404 && profile != "default") {
    stop(paste0("Profile not found: [ '", profile, "' ]"))
  }

  invisible(lapply(paths, .install_package))
}

#' Install an R-package
#'
#' @param path a path to one R-package
#'
#' @noRd
.install_package <- function(path) {
  connection <- .get_armadillo_connection()

  file <- httr::upload_file(path)

  message(paste0("Attempting to install package [ '", path, "' ]"))
  response <- httr::POST(
    handle = connection$handle,
    path = "/install-package",
    body = list(file = file),
    config = c(connection$headers, httr::content_type("multipart/form-data"))
  )

  .handle_request_error(response)
  if (response$status_code == 404) {
    stop(paste0(
      "Endpoint doesn't exist. Make sure you're running ",
      "Armadillo in development mode."))
  }

  message(paste0("Package [ '", path, "' ] installed"))
}

#' Whitelist packages
#'
#' When you installed developer packages you need to whitelist
#' them in other to be able to test them in the Armadillo platform.
#'
#' @param pkgs packages that need to be added to the whitelist
#' @param profile the profile you want to whitelist the packages on
#'
#' @export
armadillo.whitelist_packages <- function(pkgs, profile = "default") { # nolint
  msg <- paste0(
    "You need to specify the the package(s) you want to whitelist; ",
    "e.g. 'DSI'")
  .is_empty(msg, pkgs)

  connection <- .get_armadillo_connection()

  response <- httr::POST(
    handle = connection$handle,
    path = "/select-profile",
    body = profile,
    config = connection$headers
  )

  .handle_request_error(response)

  if (response$status_code == 404 && profile != "default") {
    stop(paste0("Profile not found: [ '", profile, "' ]"))
  }

  invisible(lapply(pkgs, .whitelist_package))

  response <- httr::GET(
    handle = connection$handle,
    path = "/whitelist",
    config = c(connection$headers)
  )

  .handle_request_error(response)

  message(paste0("Packages whitelisted: "))
  message(paste(
    paste0(
      " * [ '", httr::content(response), "' ]",
      collapse = "\n")))
}

#' Add package to whitelist
#'
#' @param pkg package to whitelist
#'
#' @noRd
.whitelist_package <- function(pkg) {
  connection <- .get_armadillo_connection()

  message(paste0("Attempting to whitelist package [ '", pkg, "' ]"))
  response <- httr::POST(
    handle = connection$handle,
    path = paste0("/whitelist/", pkg),
    config = c(connection$headers)
  )

  .handle_request_error(response)
  if (response$status_code == 404) {
    stop(paste0(
      "Endpoint doesn't exist. ",
      "Make sure you're running Armadillo in development mode."))
  }
  message(paste0("Package [ '", pkg, "' ] added to the whitelist"))
}



#' Get Armadillo connection details
#'
#' @noRd
.get_armadillo_connection <- function() {
  auth_token <- getOption("MolgenisArmadillo.auth.token", NULL)
  armadillo_endpoint <- getOption("MolgenisArmadillo.armadillo.endpoint", NULL)

  headers <- httr::add_headers("Authorization" = paste0("Bearer ", auth_token))
  if (is.null(armadillo_endpoint) || is.null(auth_token)) {
    stop(
      "Please login using: ",
      "'armadillo.login('http://armadillo', 'http://minio')'")
  }

  list(handle = httr::handle(armadillo_endpoint), headers = headers)
}


#' Checks for empty list or character
#'
#' Stops with the supplied error message
#' when the check fails
#'
#' @noRd
.is_empty <- function(msg, value) {
  if (length(nchar(value)) > 1) {
    invisible(lapply(value, .is_empty, msg = msg))
  } else if (is.character(value)) {
    if (nchar(value) == 0) {
      stop(msg)
    }
  } else {
    stop(paste0(
      "Datatype of package should be character: [",
      value,
      "] is type of [",
      typeof(value),
      "]"))
  }
}
