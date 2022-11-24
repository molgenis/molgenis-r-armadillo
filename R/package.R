#' Install package
#'
#' Installs a user defined package into the provided profile. The package
#' is automatically whitelisted after installation. Only available during
#' development.
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

  response <- httr::POST(
    url = .get_url(),
    path = "/select-profile",
    body = profile,
    config = httr::add_headers(.get_auth_header())
  )

  if (response$status_code == 404 && profile != "default") {
    stop(paste0("Profile not found: [ '", profile, "' ]"))
  }

  .handle_request_error(response)

  invisible(lapply(paths, .install_package))
}

#' Install an R-package
#'
#' @param path a path to one R-package
#'
#' @noRd
.install_package <- function(path) {
  file <- httr::upload_file(path)

  message(paste0("Attempting to install package [ '", path, "' ]"))
  response <- httr::POST(
    url = .get_url(),
    path = "/install-package",
    body = list(file = file),
    config = c(
      httr::content_type("multipart/form-data"),
      httr::add_headers(.get_auth_header())
      )
  )

  if (response$status_code == 404) {
    stop(paste0(
      "Endpoint doesn't exist. Make sure you're running ",
      "Armadillo in development mode."))
  }
  .handle_request_error(response)

  message(paste0("Package [ '", path, "' ] installed"))
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
