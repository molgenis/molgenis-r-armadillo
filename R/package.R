#' Install package
#'
#' Installs a user defined package.
#'
#' @param path the path to the package
#' @param profile the selected profile
#' @param url the url of the armadillo service
#'
#' @export
armadillo.install_package <- function(path, profile="default", url) {
  token <- armadillo.get_token(url)
  headers <- httr::add_headers("Authorization" = paste0("Bearer ", token))

  handle <- httr::handle(url)
  
  file <- httr::upload_file(path)
  
  query <- list(
    profile = profile
  )
  
  response <- httr::POST(
    handle = handle,
    query = query,
    path = "/install-package",
    body = file,
    config = c(headers, httr::content_type("multipart"))
  )
  .handle_request_error(response)
  if (response$status == 404) {
    stop(paste0("Profile not found: '", profile, "'"))
  }
  
}