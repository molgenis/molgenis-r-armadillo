#' Install package
#'
#' Installs a user defined package.
#'
#' @param path the path to the package
#' @param profile the selected profile
#' @param url the url of the armadillo service
#'
#' @importFrom jsonlite toJSON
#' @export
armadillo.install_package <- function(path, profile, url) {
  token <- armadillo.get_token(url)
  auth_header <- httr::add_headers("Authorization" = paste0("Bearer ", token))
  auth_header.content_type_json()
  profile_to_select <- if (profile == "") "default" else profile
  
  handle <- httr::handle(url)
  
  body <- toJSON(
    data.frame(path=c(path), profile=c(profile_to_select))
  )
  
  response <- httr::POST(
    handle = handle,
    path = "/install-package",
    body = profile_to_select,
    config = auth_header
  )
  .handle_request_error(response)
  if (response$status == 404 && profile_to_select != "default") {
    stop(paste0("Profile not found: '", profile, "'"))
  }
}