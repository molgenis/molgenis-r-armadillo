#' Login
#'
#' Interactively obtains an id token and uses it to create a session token for
#' an Armadillo Service
#'
#' @param armadillo URL of the Armadillo server,
#'
#' @return the id token
#'
#' @examples
#' \dontrun{
#' armadillo.login(
#'   "https://armadillo.dev.molgenis.org"
#' )
#' armadillo.login("http://localhost:8080")
#' }
#'
#' @importFrom urltools scheme domain
#' @export
armadillo.login <- function(armadillo) { # nolint
  # Open browser and authenticate with device code
  token <- armadillo.get_token(armadillo)

  # Configure session
  handle <- httr::handle(armadillo)
  auth_header <-
    httr::add_headers("Authorization" = paste0("Bearer ", token))

  # Do a request to authenticate with token
  httr::GET(
    handle = handle,
    path = "/my/principal",
    config = auth_header
  )

  options(
    MolgenisArmadillo.auth.token = token,
    MolgenisArmadillo.armadillo.endpoint = armadillo,
    MolgenisArmadillo.armadillo.handle = handle
  )

  invisible(token)
}

#' Get ID Token
#'
#' Get an ID token to log in on an Armadillo server.
#'
#' @param server the URL of the Armadillo server
#'
#' @return The ID token string
#'
#' @importFrom MolgenisAuth discover device_flow_auth
#' @importFrom httr GET stop_for_status content
#'
#' @keywords internal
#' @export
armadillo.get_token <- function(server) { # nolint
  auth_info <- .get_info(server)$auth
  endpoint <- MolgenisAuth::discover(auth_info$issuerUri)
  credentials <- MolgenisAuth::device_flow_auth(
    endpoint,
    auth_info$clientId
  )
  return(credentials$id_token)
}

#' Fetch server info
#'
#' Retrieves server info from Armadillo server's info endpoint
#'
#' @return structured list with info items
#'
#' @noRd
.get_info <- function(armadillo_server) {
  info_url <- armadillo_server
  urltools::path(info_url) <- "actuator/info"
  response <- httr::GET(info_url)
  httr::stop_for_status(response, task = "fetch server info")
  return(httr::content(response))
}
