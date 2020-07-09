#' Login to the Armadillo MinIO filestore
#'
#' @param server url of the MinIO server
#' @param token oAuth ID token
#' @param duration duration in seconds
#'
#' @importFrom httr POST stop_for_status content oauth_endpoint
#' @importFrom xml2 xml_ns_strip xml_text xml_find_first
#'
#' @export
login <- function(server, token, duration = 3600) {
  response <- httr::POST(server,
    query = list(
      Action = "AssumeRoleWithWebIdentity",
      DurationSeconds = duration,
      WebIdentityToken = token,
      Version = "2011-06-15"
    )
  )
  httr::stop_for_status(response, "assume role on MinIO")
  content <- xml2::xml_ns_strip(httr::content(response))

  # TODO: Shouldn't these be in a session object of sorts?
  return(list(
    key = xml2::xml_text(xml2::xml_find_first(content, "//AccessKeyId")),
    secret = xml2::xml_text(xml2::xml_find_first(content, "//SecretAccessKey")),
    session_token =
      xml2::xml_text(xml2::xml_find_first(content, "//SessionToken"))
  ))
}

#' Get ID Token
#'
#' Get an ID token to log in on an Armadillo server or its MinIO filestore.
#'
#' @param server the URL of the Armadillo server
#'
#' @return The ID token string
#'
#' @importFrom urltools path
#' @importFrom utils browseURL
#' @importFrom httr GET RETRY POST stop_for_status content
#'
#' @export
get_token <- function(server) {
  auth_info <- .get_info(server)$auth
  endpoint <- .discover_endpoint(auth_info$issuerUri)
  credentials <- .device_flow_token(
    endpoint,
    auth_info$clientId
  )
  return(credentials$id_token)
}

.get_info <- function(armadillo_server) {
  info_url <- armadillo_server
  urltools::path(info_url) <- "actuator/info"
  response <- httr::GET(info_url)
  httr::stop_for_status(response, task = "fetch server info")
  return(httr::content(response))
}

.discover_endpoint <- function(auth_server) {
  openid_config_url <- auth_server
  urltools::path(openid_config_url) <- ".well-known/openid-configuration"
  response <- httr::GET(openid_config_url)
  httr::stop_for_status(response, task = "discover OpenID configuration")
  configuration <- httr::content(response)

  return(httr::oauth_endpoint(
    request = NULL,
    authorize = configuration$authorization_endpoint,
    access = configuration$token_endpoint,
    user = configuration$userinfo_endpoint,
    device = configuration$device_authorization_endpoint,
    logout = configuration$end_session_endpoint
  ))
}

.device_flow_token <-
  function(endpoint, client_id) {
    stopifnot(
      inherits(endpoint, "oauth_endpoint"),
      is.character(client_id),
      is.character(endpoint$device)
    )
    response <- httr::POST(endpoint$device,
      body = list(
        client_id = client_id,
        scope = "openid offline_access"
      )
    )
    httr::stop_for_status(response,
                          task = "initiate OpenID Device Flow authentication")
    auth_res <- httr::content(response)
    print(paste0(
      "We're opening a browser so you can log in with code ",
      auth_res$user_code
    ))
    verification_url <- auth_res$verification_uri_complete
    verification_url <- urltools::param_set(
      verification_url,
      "client_id", client_id
    )
    utils::browseURL(verification_url)

    response <- httr::RETRY(
      url = endpoint$access,
      verb = "POST",
      pause_base = auth_res$interval,
      pause_cap = auth_res$interval,
      pause_min = auth_res$interval,
      times = auth_res$expires_in / auth_res$interval,
      quiet = FALSE,
      body = list(
        "client_id" = client_id,
        "grant_type" = "urn:ietf:params:oauth:grant-type:device_code",
        "device_code" = auth_res$device_code
      )
    )
    httr::stop_for_status(response, task = "retrieve id token")
    return(httr::content(response))
  }
