#' Login
#'
#' Interactively obtains an id token and uses it to create a session token on
#' the minio server.
#'
#' @param armadillo URL of the Armadillo server,
#' @param minio URL of the Armadillo MinIO server
#' @param duration MinIO session duration in seconds
#'
#' @return the id token
#'
#' @examples
#' \dontrun{
#' armadillo.login(
#'   "https://armadillo.dev.molgenis.org",
#'   "https://armadillo-minio.dev.molgenis.org"
#' )
#' armadillo.login("http://localhost:8080", "http://localhost:9000")
#' }
#'
#' @importFrom urltools scheme domain
#' @export
armadillo.login <- function(armadillo, minio, duration = 900) { # nolint
  token <- armadillo.get_token(armadillo)
  options(
    MolgenisArmadillo.auth.token = token,
    MolgenisArmadillo.armadillo.endpoint = armadillo
  )
  armadillo.assume_role_with_web_identity(token, minio, duration)
  invisible(token)
}

#' Retrieve temporary credentials from Armadillo MinIO filestore
#'
#' @param token ID token
#' @param server url of the MinIO server
#' @param duration duration in seconds
#' @param use boolean start using the credentials, default TRUE
#'
#' @return list of credentials, fit for use with \code{\link{set_credentials}}
#'
#' @importFrom httr POST stop_for_status content
#' @importFrom xml2 xml_text xml_find_first xml_ns_strip
#'
#' @keywords internal
#' @export
armadillo.assume_role_with_web_identity <- # nolint
  function(token,
           server,
           duration = 900,
           use = TRUE) { # nolint
    if (duration < 900 | duration > 129600) {
      stop("'duration' must be a value in seconds between 900 and 129600") # nolint
    }
    query <- list(
      Action = "AssumeRoleWithWebIdentity",
      DurationSeconds = duration,
      WebIdentityToken = token,
      Version = "2011-06-15"
    )

    response <- httr::POST(url = server, query = query)
    httr::stop_for_status(response, "assume role on S3 server")
    content <- xml2::xml_ns_strip(httr::content(response, encoding = "UTF-8"))

    armadillo.set_credentials(server,
      xml2::xml_text(xml2::xml_find_first(content, "//AccessKeyId")),
      xml2::xml_text(xml2::xml_find_first(content, "//SecretAccessKey")),
      xml2::xml_text(xml2::xml_find_first(content, "//SessionToken")),
      use = use
    )
  }

#' Set credentials
#'
#' @param server url of the MinIO server
#' @param access_key the MinIO access key
#' @param secret_key the MinIO secret key
#' @param session_token optional MinIO session token, default NULL
#' @param use boolean start using the credentials, default TRUE
#'
#' @return list of credentials, fit for use with \code{\link{set_credentials}}
#'
#' @importFrom aws.iam set_credentials
#'
#' @keywords internal
#' @export
armadillo.set_credentials <- # nolint
  function(server, access_key, secret_key, session_token = NULL, use = TRUE) {
    stopifnot(!is.na(server), !is.na(access_key), !is.na(secret_key))
    credentials <- list(
      AccessKeyId = access_key,
      SecretAccessKey = secret_key,
      SessionToken = session_token
    )

    if (isTRUE(use)) {
      aws.iam::set_credentials(credentials)
      use_https <- urltools::scheme(server) == "https"
      options(
        MolgenisArmadillo.s3.use_https = use_https,
        cloudyr.aws.default_region = ""
      )
      s3_endpoint <- urltools::domain(server)
      port <- urltools::port(server)
      if (!is.na(port)) {
        s3_endpoint <- paste0(s3_endpoint, ":", port)
      }
      Sys.setenv(AWS_S3_ENDPOINT = s3_endpoint)
    }
    invisible(credentials)
  }

#' Get ID Token
#'
#' Get an ID token to log in on an Armadillo server or its MinIO filestore.
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
