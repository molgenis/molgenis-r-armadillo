#' Check if s3 calls should use HTTPS
#'
#' @return the value of the "MolgenisArmadillo.s3.use_https" option, default
#' TRUE
#'
#' @noRd
.use_https <- function() {
  getOption("MolgenisArmadillo.s3.use_https", TRUE)
}

#' Handle generic request errors
#'
#' @param response HTTR response
#'
#' @importFrom httr content
#'
#' @noRd
.handle_request_error <- function(response) {
  if (response$status_code == 401 || response$status_code == 403) {
    stop("Unauthorized", call. = FALSE)
  } else if (response$status_code == 404) {
    stop(
      paste0(
        "Not found: ",
        httr::content(response, as = "parsed", encoding = "UTF-8")$message),
      call. = FALSE)
  } else if (response$status_code == 500) {
    stop(
      paste0(
        "Internal server error: ",
        httr::content(response, as = "text", encoding = "UTF-8")),
        call. = FALSE)
  }
}
