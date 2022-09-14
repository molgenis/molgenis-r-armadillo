#' Handle generic request errors
#'
#' @param response HTTR response
#'
#' @importFrom httr content
#'
#' @noRd
.handle_request_error <- function(response) {
  if (response$status_code == 401) {
    stop("Unauthorized", call. = FALSE)
  } else if (response$status_code == 403) {
    stop("Forbidden", call. = FALSE)
  } else if (response$status_code == 404) {
    stop(
        httr::content(response, as = "parsed", encoding = "UTF-8")$message,
        call. = FALSE
    )
  } else if (response$status_code == 409) {
    stop(
        httr::content(response, as = "parsed", encoding = "UTF-8")$message,
        call. = FALSE
    )
  } else if (response$status_code == 500) {
    stop(
      paste0(
        "Internal server error: ",
        httr::content(response, as = "parsed", encoding = "UTF-8")$message),
        call. = FALSE)
  }
}

#' Gets the handle from the options. If there is no handle, the user is not
#' logged in and an error is shown.
#'
#' @param handle the authenticated httr handle
#'
#' @noRd
.get_handle <- function() {
  handle <- getOption("MolgenisArmadillo.armadillo.handle")
  if (is.null(handle)) {
    stop("You are not logged in.
         Please log in with armadillo.login('<YOUR_SERVER>')",
         call. = FALSE)
  }
  handle
}
