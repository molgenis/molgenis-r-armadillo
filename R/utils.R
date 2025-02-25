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
      content(response, as = "parsed", encoding = "UTF-8")$message,
      call. = FALSE
    )
  } else if (response$status_code == 409) {
    stop(
      content(response, as = "parsed", encoding = "UTF-8")$message,
      call. = FALSE
    )
  } else if (response$status_code == 500) {
    stop(
      paste0(
        "Internal server error: ",
        content(response, as = "parsed", encoding = "UTF-8")$message
      ),
      call. = FALSE
    )
  } else if (response$status_code >= 300) {
    stop(
      paste0(
        "Something went wrong (",
        response$status_code,
        "): ",
        content(response, as = "parsed", encoding = "UTF-8")$message
      ),
      call. = FALSE
    )
  }
}

#' Gets the Armadillo URL from the package's environment. If there is no URL,
#' the user is not logged in and an error is shown.
#'
#' @return the URL
#'
#' @noRd
.get_url <- function() {
  if (!exists("armadillo_url", envir = .pkgglobalenv)) {
    stop("You are not logged in.
         Please log in with armadillo.login('<YOUR_SERVER>')",
      call. = FALSE
    )
  }
  .pkgglobalenv$armadillo_url
}

#' Gets the httr authentication header from the package's environment. If there
#' is no auth header, the user is not logged in and an error is shown.
#'
#' @return the httr authentication header
#'
#' @importFrom base64enc base64encode
#' @noRd
.get_auth_header <- function() {
  if (exists("auth_token", envir = .pkgglobalenv)) {
    c("Authorization" = paste0("Bearer ", .pkgglobalenv$auth_token))
  } else if (exists("auth_username", envir = .pkgglobalenv)) {
    encoded <- base64enc::base64encode(
      charToRaw(
        paste0(
          .pkgglobalenv$auth_username, ":",
          .pkgglobalenv$auth_password
        )
      )
    )
    c("Authorization" = paste0("Basic ", encoded))
  } else {
    stop("You are not logged in.
         Please log in with armadillo.login('<YOUR_SERVER>')",
      call. = FALSE
    )
  }
}

#' split_and_unlist splits the provided string based on the provided separator and returns the unlisted item
#'
#' @param item_to_split string to split
#' @param separator character to split on
#' 
#' @return splitted unlisted item
#'
#' @noRd
.split_and_unlist <- function(item_to_split, separator){
  return(unlist(strsplit(item_to_split, split=separator, fixed = TRUE)))
}
