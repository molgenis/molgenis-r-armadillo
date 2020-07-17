#' Add RData extension
#'
#' @param name filename to add extension to
#'
#' @noRd
.to_file_name <- function(name) {
  paste0(name, ".RData")
}

#' Change bucketname to readable bucket name
#'
#' @param bucket_name folder name
#'
#' @noRd
.to_readable_name <- function(bucket_name) {
  name_parts <- unlist(strsplit(bucket_name, "-"))
  paste0(name_parts[2:length(name_parts)], collapse = "-")
}

#' Add 'shared-' prefix
#'
#' @param folder_name shared foldername
#'
#' @noRd
.to_shared_bucket_name <- function(folder_name) {
  paste0("shared-", tolower(folder_name))
}

#' Add 'user-' prefix
#'
#' @param folder_name user foldername
#'
#' @noRd
.to_user_bucket_name <- function(user_name) {
  paste0("user-", tolower(user_name))
}

#' Check folder name
#'
#' It should match the regex \code{"^[a-z0-9-]{0,55}[a-z0-9]$"}.
#' But we give human readable messages
#'
#' @param name foldername
#'
#' @noRd
.check_folder_name <- function(name) {
  stopifnot(is.character(name), length(name) == 1)
  if (nchar(name) == 0) {
    stop("Folder name cannot be empty", call. = FALSE)
  }
  if (nchar(name) > 56) {
    stop("Folder name has max 56 characters", call. = FALSE)
  }
  if (grepl("-$", name)) {
    stop("Folder name cannot end with a '-'", call. = FALSE)
  }
  if (!grepl("^[a-z0-9-]{0,55}[a-z0-9]$", name)) {
    stop("Folder name must consist of lowercase letters and numbers",
      call. = FALSE
    )
  }
}

#' Check the workspace name
#'
#' Name can not be empty and may not contain a slash
#'
#' @param name workspace name
#'
#' @noRd
.check_workspace_name <- function(name) {
  if (rlang::is_empty(name)) {
    stop("Workspace name can't be empty", call. = FALSE)
  }
  if (grepl("/", name)) {
    stop("Workspace name can't contain a '/'", call. = FALSE)
  }
}
