#' Add RData extension
#'
#' @param name filename to add extension to
#'
#' @noRd
#' @keywords internal
.to_file_name <- function(name) {
  paste0(name, ".RData")
}

#' Change bucketname to readable bucket name
#'
#' @param bucket_name folder name
#'
#' @noRd
#' @keywords internal
.to_readable_name <- function(bucket_name) {
  name_parts <- unlist(strsplit(bucket_name, "-"))
  paste0(name_parts[2:length(name_parts)], collapse = "-")
}

#' Add 'shared-' prefix
#'
#' @param folder_name shared foldername
#'
#' @noRd
#' @keywords internal
.to_shared_bucket_name <- function(folder_name) {
  paste0("shared-", tolower(folder_name))
}

#' Add 'user-' prefix
#'
#' @param folder_name user foldername
#'
#' @noRd
#' @keywords internal
.to_user_bucket_name <- function(user_name) {
  paste0("user-", tolower(user_name))
}

#' Foldername check
#'
#' For empty and can not contain underscores and dashes
#'
#' @param name foldername
#'
#' @noRd
#' @keywords internal
.check_folder_name <- function(name) {
  if (rlang::is_empty(name)) {
    stop("Folder name can't be empty", call. = FALSE)
  }
  if (grepl("[/_-]", name)) {
    stop("Folder name can't contain any of [/_-]", call. = FALSE)
  }
}

#' Check the workspace name
#'
#' Name can not be empty and may not contain a slash
#'
#' @param name workspace name
#'
#' @noRd
#' @keywords internal
.check_workspace_name <- function(name) {
  if (rlang::is_empty(name)) {
    stop("Workspace name can't be empty", call. = FALSE)
  }
  if (grepl("/", name)) {
    stop("Workspace name can't contain a '/'", call. = FALSE)
  }
}
