#' Add RData extension
#'
#' @param name filename to add extension to
#'
#' @noRd
.to_file_name <- function(name) {
  paste0(name, ".RData")
}

#' To full qualified table name
#'
#' @param name filename to add extension to
#'
#' @noRd
.to_table_name <- function(folder, name) {
  paste0(folder, "/", name, ".parquet")
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

#' Check project name
#'
#' It should match the regex \code{"^[a-z0-9-]{0,55}[a-z0-9]$"}.
#' But we give human readable messages
#'
#' @param name project name
#'
#' @noRd
.check_project_name <- function(name) {
  stopifnot(is.character(name), length(name) == 1)
  if (nchar(name) == 0) {
    stop("Project name cannot be empty.", call. = FALSE)
  }
  if (nchar(name) > 56) {
    stop("Project name has max 56 characters.", call. = FALSE)
  }
  if (grepl("-$", name, perl = TRUE)) {
    stop("Project name cannot end with a '-'.", call. = FALSE)
  }
  if (!grepl("^[a-z0-9-]{0,55}[a-z0-9]$", name, perl = TRUE)) {
    stop("Project name must consist of lowercase letters and numbers.",
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
.check_folder_name <- function(name) {
  stopifnot(is.character(name), length(name) == 1)
  if (nchar(name) == 0) {
    stop("Workspace name cannot be empty.", call. = FALSE)
  }
  if (nchar(name) > 1018) {
    stop("Workspace name cannot be longer than 1018 characters.", call. = FALSE)
  }
  if (!grepl("^[a-zA-Z0-9_:-]+$", name, perl = TRUE)) {
    stop(paste0(
      "Valid workspace name characters are ",
      "ASCII letters, digits, '_', '-' and ':'"
    ),
    call. = FALSE
    )
  }
}
