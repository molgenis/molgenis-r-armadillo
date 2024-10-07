#' Change bucketname to readable bucket name
#'
#' @param bucket_name folder name
#' @return readable name for the folder
#'
#' @noRd
.to_readable_name <- function(bucket_name) {
  name_parts <- .split_and_unlist(bucket_name, "-")
  paste0(name_parts[2:length(name_parts)], collapse = "-")
}

#' Add 'shared-' prefix
#'
#' @param folder_name shared foldername
#' @return shared bucket name for the folder
#'
#' @noRd
.to_shared_bucket_name <- function(folder_name) {
  paste0("shared-", tolower(folder_name))
}

#' Check project name
#'
#' It should match the regex \code{"^[a-z0-9-]{0,55}[a-z0-9]$"}.
#' But we give human readable messages
#'
#' @param name project name
#' @return No return value, throws if name is invalid
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

#' Check a folder, table or resource name
#'
#' Name can not be empty and may not contain a slash
#'
#' @param name folder, table or resource name
#' @return No return value, throws if name is invalid
#'
#' @noRd
.check_name <- function(name) {
  stopifnot(is.character(name), length(name) == 1)
  if (nchar(name) == 0) {
    stop("Folder, table or resource name cannot be empty.", call. = FALSE)
  }

  if (!grepl("^[a-zA-Z0-9_:-]+$", name, perl = TRUE)) {
    stop(
      paste0(
        "Name: ", name, " has invalid characters.\n",
        "Only ASCII letters, digits, '_', '-' and ':' are permitted."
      ),
      call. = FALSE
    )
  }
}


#' Check the object name of a table or resource
#'
#' @param folder folder name
#' @param name table or resource name
#' @return No return value, throws if name is invalid
#'
#' @noRd
.check_full_name <- function(folder, name) {
  stopifnot(
    is.character(name),
    length(name) == 1,
    is.character(folder),
    length(folder) == 1
  )
  .check_name(folder)
  .check_name(name)

  full_name <- paste0(folder, "/", name)

  if (nchar(full_name) > 1024) {
    stop("Folder + table/resource name cannot be longer than 1024 characters.",
      call. = FALSE
    )
  }
}
