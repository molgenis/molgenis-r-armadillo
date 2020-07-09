.to_file_name <- function(name) {
  paste0(name, ".RData")
}

.to_readable_name <- function(bucket_name) {
  name_parts <- unlist(strsplit(bucket_name, "-"))
  paste0(name_parts[2:length(name_parts)], collapse = "-")
}

.to_shared_bucket_name <- function(folder_name) {
  paste0("shared-", tolower(folder_name))
}

.to_user_bucket_name <- function(user_name) {
  paste0("user-", tolower(user_name))
}

.check_folder_name <- function(name) {
  if (rlang::is_empty(name)) {
    stop("Folder name can't be empty", call. = FALSE)
  }
  if (grepl("[/_-]", name)) {
    stop("Folder name can't contain any of [/_-]", call. = FALSE)
  }
}

.check_workspace_name <- function(name) {
  if (rlang::is_empty(name)) {
    stop("Workspace name can't be empty", call. = FALSE)
  }
  if (grepl("/", name)) {
    stop("Workspace name can't contain a '/'", call. = FALSE)
  }
}
