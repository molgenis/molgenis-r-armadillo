#' Check if bucket / folder exists
#' 
#' @param bucket_name name of the folder
#'
#' @keywords internal
.check_if_bucket_exists <- function(bucket_name) {
  exists <- suppressMessages(minio.s3::bucket_exists(bucket_name,
    use_https = FALSE
  ))

  if (!exists) {
    stop(paste0(
      "Folder '", .to_readable_name(bucket_name),
      "' doesn't exist"
    ), call. = FALSE)
  }
}

#' Check if workspace exists
#'
#' @param bucket_name folder name
#' @param workspace_name workspace name
#' 
#' @keywords internal
.check_if_workspace_exists <- function(bucket_name, workspace_name) {
  .check_if_bucket_exists(bucket_name)

  exists <- suppressMessages(
    minio.s3::head_object(
      object = .to_file_name(workspace_name),
      bucket = bucket_name,
      use_https = FALSE,
      verbose = FALSE
    )
  )

  if (!exists) {
    stop(paste0("Workspace ", workspace_name, " doesn't exist"),
      call. = FALSE
    )
  }
}
