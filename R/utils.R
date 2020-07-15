#'
#' Check if bucket / folder exists
#' @param bucket_name name of the folder
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @noRd
#' @keywords internal
.check_if_bucket_exists <- function(bucket_name, ...) {
  exists <- suppressMessages(aws.s3::bucket_exists(bucket_name, ...))

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
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @noRd
#' @keywords internal
.check_if_workspace_exists <- function(bucket_name, workspace_name, ...) {
  .check_if_bucket_exists(bucket_name, ...)

  exists <- suppressMessages(
    aws.s3::head_object(
      object = .to_file_name(workspace_name),
      bucket = bucket_name,
      ...
    )
  )

  if (!exists) {
    stop(paste0("Workspace ", workspace_name, " doesn't exist"),
      call. = FALSE
    )
  }
}
