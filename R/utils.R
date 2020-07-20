#'
#' Check if bucket / folder exists
#' @param bucket_name name of the folder
#'
#' @noRd
.check_if_bucket_exists <- function(bucket_name) {
  exists <- suppressMessages(aws.s3::bucket_exists(bucket_name,
    use_https = .use_https()
  ))

  if (!exists) {
    stop(paste0(
      "Folder '", .to_readable_name(bucket_name),
      "' doesnot exist."
    ), call. = FALSE)
  }
}

#' Check if workspace exists
#'
#' @param bucket_name folder name
#' @param workspace_name workspace name
#'
#' @noRd
.check_if_workspace_exists <- function(bucket_name, workspace_name) {
  .check_if_bucket_exists(bucket_name)

  exists <- suppressMessages(
    aws.s3::head_object(
      object = .to_file_name(workspace_name),
      bucket = bucket_name,
      use_https = .use_https()
    )
  )

  if (!exists) {
    stop(paste0("Workspace ", workspace_name, " doesnot exist."),
      call. = FALSE
    )
  }
}

#' Check if s3 calls should use HTTPS
#'
#' This value is set when the package loads and updated when you log in.
#'
#' @noRd
.use_https <- function() {
  getOption("MolgenisArmadillo.s3.use_https", TRUE)
}
