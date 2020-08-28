#'
#' Check if project exists
#' @param bucket_name name of the folder
#'
#' @noRd
.check_if_bucket_exists <- function(bucket_name) {
  exists <- suppressMessages(aws.s3::bucket_exists(bucket_name,
    use_https = .use_https()
  ))

  if (!exists) {
    stop(paste0(
      "Project '", .to_readable_name(bucket_name),
      "' does not exist."
    ), call. = FALSE)
  }
}

#' Check if workspace exists
#'
#' @param bucket_name folder name
#' @param workspace_name workspace name
#'
#' @noRd
.check_if_table_exists <- function(bucket_name, folder_name, table_name) {
  .check_if_bucket_exists(bucket_name)
  table_name <- .to_table_name(folder_name, table_name)

  exists <- suppressMessages(
    aws.s3::head_object(
      object = table_name,
      bucket = bucket_name,
      use_https = .use_https()
    )
  )

  if (!exists) {
    stop(paste0("Table '", table_name, "' does not exist."),
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
