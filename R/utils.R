#'
#' Check if project exists
#' @param bucket_name name of the folder
#'
#' @return No return value, throws if the bucket does not exist
#'
#' @importFrom aws.s3 bucket_exists
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

#' Check if object exists
#'
#' @param bucket_name bucket name
#' @param folder_name the name of the folder the table is in
#' @param object_name the name of the table or resource
#' @param extension the extension of the object
#'
#' @return No return value, throws if the object does not exist
#'
#' @noRd
.check_if_object_exists <- function(bucket_name, folder_name, object_name,
                                    extension) {
  .check_if_bucket_exists(bucket_name)
  full_name <- paste0(folder_name, "/", object_name, extension)

  exists <- suppressMessages(
    aws.s3::head_object(
      object = full_name,
      bucket = bucket_name,
      use_https = .use_https()
    )
  )

  if (!exists) {
    stop(paste0(
      "'",
      tools::file_path_sans_ext(full_name),
      "' does not exist."
    ),
    call. = FALSE
    )
  }
}

#' Check if s3 calls should use HTTPS
#'
#' @return the value of the "MolgenisArmadillo.s3.use_https" option, default
#' TRUE
#'
#' @noRd
.use_https <- function() {
  getOption("MolgenisArmadillo.s3.use_https", TRUE)
}
