#' Create a folder for a variable collection
#'
#' @param folder_name name of the folder to create variable collection
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @importFrom aws.s3 put_bucket
#'
#' @examples
#' \dontrun{
#' create_folder(
#'   folder_name = "gecko"
#' )
#' }
#'
#' @export
create_folder <- function(folder_name, ...) {
  .check_folder_name(folder_name)

  aws.s3::put_bucket(.to_shared_bucket_name(folder_name), ...)
}

#' List all folders
#'
#' @param ... Arguments passed to \code{\link{s3HTTP}}
#'
#' @return list all folders (user and shared)
#'
#' @examples
#' \dontrun{
#' list_folders()
#' }
#'
#' @export
list_folders <- function(...) {
  .get_shared_buckets(...)
}

#' Delete folder
#'
#' A folder represents usually a study or collection of variables
#'
#' @param folder_name study or collection of variables name
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @examples
#' \dontrun{
#' delete_folder(
#'   folder_name = "gecko"
#' )
#' }
#'
#' @export
delete_folder <- function(folder_name, ...) {
  .delete_bucket(.to_shared_bucket_name(folder_name), ...)
}

#' List the user folders
#'
#' @return list of user folders
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @examples
#' \dontrun{
#' list_user_folders()
#' }
#'
#' @export
list_user_folders <- function(...) {
  .get_user_buckets(...)
}

#' Deletes a user's folder from the Users bucket
#'
#' @param user_name the folder of the user to delete
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @examples
#' \dontrun{
#' delete_user_folder(
#'   user_name = "sido"
#' )
#' }
#'
#' @export
delete_user_folder <- function(user_name, ...) {
  .delete_bucket(.to_user_bucket_name(user_name), ...)
}

#' Delete bucket
#'
#' @param bucket_name name of the bucket, usually a collection of variables
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @keywords internal
.delete_bucket <- function(bucket_name, ...) {
  .check_if_bucket_exists(bucket_name, ...)
  aws.s3::delete_bucket(bucket_name, ...)
  message(paste0("Deleted folder '", .to_readable_name(bucket_name), "'"))
}

#' Get shared buckets
#'
#' @param ... Arguments passed to \code{\link{s3HTTP}}
#'
#' @return all shared buckets
#'
#' @noRd
#' @keywords internal
.get_shared_buckets <- function(...) {
  .get_buckets("shared-", ...)
}

#' Get the user buckets
#'
#' @param ... Arguments passed to \code{\link{s3HTTP}}
#'
#' @return user buckets
#'
#' @noRd
#' @keywords internal
.get_user_buckets <- function(...) {
  .get_buckets("user-", ...)
}

#' Get buckets
#'
#' @param prefix can be 'shared-' or 'user-'
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @noRd
#' @keywords internal
.get_buckets <- function(prefix, ...) {
  buckets <- aws.s3::bucketlist(...)
  bucket_names <- buckets[["Bucket"]]
  shared_buckets <- bucket_names[startsWith(bucket_names, prefix)]
  sapply(shared_buckets,
    function(name) gsub(prefix, "", name),
    USE.NAMES = FALSE
  )
}
