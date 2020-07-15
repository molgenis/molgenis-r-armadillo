#' Create a folder for a variable collection
#'
#' @param folder_name name of the folder to create variable collection
#'
#' @importFrom aws.s3 put_bucket
#'
#' @examples
#' \dontrun{
#' create_folder(folder_name = "gecko")
#' }
#'
#' @export
create_folder <- function(folder_name) {
  .check_folder_name(folder_name)

  aws.s3::put_bucket(.to_shared_bucket_name(folder_name),
    use_https = getOption("MolgenisArmadillo.s3.use_https")
  )
}

#' List all folders
#'
#' @return list all folders (user and shared)
#'
#' @examples
#' \dontrun{
#' list_folders()
#' }
#'
#' @export
list_folders <- function() {
  .get_shared_buckets()
}

#' Delete folder
#'
#' A folder represents usually a study or collection of variables
#'
#' @param folder_name study or collection of variables name
#'
#' @examples
#' \dontrun{
#' delete_folder(folder_name = "gecko")
#' }
#'
#' @export
delete_folder <- function(folder_name) {
  .delete_bucket(.to_shared_bucket_name(folder_name))
}

#' List the user folders
#'
#' @return list of user folders
#'
#' @examples
#' \dontrun{
#' list_user_folders()
#' }
#'
#' @export
list_user_folders <- function() {
  .get_user_buckets()
}

#' Deletes a user's folder from the Users bucket
#'
#' @param user_name the folder of the user to delete
#'
#' @examples
#' \dontrun{
#' delete_user_folder(user_name = "sido")
#' }
#'
#' @export
delete_user_folder <- function(user_name) {
  .delete_bucket(.to_user_bucket_name(user_name))
}

#' Delete bucket
#'
#' @param bucket_name name of the bucket, usually a collection of variables
#'
#' @noRd
.delete_bucket <- function(bucket_name) {
  .check_if_bucket_exists(bucket_name)
  aws.s3::delete_bucket(bucket_name,
    use_https = getOption("MolgenisArmadillo.s3.use_https")
  )
  message(paste0("Deleted folder '", .to_readable_name(bucket_name), "'"))
}

#' Get shared buckets
#'
#' @return all shared buckets
#'
#' @noRd
.get_shared_buckets <- function() {
  .get_buckets("shared-")
}

#' Get the user buckets
#'
#' @return user buckets
#'
#' @noRd
.get_user_buckets <- function() {
  .get_buckets("user-")
}

#' Get buckets
#'
#' @param prefix can be 'shared-' or 'user-'
#'
#' @noRd
.get_buckets <- function(prefix) {
  buckets <- aws.s3::bucketlist(
    use_https = getOption("MolgenisArmadillo.s3.use_https")
  )
  bucket_names <- buckets[["Bucket"]]
  shared_buckets <- bucket_names[startsWith(bucket_names, prefix)]
  sapply(shared_buckets,
    function(name) gsub(prefix, "", name),
    USE.NAMES = FALSE
  )
}
