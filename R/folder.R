#' Create a folder for a variable collection
#' 
#' @param folder_name name of the folder to create variable collection
#' 
#' @importFrom minio.s3 put_bucket
#' 
#' @examples 
#' \dontrun{
#' create_folder(
#'   folder_name = "gecko"
#' )
#' }
#' 
#' @export
create_folder <- function(folder_name) {
  .check_folder_name(folder_name)

  minio.s3::put_bucket(.to_shared_bucket_name(folder_name),
    acl = "public-read-write"
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
#' delete_folder(
#'   folder_name = "gecko"
#' )
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
#' delete_user_folder(
#'   user_name = "sido"
#' )
#' }
#'
#' @export
delete_user_folder <- function(user_name) {
  .delete_bucket(.to_user_bucket_name(user_name))
}

#' Delete bucket
#'
#' @param bucket_name specify name of the bucket usually a collection of variables
#' 
#' @keywords internal
.delete_bucket <- function(bucket_name) {
  .check_if_bucket_exists(bucket_name)
  minio.s3::delete_bucket(bucket_name,
    verbose = FALSE,
    use_https = FALSE
  )
  message(paste0("Deleted folder '", .to_readable_name(bucket_name), "'"))
}

#' Get shared buckets
#' 
#' @return all shared buckets
#'
#' @keywords internal
.get_shared_buckets <- function() {
  .get_buckets("shared-")
}

#' Get the user buckets
#' 
#' @return user buckets
#' 
#' @keywords internal
.get_user_buckets <- function() {
  .get_buckets("user-")
}

#' Get buckets
#' 
#' @param prefix can be 'shared-' or 'user-'
#' 
#' @keywords internal
.get_buckets <- function(prefix) {
  buckets <- minio.s3::bucketlist()
  bucket_names <- buckets[["Bucket"]]
  shared_buckets <- bucket_names[startsWith(bucket_names, prefix)]
  sapply(shared_buckets,
    function(name) gsub(prefix, "", name),
    USE.NAMES = FALSE
  )
}
