#' @export
create_folder <- function(folder_name) {
  .check_folder_name(folder_name)

  minio.s3::put_bucket(.to_shared_bucket_name(folder_name),
    acl = "public-read-write"
  )
}

#' @export
list_folders <- function() {
  .get_shared_buckets()
}

#' @export
delete_folder <- function(folder_name) {
  .delete_bucket(.to_shared_bucket_name(folder_name))
}

#' @export
list_user_folders <- function() {
  .get_user_buckets()
}

#' Deletes a user's folder from the Users bucket
#'
#' @param username the folder to delete
#'
#' @export
delete_user_folder <- function(user_name) {
  .delete_bucket(.to_user_bucket_name(user_name))
}

.delete_bucket <- function(bucket_name) {
  .check_if_bucket_exists(bucket_name)
  minio.s3::delete_bucket(bucket_name,
    verbose = FALSE,
    use_https = FALSE
  )
  message(paste0("Deleted folder '", .to_readable_name(bucket_name), "'"))
}

.get_shared_buckets <- function() {
  .get_buckets("shared-")
}

.get_user_buckets <- function() {
  .get_buckets("user-")
}

.get_buckets <- function(prefix) {
  buckets <- minio.s3::bucketlist()
  bucket_names <- buckets[["Bucket"]]
  shared_buckets <- bucket_names[startsWith(bucket_names, prefix)]
  sapply(shared_buckets,
    function(name) gsub(prefix, "", name),
    USE.NAMES = FALSE
  )
}
