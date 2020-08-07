#' Create a folder for a variable collection
#'
#' @param folder_name The name of the folder to create. The folder name
#' \itemize{
#'   \item{cannot be empty.}
#'   \item{must be no more than 56 characters.}
#'   \item{cannot end with a \code{-}.}
#'   \item{must consist of lowercase letters and numbers.}
#'   }
#' @return TRUE if successful
#'
#' @importFrom aws.s3 put_bucket
#'
#' @examples
#' \dontrun{
#' armadillo.create_folder("gecko")
#' }
#'
#' @export
armadillo.create_folder <- function(folder_name) { # nolint
  .check_folder_name(folder_name)

  success <- aws.s3::put_bucket(.to_shared_bucket_name(folder_name),
    use_https = .use_https()
  )

  if (success) {
    message(paste0("Created folder '", folder_name, "'"))
  }
  invisible(success)
}

#' Delete folder
#'
#' A folder represents usually a study or collection of variables
#'
#' @param folder_name study or collection of variables name
#'
#' @examples
#' \dontrun{
#' armadillo.delete_folder(folder_name = "gecko")
#' }
#'
#' @export
armadillo.delete_folder <- function(folder_name) { # nolint
  .delete_bucket(.to_shared_bucket_name(folder_name))
}

#' List the user folders
#'
#' @return list of user folders
#'
#' @examples
#' \dontrun{
#' armadillo.list_user_folders()
#' }
#'
#' @export
armadillo.list_user_folders <- function() { # nolint
  .get_buckets("user-")
}

#' List the shared folders
#'
#' @return list of shared folders
#'
#' @examples
#' \dontrun{
#' armadillo.list_folders()
#' }
#'
#' @export
armadillo.list_folders <- function() { # nolint
  .get_buckets("shared-")
}

#' Deletes a user's folder from the Users bucket
#'
#' @param user_name the folder of the user to delete
#'
#' @examples
#' \dontrun{
#' armadillo.delete_user_folder(user_name = "sido")
#' }
#'
#' @export
armadillo.delete_user_folder <- function(user_name) { # nolint
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
    use_https = .use_https()
  )
  message(paste0("Deleted folder '", .to_readable_name(bucket_name), "'"))
}

#' Get buckets
#'
#' @param prefix can be 'shared-' or 'user-'
#'
#' @noRd
.get_buckets <- function(prefix) {
  buckets <- aws.s3::bucketlist(use_https = .use_https())
  bucket_names <- buckets[["Bucket"]]
  filtered_buckets <- bucket_names[startsWith(bucket_names, prefix)]
  sapply(filtered_buckets,
    function(name) gsub(prefix, "", name),
    USE.NAMES = FALSE
  )
}
