#' Create a project for a variable collection
#'
#' @param project_name The name of the project to create. The project name
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
#' armadillo.create_project("gecko")
#' }
#'
#' @export
armadillo.create_project <- function(project_name) { # nolint
  .check_project_name(project_name)

  success <- aws.s3::put_bucket(.to_shared_bucket_name(project_name),
    use_https = .use_https()
  )

  if (success) {
    message(paste0("Created project '", project_name, "'"))
  }
  invisible(success)
}

#' Delete project
#'
#' A project represents usually a study or collection of variables
#'
#' @param project_name the name of the study or collection of variables name
#' @return NULL
#'
#' @examples
#' \dontrun{
#' armadillo.delete_project(project_name = "gecko")
#' }
#'
#' @export
armadillo.delete_project <- function(project_name) { # nolint
  .delete_bucket(.to_shared_bucket_name(project_name))
}

#' List the projects
#'
#' @return the projects
#'
#' @examples
#' \dontrun{
#' armadillo.list_projects()
#' }
#'
#' @export
armadillo.list_projects <- function() { # nolint
  .get_buckets("shared-")
}

#' Delete bucket
#'
#' @param bucket_name name of the bucket, usually a collection of variables
#'
#' @return NULL
#'
#' @noRd
.delete_bucket <- function(bucket_name) {
  .check_if_bucket_exists(bucket_name)
  aws.s3::delete_bucket(bucket_name,
    use_https = .use_https()
  )
  message(paste0("Deleted project '", .to_readable_name(bucket_name), "'"))
}

#' Get buckets
#'
#' @param prefix can be 'shared-' or 'user-'
#'
#' @return bucket names, prefix deleted
#'
#' @noRd
.get_buckets <- function(prefix) {
  buckets <- aws.s3::bucketlist(use_https = .use_https())
  bucket_names <- buckets[["Bucket"]]
  if (length(bucket_names) == 0) {
    NULL
  } else {
    filtered_buckets <- bucket_names[startsWith(bucket_names, prefix)]
    sapply(filtered_buckets,
      function(name) gsub(prefix, "", name),
      USE.NAMES = FALSE
    )
  }
}
