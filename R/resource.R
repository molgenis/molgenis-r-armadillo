#' Uploads a resource to a folder in a project
#'
#' @param project the project to upload to
#' @param folder the folder to upload to
#' @param table the resource to upload
#' @param name name of the resource (optional)
#'
#' @return TRUE if successful, otherwise an object of class aws_error details
#' if not.
#'
#' @importFrom aws.s3 put_object
#'
#' @examples
#' \dontrun{
#' armadillo.upload_table(
#'   project = "gecko",
#'   folder = "core_all",
#'   table1
#' )
#' }
#'
#' @export
armadillo.upload_resource <- function(project, folder, resource, name = NULL) { # nolint
  compress_resource <- function(table, file) {
    saveRDS(resource, file = file)
    ".rds"
  }

  .upload_object(project, folder, resource, name, compress_resource)
}
