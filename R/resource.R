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
  if (is.null(name)) { # nolint
    name <- deparse(substitute(resource))
  }

  compress_resource <- function(table, file) {
    saveRDS(resource, file = file)
    ".rds"
  }

  .upload_object(project, folder, resource, name, compress_resource)
}

#' List the resources in a project
#'
#' @param project the shared project in which the resources are located
#'
#' @importFrom aws.s3 get_bucket
#'
#' @examples
#' \dontrun{
#' armadillo.list_resources("gecko")
#' }
#'
#' @export
armadillo.list_resources <- function(project) { # nolint
  .list_objects_by_extension(project, ".rds")
}

#' Delete resource
#'
#' @param project project to delete the resource from
#' @param folder folder to delete the resource from
#' @param name resource name
#' @return TRUE if successful, otherwise an object of class aws_error details
#' if not.
#'
#' @importFrom aws.s3 delete_object
#'
#' @examples
#' \dontrun{
#' armadillo.delete_resource(
#'   project = "gecko",
#'   folder = "core_all",
#'   name = "table1"
#' )
#' }
#'
#' @export
armadillo.delete_resource <- function(project, folder, name) { # nolint
  .delete_object(project, folder, name, ".rds")
}

#' Copy resource
#'
#' @param project study or other variable collection
#' @param folder the folder containing the resource
#' @param name specific resource for copy action
#' @param new_project new location of study or other variable collection
#' @param new_folder name of the folder in which to place the copy, defaults to
#' folder
#' @param new_name name of the copy, defaults to name
#'
#' @importFrom aws.s3 copy_object
#'
#' @examples
#' \dontrun{
#' armadillo.copy_resource(
#'   project = "gecko",
#'   folder = "core_all",
#'   name = "table1",
#'   new_project = "gecko",
#'   new_folder = "core_all_v2",
#' )
#' }
#'
#' @export
armadillo.copy_resource <- # nolint
  function(project, folder, name,
           new_project = project,
           new_folder = folder,
           new_name = name) {
    .copy_object(project,
                folder,
                name,
                new_project,
                new_folder,
                new_name,
                ".rds")
  }
