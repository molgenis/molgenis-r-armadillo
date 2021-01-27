#' Uploads a resource to a folder in a project
#'
#' @param project the project to upload to
#' @param folder the folder to upload to
#' @param resource the resource to upload
#' @param name name of the resource (optional)
#'
#' @return TRUE if successful, otherwise an object of class aws_error details
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
  resource # checks for missing argument

  if (is.null(name)) { # nolint
    name <- deparse(substitute(resource))
  }

  .upload_object(project, folder, resource, name, .compress_resource)
}

#' Helper function for compressing to an RDS file
#'
#' @param resource the resource to write to file
#' @param file the name of the file (without extension)
#'
#' @return the extension of the file
#'
.compress_resource <- function(resource, file) {
  saveRDS(resource, file = file)
  ".rds"
}

#' List the resources in a project
#'
#' @param project the shared project in which the resources are located
#'
#' @return the resources in the project
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
#'
#' @return TRUE if successful, otherwise an object of class aws_error details
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
#' @return the response from the server
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
    .copy_object(
      project,
      folder,
      name,
      new_project,
      new_folder,
      new_name,
      ".rds"
    )
  }

#' Move the resource
#'
#' @param project a study or collection of variables
#' @param folder the folder containing the resource to move
#' @param name a resource to move
#' @param new_project the project to move the resource to
#' @param new_folder the folder to move the resource to, defaults to folder
#' @param new_name use to rename the file, defaults to name
#'
#' @return NULL, invisibly
#'
#' @examples
#' \dontrun{
#' armadillo.move_resource(
#'   project = "gecko",
#'   folder = "core_all",
#'   name = "table1",
#'   new_project = "gecko",
#'   new_folder = "core_all_v2",
#' )
#' }
#'
#' @export
armadillo.move_resource <- # nolint
  function(project, folder, name,
           new_project = project, new_folder = folder, new_name = name) {
    .move_object(
      project,
      folder,
      name,
      new_project,
      new_folder,
      new_name,
      ".rds"
    )
  }

#' Load a resource from a project
#'
#' @param project study or collection variables
#' @param folder the folder containing the resource
#' @param name name of the resource
#'
#' @return the loaded resource
#'
#' @examples
#' \dontrun{
#' armadillo.load_resource(
#'   project = "gecko",
#'   folder = "core_all",
#'   name = "lc_core_1"
#' )
#' }
#'
#' @export
armadillo.load_resource <- function(project, folder, name) { # nolint
  .load_object(project, folder, name, .load_resource, ".rds")
}

#' Helper function to extract an RDS file
#'
#' @param file file to extract
#'
#' @return the contents of the file
#'
.load_resource <- function(file) {
  readRDS(tools::file_path_as_absolute(file))
}
