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

#' Move the resource
#'
#' @param project a study or collection of variables
#' @param folder the folder containing the resource to move
#' @param name a resource to move
#' @param new_project the project to move the resource to
#' @param new_folder the folder to move the resource to, defaults to folder
#' @param new_name use to rename the file, defaults to name
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
    .move_object(project,
                 folder,
                 name,
                 new_project,
                 new_folder,
                 new_name,
                 ".rds")
  }

#' Load a resource from a project
#'
#' @param project study or collection variables
#' @param folder the folder containing the resource
#' @param name name of the resource
#' @param env The environment in which you want to load the resource.
#' Default is the parent.frame() from which the function is called.
#' @return NULL, invisibly
#'
#' @examples
#' \dontrun{
#' armadillo.load_resource(
#'   project = "gecko",
#'   folder = "core_all",
#'   name = "lc_core_1"
#' )
#'
#' armadillo.load_resource(
#'   project = "gecko",
#'   folder = "core_all",
#'   name = "lc_core_1",
#'   env = globalenv()
#' )
#' }
#'
#' @export
armadillo.load_resource <- function(project, folder, name, # nolint
                                    env = parent.frame()) {
  load_resource <- function(file) {
    readRDS(tools::file_path_as_absolute(file))
  }

  .load_object(project, folder, name, env, load_resource, ".rds")
}
