#' Combines tables in an .Rdata file and uploads them to a folder.
#'
#' @param folder foldername to upload to
#' @param name name of the .Rdata file
#' @param dataset vector of tables
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @importFrom aws.s3 s3save
#'
#' @examples
#' \dontrun{
#' create_workspace(
#'   folder = "gecko",
#'   name = "johan_subset_1",
#'   dataset = "table1and2"
#' )
#' }
#'
#' @export
create_workspace <- function(folder, name, dataset, ...) {
  # TODO check dataset not empty
  # TODO multiple datasets
  .check_workspace_name(name)
  bucket_name <- .to_shared_bucket_name(folder)
  .check_if_bucket_exists(bucket_name, ...)

  aws.s3::s3save(dataset,
    object = .to_file_name(name),
    bucket = bucket_name,
    ...
  )

  message(paste0("Created workspace '", name, "'"))
}

#' List the workspaces
#'
#' @param folder the folder in which the workspaces are located
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @importFrom aws.s3 get_bucket
#'
#' @examples
#' \dontrun{
#' list_workspaces(
#'   folder = "gecko"
#' )
#' }
#'
#' @export
list_workspaces <- function(folder, ...) {
  bucket_name <- .to_shared_bucket_name(folder)
  .check_if_bucket_exists(bucket_name, ...)

  objects <- aws.s3::get_bucket(bucket_name, ...)
  object_names <- lapply(objects, function(obj) obj$Key)
  unlist(object_names, use.names = FALSE)

  # TODO strip .RData from file names?
}

#' Delete workspace
#'
#' @param folder folder to delete the workspace from
#' @param name workspace name
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @importFrom aws.s3 delete_object
#'
#' @examples
#' \dontrun{
#' delete_workspace(
#'   folder = "gecko",
#'   name = "johan_subset_1"
#' )
#' }
#'
#' @export
delete_workspace <- function(folder, name, ...) {
  bucket_name <- .to_shared_bucket_name(folder)
  .check_if_workspace_exists(bucket_name, name, ...)

  aws.s3::delete_object(
    object = .to_file_name(name),
    bucket = bucket_name,
    ...
  )
  message(paste0("Deleted workspace '", name, "'"))
}

#' Copy workspace
#'
#' @param folder study or other variable collection
#' @param name specific workspace for copy action
#' @param new_folder new location of study or other variable collection
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @importFrom aws.s3 copy_object
#'
#' @examples
#' \dontrun{
#' copy_worspace(
#'   folder = "gecko",
#'   name = "tim_subset_1",
#'   new_folder = "gecko_subset_1"
#' )
#' }
#'
#' @export
copy_workspace <- function(folder, name, new_folder, ...) {
  bucket_name <- .to_shared_bucket_name(folder)
  new_bucket_name <- .to_shared_bucket_name(new_folder)
  .check_if_workspace_exists(bucket_name, name, ...)
  .check_if_bucket_exists(new_bucket_name, ...)

  aws.s3::copy_object(
    from_object = .to_file_name(name),
    to_object = .to_file_name(name),
    from_bucket = bucket_name,
    to_bucket = new_bucket_name,
    ...
  )

  message(paste0(
    "Copied workspace '", name, "' to folder '",
    new_folder, "'"
  ))
}

#' Load workspace based upon study folder and tableset
#'
#' @param folder study or collection variables
#' @param name tableset containing the subset
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @importFrom aws.s3 s3load
#'
#' @examples
#' \dontrun{
#' load_workspace(
#'   folder = "gecko",
#'   name = "lc_core_1"
#' )
#' }
#'
#' @export
load_workspace <- function(folder, name, ...) {
  bucket_name <- .to_shared_bucket_name(folder)
  .check_if_workspace_exists(bucket_name, name, ...)

  aws.s3::s3load(
    object = .to_file_name(name),
    bucket = bucket_name,
    ...
  )
}

#' Move the workspace
#'
#' @param folder a study or collection of variables
#' @param name a tableset to move
#' @param new_folder a subset of the studies new location
#' @param ... Additional arguments passed to \code{\link{s3HTTP}}
#'
#' @examples
#' \dontrun{
#' move_workspace(
#'   folder = "",
#'   name = "",
#'   new_folder = ""
#' )
#' }
#'
#' @export
move_workspace <- function(folder, name, new_folder, ...) {
  suppressMessages(copy_workspace(folder, name, new_folder, ...))
  suppressMessages(delete_workspace(folder, name, ...))
  message(paste0(
    "Moved workspace '", name, "' to folder '",
    new_folder, "'"
  ))
}
