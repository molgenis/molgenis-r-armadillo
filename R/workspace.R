#' Combines tables in an .Rdata file and uploads them to a folder.
#'
#' @param folder the folder to upload to
#' @param name name of the workspace to create
#' @param ... tables to upload to the dataset
#'
#' @importFrom aws.s3 s3save
#'
#' @examples
#' \dontrun{
#' armadillo.create_workspace(
#'   folder = "gecko",
#'   name = "johan_subset_1",
#'   table1,
#'   table2
#' )
#' }
#'
#' @export
armadillo.create_workspace <- function(folder, name, ...) { # nolint
  if (length(list(...)) == 0) {
    stop("No tables were provided to upload.")
  }
  .check_workspace_name(name)
  bucket_name <- .to_shared_bucket_name(folder)
  .check_if_bucket_exists(bucket_name)

  result <- aws.s3::s3save(...,
    object = .to_file_name(name),
    bucket = bucket_name,
    opts = c(
      use_https = .use_https()
    )
  )

  if (isTRUE(result)) {
    message(paste0("Created workspace '", name, "'."))
  }
  invisible(result)
}

#' List the workspaces
#'
#' @param folder the shared folder in which the workspaces are located
#'
#' @importFrom aws.s3 get_bucket
#'
#' @examples
#' \dontrun{
#' armadillo.list_workspaces("gecko")
#' }
#'
#' @export
armadillo.list_workspaces <- function(folder) { # nolint
  bucket_name <- .to_shared_bucket_name(folder)
  .check_if_bucket_exists(bucket_name)

  objects <- aws.s3::get_bucket(bucket_name,
    use_https = .use_https()
  )
  object_names <- lapply(objects, function(obj) obj$Key)
  unlist(object_names, use.names = FALSE)

  # TODO strip .RData from file names?
}

#' Delete workspace
#'
#' @param folder folder to delete the workspace from
#' @param name workspace name
#' @return TRUE if successful, otherwise an object of class aws_error details
#' if not.
#'
#' @importFrom aws.s3 delete_object
#'
#' @examples
#' \dontrun{
#' armadillo.delete_workspace(
#'   folder = "gecko",
#'   name = "johan_subset_1"
#' )
#' }
#'
#' @export
armadillo.delete_workspace <- function(folder, name) { # nolint
  bucket_name <- .to_shared_bucket_name(folder)
  .check_if_workspace_exists(bucket_name, name)

  result <- aws.s3::delete_object(
    object = .to_file_name(name),
    bucket = bucket_name,
    use_https = .use_https()
  )

  if (isTRUE(result)) {
    message(paste0("Deleted workspace '", name, "'."))
  }
  invisible(result)
}

#' Copy workspace
#'
#' @param folder study or other variable collection
#' @param name specific workspace for copy action
#' @param new_folder new location of study or other variable collection
#' @param new_name name of the copy, defaults to name
#'
#' @importFrom aws.s3 copy_object
#'
#' @examples
#' \dontrun{
#' armadillo.copy_worspace(
#'   folder = "gecko",
#'   name = "tim_subset_1",
#'   new_folder = "gecko_subset_1"
#' )
#' }
#'
#' @export
armadillo.copy_workspace <- # nolint
  function(folder, name, new_folder, new_name = name) {
    if(folder == new_folder && name == new_name) {
      stop("Cannot copy workspace onto itself.", call. = FALSE)
    }
    bucket_name <- .to_shared_bucket_name(folder)
    new_bucket_name <- .to_shared_bucket_name(new_folder)
    .check_if_workspace_exists(bucket_name, name)
    .check_if_bucket_exists(new_bucket_name)

    result <- aws.s3::copy_object(
      from_object = .to_file_name(name),
      to_object = .to_file_name(new_name),
      from_bucket = bucket_name,
      to_bucket = new_bucket_name,
      use_https = .use_https()
    )

    if (isTRUE(result)) {
      message(paste0(
        "Copied workspace '", name, "' to folder '",
        new_folder, "'."
      ))
    }
    invisible(result)
  }

#' Load workspace based upon study folder and tableset
#'
#' @param folder study or collection variables
#' @param name tableset containing the subset
#' @param env The environment in which you want to load the objects in the
#' workspace. Default is the parent.frame() from which the function is called.
#'
#' @importFrom aws.s3 s3load
#'
#' @examples
#' \dontrun{
#' armadillo.load_workspace(
#'   folder = "gecko",
#'   name = "lc_core_1"
#' )
#'
#' armadillo.load_workspace(
#'   folder = "gecko",
#'   name = "lc_core_1",
#'   env = globalenv()
#' )
#' }
#'
#' @export
armadillo.load_workspace <- function(folder, name, env = parent.frame()) { # nolint
  bucket_name <- .to_shared_bucket_name(folder)
  .check_if_workspace_exists(bucket_name, name)

  aws.s3::s3load(
    object = .to_file_name(name),
    bucket = bucket_name,
    use_https = .use_https(),
    envir = env
  )
}

#' Move the workspace
#'
#' @param folder a study or collection of variables
#' @param name a tableset to move
#' @param new_folder a subset of the studies new location
#'
#' @examples
#' \dontrun{
#' armadillo.move_workspace(
#'   folder = "",
#'   name = "",
#'   new_folder = ""
#' )
#' }
#'
#' @export
armadillo.move_workspace <- function(folder, name, new_folder) { # nolint
  suppressMessages(armadillo.copy_workspace(folder, name, new_folder))
  suppressMessages(armadillo.delete_workspace(folder, name))
  message(paste0(
    "Moved workspace '", name, "' to folder '",
    new_folder, "'"
  ))
}
