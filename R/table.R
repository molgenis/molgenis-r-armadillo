#' Uploads a table to a folder in a project
#'
#' @param project the project to upload to
#' @param folder the folder to upload to
#' @param table the table to upload
#' @param name name of the table (optional)
#'
#' @return TRUE if successful, otherwise an object of class aws_error details
#' if not.
#'
#' @importFrom aws.s3 put_object
#' @importFrom arrow write_parquet
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
armadillo.upload_table <- function(project, folder, table, name = NULL) { # nolint
  stopifnot(!is.na(project), !is.na(folder), !is.na(table))
  if (is.null(name)) { # nolint
    name <- deparse(substitute(table))
  }
  .check_full_table_name(folder, name)

  bucket_name <- .to_shared_bucket_name(project)
  .check_if_bucket_exists(bucket_name)

  file <- tempfile()
  on.exit(unlink(file))
  message("Compressing table...")
  arrow::write_parquet(table, file)

  full_name <- paste0(folder, "/", name)
  result <- aws.s3::put_object(
    file = file,
    object = paste0(full_name, ".parquet"),
    bucket = bucket_name,
    multipart = TRUE,
    show_progress = interactive(),
    use_https = .use_https()
  )

  if (isTRUE(result)) {
    message(paste0("Uploaded table ", full_name))
  }
  invisible(result)
}

#' List the tables in a project
#'
#' @param project the shared project in which the tables are located
#'
#' @importFrom aws.s3 get_bucket
#'
#' @examples
#' \dontrun{
#' armadillo.list_tables("gecko")
#' }
#'
#' @export
armadillo.list_tables <- function(project) { # nolint
  bucket_name <- .to_shared_bucket_name(project)
  .check_if_bucket_exists(bucket_name)

  objects <- aws.s3::get_bucket(bucket_name,
    use_https = .use_https()
  )
  object_names <- lapply(objects, function(obj) obj$Key)
  project <- unlist(object_names, use.names = FALSE)
  tools::file_path_sans_ext(project)
}

#' Delete table
#'
#' @param project project to delete the table from
#' @param folder folder to delete the table from
#' @param name table name
#' @return TRUE if successful, otherwise an object of class aws_error details
#' if not.
#'
#' @importFrom aws.s3 delete_object
#'
#' @examples
#' \dontrun{
#' armadillo.delete_table(
#'   project = "gecko",
#'   folder = "core_all",
#'   name = "table1"
#' )
#' }
#'
#' @export
armadillo.delete_table <- function(project, folder, name) { # nolint
  bucket_name <- .to_shared_bucket_name(project)
  .check_if_table_exists(bucket_name, folder, name)

  full_name <- paste0(folder, "/", name)

  result <- aws.s3::delete_object(
    object = paste0(full_name, ".parquet"),
    bucket = bucket_name,
    use_https = .use_https()
  )

  if (isTRUE(result)) {
    message(paste0("Deleted table '", full_name, "'."))
  }
  invisible(result)
}

#' Copy table
#'
#' @param project study or other variable collection
#' @param folder the folder containing the table
#' @param name specific table for copy action
#' @param new_project new location of study or other variable collection
#' @param new_folder name of the folder in which to place the copy, defaults to
#' folder
#' @param new_name name of the copy, defaults to name
#'
#' @importFrom aws.s3 copy_object
#'
#' @examples
#' \dontrun{
#' armadillo.copy_worspace(
#'   project = "gecko",
#'   folder = "core_all",
#'   name = "table1",
#'   new_project = "gecko",
#'   new_folder = "core_all_v2",
#' )
#' }
#'
#' @export
armadillo.copy_table <- # nolint
  function(project, folder, name,
           new_project = project,
           new_folder = folder,
           new_name = name) {
    if (project == new_project &&
      folder == new_folder &&
      name == new_name) {
      stop("Cannot copy table onto itself.", call. = FALSE)
    }
    bucket_name <- .to_shared_bucket_name(project)
    new_bucket_name <- .to_shared_bucket_name(new_project)
    .check_if_table_exists(bucket_name, folder, name)
    .check_if_bucket_exists(new_bucket_name)
    .check_full_table_name(new_folder, new_name)

    result <- aws.s3::copy_object(
      from_object = .to_table_name(folder, name),
      to_object = .to_table_name(new_folder, new_name),
      from_bucket = bucket_name,
      to_bucket = new_bucket_name,
      use_https = .use_https()
    )

    message(paste0(
      "Copied table '", project, "/", folder, "/", name, "' to '",
      new_project, "/", new_folder, "/", new_name, "'."
    ))

    invisible(result)
  }

#' Load a table from a project
#'
#' @param project study or collection variables
#' @param folder the folder containing the table
#' @param name name of the table
#' @param env The environment in which you want to load the table.
#' Default is the parent.frame() from which the function is called.
#' @return NULL, invisibly
#'
#' @importFrom aws.s3 get_object
#' @importFrom arrow read_parquet
#'
#' @examples
#' \dontrun{
#' armadillo.load_table(
#'   project = "gecko",
#'   folder = "core_all",
#'   name = "lc_core_1"
#' )
#'
#' armadillo.load_table(
#'   project = "gecko",
#'   folder = "core_all",
#'   name = "lc_core_1",
#'   env = globalenv()
#' )
#' }
#'
#' @export
armadillo.load_table <- function(project, folder, name, env = parent.frame()) { # nolint
  bucket_name <- .to_shared_bucket_name(project)
  .check_if_table_exists(bucket_name, folder, name)

  file <- aws.s3::get_object(
    object = .to_table_name(folder, name),
    bucket = bucket_name,
    use_https = .use_https()
  )
  on.exit(unlink(file))

  assign(
    name,
    arrow::read_parquet(file),
    envir = env
  )
  invisible(NULL)
}

#' Move the table
#'
#' @param project a study or collection of variables
#' @param folder the folder containing the table to move
#' @param name a table to move
#' @param new_project the project to move the table to
#' @param new_folder the folder to move the table to, defaults to folder
#' @param new_name use to rename the file, defaults to name
#'
#' @examples
#' \dontrun{
#' armadillo.move_folder(
#'   project = "gecko",
#'   folder = "core_all",
#'   name = "table1",
#'   new_project = "gecko",
#'   new_folder = "core_all_v2",
#' )
#' }
#'
#' @export
armadillo.move_table <- # nolint
  function(project, folder, name,
           new_project = project, new_folder = folder, new_name = name) {
    suppressMessages(armadillo.copy_table(
      project, folder, name, new_project,
      new_folder, new_name
    ))
    suppressMessages(armadillo.delete_table(project, folder, name))

    message(paste0(
      "Moved table '", project, "/", folder, "/", name,
      "' to '", new_project, "/", new_folder, "/", new_name, "'."
    ))
  }
