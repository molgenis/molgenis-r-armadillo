#' Uploads an object to a folder in a project
#'
#' @param project the project to upload to
#' @param folder the folder to upload to
#' @param object the object to upload
#' @param name name of the object
#' @param compression_function a function that compresses an object to a file
#'
#' @return TRUE if successful, otherwise an object of class aws_error details
#'
#' @importFrom aws.s3 put_object
#'
#' @noRd
.upload_object <- function(project, folder, object, name,
                           compression_function) { # nolint
  stopifnot(!is.na(project), !is.na(folder))
  .check_full_name(folder, name)

  bucket_name <- .to_shared_bucket_name(project)
  .check_if_bucket_exists(bucket_name)

  file <- tempfile()
  on.exit(unlink(file))
  message("Compressing...")
  extension <- compression_function(object, file = file)

  full_name <- paste0(folder, "/", name)
  result <- aws.s3::put_object(
    file = file,
    object = paste0(full_name, extension),
    bucket = bucket_name,
    multipart = TRUE,
    show_progress = interactive(),
    use_https = .use_https()
  )

  if (isTRUE(result)) {
    message(paste0("Uploaded ", full_name))
  }
  invisible(result)
}

#' List the objects in a project based on the file type
#'
#' @param project the shared project in which the objects are located
#' @param extension the extension of the files to list
#'
#' @return the object names, without the extension
#'
#' @importFrom aws.s3 get_bucket
#' @noRd
.list_objects_by_extension <- function(project, extension) { # nolint
  bucket_name <- .to_shared_bucket_name(project)
  .check_if_bucket_exists(bucket_name)
  extension_regex <- paste0(extension, "$")

  objects <- aws.s3::get_bucket(bucket_name,
    use_https = .use_https()
  )
  object_names <- lapply(objects, function(obj) obj$Key)
  object_names <- unlist(object_names, use.names = FALSE)
  object_names <- object_names[grepl(extension_regex, object_names)]
  tools::file_path_sans_ext(object_names)
}


#' Delete object
#'
#' @param project project to delete the object from
#' @param folder folder to delete the object from
#' @param name object name
#' @param extension extension of the object to delete
#'
#' @return TRUE if successful, otherwise an object of class aws_error details
#'
#' @importFrom aws.s3 delete_object
#' @noRd
.delete_object <- function(project, folder, name, extension) { # nolint
  bucket_name <- .to_shared_bucket_name(project)
  .check_if_object_exists(bucket_name, folder, name, extension)

  full_name <- paste0(folder, "/", name)

  result <- aws.s3::delete_object(
    object = paste0(full_name, extension),
    bucket = bucket_name,
    use_https = .use_https()
  )

  if (isTRUE(result)) {
    message(paste0("Deleted '", full_name, "'."))
  }
  invisible(result)
}

#' Copy object
#'
#' @param project study or other variable collection
#' @param folder the folder containing the object
#' @param name specific object for copy action
#' @param new_project new location of study or other variable collection
#' @param new_folder name of the folder in which to place the copy, defaults to
#' folder
#' @param new_name name of the copy, defaults to name
#' @param extension extension of the objects
#'
#' @return the response from the server
#'
#' @importFrom aws.s3 copy_object
#' @noRd
.copy_object <- # nolint
  function(project, folder, name,
           new_project = project,
           new_folder = folder,
           new_name = name,
           extension) {
    if (project == new_project &&
      folder == new_folder &&
      name == new_name) {
      stop("Cannot copy table or resource onto itself.", call. = FALSE)
    }
    bucket_name <- .to_shared_bucket_name(project)
    new_bucket_name <- .to_shared_bucket_name(new_project)
    .check_if_object_exists(bucket_name, folder, name, extension)
    .check_if_bucket_exists(new_bucket_name)
    .check_full_name(new_folder, new_name)

    result <- aws.s3::copy_object(
      from_object = paste0(folder, "/", name, extension),
      to_object = paste0(new_folder, "/", new_name, extension),
      from_bucket = bucket_name,
      to_bucket = new_bucket_name,
      use_https = .use_https()
    )

    message(paste0(
      "Copied '", project, "/", folder, "/", name, "' to '",
      new_project, "/", new_folder, "/", new_name, "'."
    ))

    invisible(result)
  }

#' Move the object
#'
#' @param project a study or collection of variables
#' @param folder the folder containing the object to move
#' @param name name of the object to move
#' @param new_project the project to move the object to
#' @param new_folder the folder to move the object to, defaults to folder
#' @param new_name use to rename the file, defaults to name
#' @param extension extension of the file
#'
#' @return NULL, invisibly
#' @noRd
.move_object <- # nolint
  function(project, folder, name, new_project, new_folder, new_name,
           extension) {
    suppressMessages(.copy_object(
      project, folder, name, new_project,
      new_folder, new_name, extension
    ))
    suppressMessages(.delete_object(project, folder, name, extension))

    message(paste0(
      "Moved '", project, "/", folder, "/", name,
      "' to '", new_project, "/", new_folder, "/", new_name, "'."
    ))
  }

#' Load an object from a project
#'
#' @param project study or collection variables
#' @param folder the folder containing the object
#' @param name name of the object
#' @param load_function a function to extract the object with
#' @param extension the extension of the file
#'
#' @return the result of load_function
#'
#' @importFrom aws.s3 get_object
#' @noRd
.load_object <- function(project, folder, name, load_function, extension) {
  bucket_name <- .to_shared_bucket_name(project)
  .check_if_object_exists(bucket_name, folder, name, extension)

  file <- tempfile()
  on.exit(unlink(file))
  aws.s3::save_object(
    object = paste0(folder, "/", name, extension),
    bucket = bucket_name,
    file = file,
    use_https = .use_https()
  )

  load_function(file)
}
