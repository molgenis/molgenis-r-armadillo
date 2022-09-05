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
#' @importFrom httr POST
#'
#' @noRd
.upload_object <- function(project, folder, object, name,
                           compression_function) { # nolint
  stopifnot(!is.na(project), !is.na(folder))
  .check_full_name(folder, name)
  
  handle <- getOption("MolgenisArmadillo.armadillo.handle")
  full_name <- paste0(folder, "/", name)

  file <- tempfile()
  on.exit(unlink(file))
  message("Compressing...")
  extension <- compression_function(object, file = file)

  response <- httr::POST(
    handle = handle,
    path = paste0("/storage/projects/", project, "/objects"),
    body = list(
      file = httr::upload_file(file, type = "application/json; charset=UTF-8"),
      object = paste0(full_name, extension)
    ),
    headers = httr::add_headers("Content-Type" = "multipart/form-data")
  )
  .handle_request_error(response)
  
  message(paste0("Uploaded ", full_name))
}

#' List the objects in a project based on the file type
#'
#' @param project the shared project in which the objects are located
#' @param extension the extension of the files to list
#'
#' @return the object names, without the extension
#'
#' @importFrom httr GET
#' @noRd
.list_objects_by_extension <- function(project, extension) { # nolint
  extension_regex <- paste0(extension, "$")
  handle <- getOption("MolgenisArmadillo.armadillo.handle")
  
  response <- httr::GET(
    handle = handle,
    path = paste0("/storage/projects/", project, "/objects")
  )
  .handle_request_error(response)
  content <- httr::content(response, as = "parsed")

  objects <- sapply(content, function(object) object[1])
  objects_filtered <- objects[grepl(extension_regex, objects)]
  tools::file_path_sans_ext(objects_filtered)
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
#' @importFrom httr DELETE
#' @noRd
.delete_object <- function(project, folder, name, extension) { # nolint
  handle <- getOption("MolgenisArmadillo.armadillo.handle")

  object_name <- paste0(folder, "/", name)

  response <- httr::DELETE(
    handle = handle,
    path = .to_object_URI(project, object_name, extension),
  )
  .handle_request_error(response)

  message(paste0("Deleted '", object_name, "'"))
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
    .check_full_name(new_folder, new_name)
    
    handle <- getOption("MolgenisArmadillo.armadillo.handle")
    object_name <- paste0(folder, "/", name)
    new_object_name <- paste0(new_folder, "/", new_name)
    
    response <- httr::POST(
      handle = handle,
      path = paste0(.to_object_URI(project, object_name, extension), "/copy"),
      body = list(
        name = paste0(new_object_name, extension)
      ),
      encode = "json"
    )

    .handle_request_error(response)

    message(paste0(
      "Copied '", project, "/", object_name, "' to '",
      new_project, "/", new_object_name, "'."
    ))

    invisible(response)
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
    if (project == new_project &&
        folder == new_folder &&
        name == new_name) {
      stop("Cannot move table or resource onto itself.", call. = FALSE)
    }
    .check_full_name(new_folder, new_name)
    
    handle <- getOption("MolgenisArmadillo.armadillo.handle")
    object_name <- paste0(folder, "/", name)
    new_object_name <- paste0(new_folder, "/", new_name)
    
    response <- httr::POST(
      handle = handle,
      path = paste0(.to_object_URI(project, object_name, extension), "/move"),
      body = list(
        name = paste0(new_object_name, extension)
      ),
      encode = "json"
    )
    
    .handle_request_error(response)

    message(paste0(
      "Moved '", project, "/", object_name,
      "' to '", new_project, "/", new_object_name, "'."
    ))
    
    invisible(response)
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
  handle <- getOption("MolgenisArmadillo.armadillo.handle")

  file <- tempfile()
  on.exit(unlink(file))
  
  response <- httr::GET(
    handle = handle,
    path = .to_object_URI(project, paste0(folder, "/", name), extension)
  )
  
  .handle_request_error(response)
  
  writeBin(content(response, "raw"), file)

  load_function(file)
}

#' Get storage API object URI.
#'
#' @param project project name
#' @param folder the folder containing the object
#' @param name name of the object
#' @param extension the extension of the file
#' 
#' @return a storage API object URI
#'
#' @noRd
.to_object_URI <- function(project, object_name, extension) {
  full_name <- paste0(object_name, extension)
  paste0("/storage/projects/", project, "/objects/", URLencode(full_name, reserved = TRUE))
}
