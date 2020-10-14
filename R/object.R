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

.load_object <- function(project, folder, name, env,
                         load_function, extension) { # nolint
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

  assign(
    name,
    load_function(file),
    envir = env
  )
  invisible(NULL)
}
