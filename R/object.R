.upload_object <- function(project, folder, object, name = NULL,
                           compression_function) { # nolint
  stopifnot(!is.na(project), !is.na(folder))
  if (is.null(name)) { # nolint
    name <- deparse(substitute(object))
  }
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
