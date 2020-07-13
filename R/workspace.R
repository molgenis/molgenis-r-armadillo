#' Combines tables in an .Rdata file and uploads them to a folder.
#'
#' @param folder foldername to upload to
#' @param name name of the .Rdata file
#' @param datasets vector of tables
#'
#' @export
create_workspace <- function(folder, name, dataset) {
  # TODO check dataset not empty
  # TODO multiple datasets
  .check_workspace_name(name)
  bucket_name <- .to_shared_bucket_name(folder)
  .check_if_bucket_exists(bucket_name)

  minio.s3::s3save(dataset,
    object = .to_file_name(name),
    bucket = bucket_name
  )

  message(paste0("Created workspace '", name, "'"))
}

#' @export
list_workspaces <- function(folder) {
  bucket_name <- .to_shared_bucket_name(folder)
  .check_if_bucket_exists(bucket_name)

  objects <- minio.s3::get_bucket(bucket_name)
  object_names <- lapply(objects, function(obj) obj$Key)
  unlist(object_names, use.names = FALSE)

  # TODO strip .RData from file names?
}

#' @export
delete_workspace <- function(folder, name) {
  bucket_name <- .to_shared_bucket_name(folder)
  .check_if_workspace_exists(bucket_name, name)

  minio.s3::delete_object(
    object = .to_file_name(name),
    bucket = bucket_name
  )
  message(paste0("Deleted workspace '", name, "'"))
}

#' @export
copy_workspace <- function(folder, name, new_folder) {
  bucket_name <- .to_shared_bucket_name(folder)
  new_bucket_name <- .to_shared_bucket_name(new_folder)
  .check_if_workspace_exists(bucket_name, name)
  .check_if_bucket_exists(new_bucket_name)

  minio.s3::copy_object(
    from_object = .to_file_name(name),
    to_object = .to_file_name(name),
    from_bucket = bucket_name,
    to_bucket = new_bucket_name,
    use_https = FALSE,
    verbose = FALSE
  )

  message(paste0(
    "Copied workspace '", name, "' to folder '",
    new_folder, "'"
  ))
}

#' @export
load_workspace <- function(folder, name) {
  bucket_name <- .to_shared_bucket_name(folder)
  .check_if_workspace_exists(bucket_name, name)

  minio.s3::s3load(
    object = .to_file_name(name),
    bucket = bucket_name,
    use_https = FALSE
  )
}

#' @export
move_workspace <- function(folder, name, new_folder) {
  suppressMessages(copy_workspace(folder, name, new_folder))
  suppressMessages(delete_workspace(folder, name))
  message(paste0(
    "Moved workspace '", name, "' to folder '",
    new_folder, "'"
  ))
}
