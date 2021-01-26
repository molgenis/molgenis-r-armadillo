test_that(".upload_object checks if the project exists", {
  bucket_exists <- mock(FALSE)
  compress <- mock(".rds")

  expect_error(
    with_mock(
      .upload_object(
        project = "project",
        folder = "example",
        object = datasets::iris,
        name = "test",
        compression_function = compress
      ),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Project 'project' does not exist\\."
  )

  expect_args(bucket_exists, 1, "shared-project", use_https = TRUE)
})

test_that(".upload_object uploads object", {
  put_object <- mock(TRUE)
  bucket_exists <- mock(TRUE)
  file <- tempfile()
  on.exit(unlink(file))
  # Cannot mock base functions, so stub it instead
  stub(.upload_object, "tempfile", file)

  compress_table <- function(table, file) {
    arrow::write_parquet(table, file)
    ".parquet"
  }

  expect_message(
    with_mock(
      result <- .upload_object(
        project = "project",
        folder = "folder",
        object = datasets::iris,
        name = "datasets::iris",
        compression_function = compress_table
      ),
      "aws.s3::put_object" = put_object,
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(FALSE, cycle = TRUE)
    ),
    "Compressing...|Uploaded folder/datasets::iris",
    all = TRUE
  )

  expect_true(result)

  expect_args(put_object, 1,
    file = file,
    object = "folder/datasets::iris.parquet",
    bucket = "shared-project",
    multipart = TRUE,
    show_progress = interactive(),
    use_https = FALSE
  )
})

test_that(".list_objects_by_extension checks if the project exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      .list_objects_by_extension("project", ".rds"),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Project 'project' does not exist\\."
  )

  expect_args(bucket_exists, 1, "shared-project", use_https = TRUE)
})

test_that(".list_objects_by_extension lists the objects in a project", {
  bucket_exists <- mock(TRUE)
  get_bucket <- mock(tables)

  with_mock(
    result <- .list_objects_by_extension("project", ".parquet"),
    "aws.s3::get_bucket" = get_bucket,
    "aws.s3::bucket_exists" = bucket_exists,
    "MolgenisArmadillo:::.use_https" = mock(FALSE, cycle = TRUE)
  )

  expect_equal(result, c("folder/patient", "folder/test"))
  expect_args(get_bucket, 1,
    bucket = "shared-project",
    use_https = FALSE
  )
})

test_that(".delete_object checks if the project exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      .delete_object("project", "test"),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Project 'project' does not exist\\."
  )

  expect_args(bucket_exists, 1, "shared-project", use_https = TRUE)
})

test_that(".delete_object checks if the object exists", {
  head_object <- mock(FALSE)

  expect_error(
    with_mock(
      .delete_object("project", "folder", "test", ".parquet"),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = head_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "'folder/test' does not exist\\."
  )

  expect_args(head_object, 1,
    "folder/test.parquet", "shared-project",
    use_https = TRUE
  )
})

test_that(".delete_object deletes an object", {
  delete_object <- mock(TRUE)

  result <- expect_message(
    with_mock(
      .delete_object("project", "folder", "test", ".parquet"),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = mock(TRUE),
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE),
      "aws.s3::delete_object" = delete_object
    ),
    "Deleted 'folder/test'\\."
  )

  expect_true(result)
})

test_that(".copy_object checks if the source project exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      .copy_object(
        project = "project",
        folder = "folder",
        name = "tim_subset_1",
        new_folder = "gecko_subset_1",
        extension = ".parquet"
      ),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Project 'project' does not exist\\."
  )

  expect_args(bucket_exists, 1, "shared-project", use_https = TRUE)
})

test_that(".copy_object checks if the source object exists", {
  head_object <- mock(FALSE)

  expect_error(
    with_mock(
      .copy_object(
        project = "project",
        folder = "folder",
        name = "test",
        new_folder = "gecko_subset_1",
        extension = ".parquet"
      ),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = head_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "'folder/test' does not exist\\."
  )

  expect_args(head_object, 1,
    "folder/test.parquet", "shared-project",
    use_https = TRUE
  )
})

test_that(".copy_object checks if the target project exists", {
  bucket_exists <- mock(TRUE, FALSE)

  expect_error(
    with_mock(
      .copy_object(
        project = "project",
        folder = "folder",
        name = "test",
        new_project = "target",
        extension = ".parquet"
      ),
      "aws.s3::bucket_exists" = bucket_exists,
      "aws.s3::head_object" = mock(TRUE),
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "Project 'target' does not exist\\."
  )

  expect_args(bucket_exists, 2, "shared-target", use_https = TRUE)
})

test_that(".copy_object warns if you copy an object onto itself", {
  expect_error(
    .copy_object(
      project = "project",
      folder = "folder",
      name = "test",
      extension = ".parquet"
    ),
    "Cannot copy table or resource onto itself\\."
  )
})

test_that(".copy_object copies object", {
  copy_object <- mock(TRUE)

  expect_message(
    with_mock({
        result <- .copy_object(
          project = "project",
          folder = "folder",
          name = "test",
          new_folder = "target",
          extension = ".rds"
        )},
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE),
      "aws.s3::bucket_exists" = mock(TRUE, cycle = TRUE),
      "aws.s3::head_object" = mock(TRUE),
      "aws.s3::copy_object" = copy_object
    ),
    "Copied 'project/folder/test' to 'project/target/test'\\."
  )

  expect_true(result)
  expect_args(copy_object, 1,
    from_object = "folder/test.rds",
    to_object = "target/test.rds",
    from_bucket = "shared-project",
    to_bucket = "shared-project",
    use_https = TRUE
  )
})

test_that(".load_object checks if the project exists", {
  bucket_exists <- mock(FALSE)
  load <- mock()

  expect_error(
    with_mock(
      .load_object(
        project = "project",
        folder = "example",
        name = "tim_subset_1",
        load_function = load,
        extension = ".rds"
      ),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Project 'project' does not exist\\."
  )

  expect_args(bucket_exists, 1, "shared-project", use_https = TRUE)
})

test_that(".load_object checks if the object exists", {
  head_object <- mock(FALSE)
  load <- mock()

  expect_error(
    with_mock(
      .load_object(
        project = "project",
        folder = "folder",
        name = "test",
        load_function = load,
        extension = ".parquet"
      ),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = head_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "'folder/test' does not exist\\."
  )

  expect_args(head_object, 1,
    "folder/test.parquet", "shared-project",
    use_https = TRUE
  )
})

test_that(".load_object loads the object from file", {
  file <- tempfile()
  data <- tibble::tribble(
    ~colA, ~colB,
    "a", 1,
    "b", 2,
    "c", 3
  )
  arrow::write_parquet(data, file)
  on.exit(unlink(file))

  save_object <- mock(invisible(file))

  load_table <- function(infile) {
    arrow::read_parquet(infile)
  }

  expect_silent(
    with_mock({
        result <- .load_object(
          project = "project",
          folder = "folder",
          name = "test",
          load_function = load_table,
          extension = ".parquet"
        )},
      "tempfile" = function() {
        file
      },
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = mock(TRUE),
      "aws.s3::save_object" = save_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    )
  )

  expect_args(save_object, 1,
    object = "folder/test.parquet",
    bucket = "shared-project",
    file = file,
    use_https = TRUE
  )
  expect_equal(data, result)
})

test_that(".move_object calls copy and delete", {
  copy_object <- mock(TRUE)
  delete_object <- mock(TRUE)
  expect_message(
    with_mock(
      .move_object(
        project = "project",
        folder = "folder",
        name = "test",
        new_project = "target",
        new_folder = "folder2",
        new_name = "test",
        extension = ".parquet"
      ),
      "MolgenisArmadillo::.copy_object" = copy_object,
      "MolgenisArmadillo::.delete_object" = delete_object
    ), "Moved 'project/folder/test' to 'target/folder2/test'\\."
  )

  expect_args(copy_object, 1,
    project = "project",
    folder = "folder",
    name = "test",
    new_project = "target",
    new_folder = "folder2",
    new_name = "test",
    extension = ".parquet"
  )
  expect_args(delete_object, 1,
    project = "project",
    folder = "folder",
    name = "test",
    extension = ".parquet"
  )
})
