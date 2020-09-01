test_that("armadillo.upload_table checks if table is provided", {
  expect_error(
    armadillo.upload_table(project = "project", folder = "folder"),
    "argument \"table\" is missing, with no default"
  )
})

test_that("armadillo.upload_table checks if folder is provided", {
  expect_error(
    armadillo.upload_table(project = "project", table = datasets::iris),
    "argument \"folder\" is missing, with no default"
  )
})

test_that("armadillo.upload_table checks if the project exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.upload_table(
        project = "project",
        folder = "example",
        table = datasets::iris
      ),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Project 'project' does not exist\\."
  )

  expect_args(bucket_exists, 1, "shared-project", use_https = TRUE)
})

test_that("armadillo.upload_table uploads table", {
  put_object <- mock(TRUE)
  bucket_exists <- mock(TRUE)
  file <- tempfile()
  on.exit(unlink(file))
  # Cannot mock base functions, so stub it instead
  stub(armadillo.upload_table, "tempfile", file)

  expect_message(
    with_mock(
      result <- armadillo.upload_table(
        project = "project",
        folder = "folder",
        table = datasets::iris
      ),
      "aws.s3::put_object" = put_object,
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(FALSE, cycle = TRUE)
    ),
    "Compressing table...|Uploaded table folder/datasets::iris",
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

test_that("armadillo.list_tables checks if the project exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.list_tables("project"),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Project 'project' does not exist\\."
  )

  expect_args(bucket_exists, 1, "shared-project", use_https = TRUE)
})

test_that("armadillo.list_tables lists the tables in a project", {
  bucket_exists <- mock(TRUE)
  get_bucket <- mock(tables)

  with_mock(
    result <- armadillo.list_tables("project"),
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

test_that("armadillo.delete_table checks if the project exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.delete_table("project", "test"),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Project 'project' does not exist\\."
  )

  expect_args(bucket_exists, 1, "shared-project", use_https = TRUE)
})

test_that("armadillo.delete_table checks if the table exists", {
  head_object <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.delete_table("project", "folder", "test"),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = head_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "Table 'folder/test' does not exist\\."
  )

  expect_args(head_object, 1,
    "folder/test.parquet", "shared-project",
    use_https = TRUE
  )
})

test_that("armadillo.delete_workspace deletes a workspace", {
  delete_object <- mock(TRUE)

  expect_message(
    result <- with_mock(
      armadillo.delete_table("project", "folder", "test"),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = mock(TRUE),
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE),
      "aws.s3::delete_object" = delete_object
    ),
    "Deleted table 'folder/test'\\."
  )

  expect_true(result)
})

test_that("armadillo.copy_table checks if the source project exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.copy_table(
        project = "project",
        folder = "folder",
        name = "tim_subset_1",
        new_folder = "gecko_subset_1"
      ),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Project 'project' does not exist\\."
  )

  expect_args(bucket_exists, 1, "shared-project", use_https = TRUE)
})

test_that("armadillo.copy_table checks if the source table exists", {
  head_object <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.copy_table(
        project = "project",
        folder = "folder",
        name = "test",
        new_folder = "gecko_subset_1"
      ),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = head_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "Table 'folder/test' does not exist\\."
  )

  expect_args(head_object, 1,
    "folder/test.parquet", "shared-project",
    use_https = TRUE
  )
})

test_that("armadillo.copy_table checks if the target project exists", {
  bucket_exists <- mock(TRUE, FALSE)

  expect_error(
    with_mock(
      armadillo.copy_table(
        project = "project",
        folder = "folder",
        name = "test",
        new_project = "target"
      ),
      "aws.s3::bucket_exists" = bucket_exists,
      "aws.s3::head_object" = mock(TRUE),
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "Project 'target' does not exist\\."
  )

  expect_args(bucket_exists, 2, "shared-target", use_https = TRUE)
})

test_that("armadillo.copy_table warns if you copy an object onto itself", {
  expect_error(
    armadillo.copy_table(
      project = "project",
      folder = "folder",
      name = "test"
    ),
    "Cannot copy table onto itself\\."
  )
})

test_that("armadillo.copy_table copies table", {
  copy_object <- mock(TRUE)

  expect_message(
    with_mock(
      result <- armadillo.copy_table(
        project = "project",
        folder = "folder",
        name = "test",
        new_folder = "target"
      ),
      "aws.s3::bucket_exists" = mock(TRUE, cycle = TRUE),
      "aws.s3::head_object" = mock(TRUE),
      "aws.s3::copy_object" = copy_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "Copied table 'project/folder/test' to 'project/target/test'\\."
  )

  expect_true(result)
  expect_args(copy_object, 1,
    from_object = "folder/test.parquet",
    to_object = "target/test.parquet",
    from_bucket = "shared-project",
    to_bucket = "shared-project",
    use_https = TRUE
  )
})

test_that("armadillo.load_table checks if the project exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.load_table(
        project = "project",
        folder = "example",
        name = "tim_subset_1"
      ),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Project 'project' does not exist\\."
  )

  expect_args(bucket_exists, 1, "shared-project", use_https = TRUE)
})

test_that("armadillo.load_workspace checks if the table exists", {
  head_object <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.load_table(
        project = "project",
        folder = "folder",
        name = "test"
      ),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = head_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "Table 'folder/test' does not exist\\."
  )

  expect_args(head_object, 1,
    "folder/test.parquet", "shared-project",
    use_https = TRUE
  )
})

test_that("armadillo.load_table loads the table from file", {
  file <- tempfile()
  data <- tibble::tribble(
    ~colA, ~colB,
    "a", 1,
    "b", 2,
    "c", 3
  )
  arrow::write_parquet(data, file)
  on.exit(unlink(file))

  s3getobject <- mock(invisible(file))
  environment <- new.env()

  expect_silent(
    with_mock(
      result <- armadillo.load_table(
        project = "project",
        folder = "folder",
        name = "test",
        env = environment
      ),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = mock(TRUE),
      "aws.s3::get_object" = s3getobject,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    )
  )

  expect_equal(result, invisible(NULL))
  expect_args(s3getobject, 1,
    object = "folder/test.parquet",
    bucket = "shared-project",
    use_https = TRUE
  )
  expect_equal(data, environment$test)
})

test_that("armadillo.move_table calls copy and delete", {
  copy_table <- mock(TRUE)
  delete_table <- mock(TRUE)
  expect_message(
    with_mock(
      armadillo.move_table(
        project = "project",
        folder = "folder",
        name = "test",
        new_project = "target"
      ),
      "MolgenisArmadillo::armadillo.copy_table" = copy_table,
      "MolgenisArmadillo::armadillo.delete_table" = delete_table
    ), "Moved table 'project/folder/test' to 'target/folder/test'\\."
  )

  expect_args(copy_table, 1,
    project = "project",
    folder = "folder",
    name = "test",
    new_project = "target",
    new_folder = "folder",
    new_name = "test"
  )
  expect_args(delete_table, 1,
    project = "project",
    folder = "folder",
    name = "test"
  )
})
