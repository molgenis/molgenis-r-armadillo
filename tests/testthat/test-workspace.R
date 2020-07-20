test_that("armadillo.create_workspace checks if datasets are provided", {
  expect_error(
    armadillo.create_workspace("example", "data"),
    "No tables were provided to upload\\."
  )
})

test_that("armadillo.create_workspace checks if the shared bucket exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.create_workspace(
        folder = "example",
        name = "data",
        d <- 5
      ),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Folder 'example' doesnot exist\\."
  )

  expect_args(bucket_exists, 1, "shared-example", use_https = TRUE)
})

test_that("armadillo.create_workspace creates a workspace", {
  s3save <- mock(TRUE)
  bucket_exists <- mock(TRUE)
  d <- 5

  expect_message(
    with_mock(
      result <- armadillo.create_workspace(
        folder = "example",
        name = "data",
        d
      ),
      "aws.s3::s3save" = s3save,
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(FALSE, cycle = TRUE)
    ),
    "Created workspace 'data'\\."
  )

  expect_true(result)
  expect_args(s3save, 1,
    d,
    object = "data.RData",
    bucket = "shared-example",
    opts = c(use_https = FALSE)
  )
})

test_that("armadillo.list_workspaces checks if the shared folder exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.list_workspaces("example"),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Folder 'example' doesnot exist\\."
  )

  expect_args(bucket_exists, 1, "shared-example", use_https = TRUE)
})

test_that("armadillo.list_workspaces lists the workspaces in a folder", {
  bucket_exists <- mock(TRUE)
  get_bucket <- mock(workspaces)

  with_mock(
    result <- armadillo.list_workspaces("example"),
    "aws.s3::get_bucket" = get_bucket,
    "aws.s3::bucket_exists" = bucket_exists,
    "MolgenisArmadillo:::.use_https" = mock(FALSE, cycle = TRUE)
  )

  expect_equal(result, c("patient.RData", "test.RData"))
  expect_args(get_bucket, 1,
    bucket = "shared-example",
    use_https = FALSE
  )
})

test_that("armadillo.delete_workspace checks if the shared folder exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.delete_workspace("example"),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Folder 'example' doesnot exist\\."
  )

  expect_args(bucket_exists, 1, "shared-example", use_https = TRUE)
})

test_that("armadillo.delete_workspace checks if the workspace exists", {
  head_object <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.delete_workspace("example", "test"),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = head_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "Workspace 'test' doesnot exist\\."
  )

  expect_args(head_object, 1, "test.RData", "shared-example", use_https = TRUE)
})

test_that("armadillo.delete_workspace deletes a workspace", {
  delete_object <- mock(TRUE)

  expect_message(
    result <- with_mock(
      armadillo.delete_workspace("example", "test"),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = mock(TRUE),
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE),
      "aws.s3::delete_object" = delete_object
    ),
    "Deleted workspace 'test'\\."
  )

  expect_true(result)
})

test_that("armadillo.delete_workspace checks if the source folder exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.copy_workspace(
        folder = "example",
        name = "tim_subset_1",
        new_folder = "gecko_subset_1"
      ),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Folder 'example' doesnot exist\\."
  )

  expect_args(bucket_exists, 1, "shared-example", use_https = TRUE)
})

test_that("armadillo.copy_workspace checks if the source workspace exists", {
  head_object <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.copy_workspace(
        folder = "example",
        name = "test",
        new_folder = "gecko_subset_1"
      ),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = head_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "Workspace 'test' doesnot exist\\."
  )

  expect_args(head_object, 1, "test.RData", "shared-example", use_https = TRUE)
})

test_that("armadillo.copy_workspace checks if the target folder exists", {
  bucket_exists <- mock(TRUE, FALSE)

  expect_error(
    with_mock(
      armadillo.copy_workspace(
        folder = "example",
        name = "test",
        new_folder = "target"
      ),
      "aws.s3::bucket_exists" = bucket_exists,
      "aws.s3::head_object" = mock(TRUE),
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "Folder 'target' doesnot exist\\."
  )

  expect_args(bucket_exists, 2, "shared-target", use_https = TRUE)
})

test_that("armadillo.copy_workspace warns if you copy object onto itself", {
  expect_error(
    armadillo.copy_workspace(
      folder = "example",
      name = "test",
      new_folder = "example"
    ),
    "Cannot copy workspace onto itself\\."
  )
})

test_that("armadillo.copy_workspace copies workspace", {
  copy_object <- mock(TRUE)

  expect_message(
    with_mock(
      result <- armadillo.copy_workspace(
        folder = "example",
        name = "test",
        new_folder = "target"
      ),
      "aws.s3::bucket_exists" = mock(TRUE, cycle = TRUE),
      "aws.s3::head_object" = mock(TRUE),
      "aws.s3::copy_object" = copy_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "Copied workspace 'test' to folder 'target'\\."
  )

  expect_true(result)
  expect_args(copy_object, 1,
    from_object = "test.RData",
    to_object = "test.RData",
    from_bucket = "shared-example",
    to_bucket = "shared-target",
    use_https = TRUE
  )
})

test_that("armadillo.load_workspace checks if the folder exists", {
  bucket_exists <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.load_workspace(
        folder = "example",
        name = "tim_subset_1"
      ),
      "aws.s3::bucket_exists" = bucket_exists,
      "MolgenisArmadillo:::.use_https" = mock(TRUE)
    ),
    "Folder 'example' doesnot exist\\."
  )

  expect_args(bucket_exists, 1, "shared-example", use_https = TRUE)
})

test_that("armadillo.load_workspace checks if the workspace exists", {
  head_object <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.copy_workspace(
        folder = "example",
        name = "test",
        new_folder = "gecko_subset_1"
      ),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = head_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "Workspace 'test' doesnot exist\\."
  )

  expect_args(head_object, 1, "test.RData", "shared-example", use_https = TRUE)
})

test_that("armadillo.load_workspace loads the workspace", {
  s3load <- mock(invisible(NULL))
  environment <- new.env()

  expect_silent(
    with_mock(
      result <- armadillo.load_workspace(
        folder = "example",
        name = "test",
        env = environment
      ),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::head_object" = mock(TRUE),
      "aws.s3::s3load" = s3load,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    )
  )

  expect_equal(result, invisible(NULL))
  expect_args(s3load, 1,
    object = "test.RData",
    bucket = "shared-example",
    use_https = TRUE,
    envir = environment
  )
})
