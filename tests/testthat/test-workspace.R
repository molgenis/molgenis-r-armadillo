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
  bucket_exists <- mock(TRUE)
  head_object <- mock(FALSE)

  expect_error(
    with_mock(
      armadillo.delete_workspace("example", "test"),
      "aws.s3::bucket_exists" = bucket_exists,
      "aws.s3::head_object" = head_object,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ),
    "Workspace 'test' doesnot exist\\."
  )

  expect_args(head_object, 1, "test.RData", "shared-example", use_https = TRUE)
})
