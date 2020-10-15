test_that("armadillo.create_project checks folder name", {
  expect_error(
    armadillo.create_project("example_folder"),
    "Project name must consist of lowercase letters and numbers\\."
  )
})

test_that("armadillo.create_project creates a folder", {
  put_bucket <- mock(TRUE)
  use_https <- mock(FALSE)

  result <- with_mock(
    armadillo.create_project("project"),
    "aws.s3::put_bucket" = put_bucket,
    "MolgenisArmadillo:::.use_https" = use_https
  )

  expect_true(result)
  expect_args(put_bucket, 1, "shared-project", use_https = FALSE)
})

test_that("armadillo.list_projects lists all shared buckets", {
  bucketlist <- mock(buckets)
  use_https <- mock(FALSE)

  folders <- with_mock(
    armadillo.list_projects(),
    "aws.s3::bucketlist" = bucketlist,
    "MolgenisArmadillo:::.use_https" = use_https
  )

  expect_equal(
    folders,
    c("diabetes", "gecko")
  )
  expect_args(bucketlist, 1, use_https = FALSE)
})

test_that("armadillo.delete_project checks if project exists", {
  bucketexists <- mock(FALSE)

  expect_error(
    with_mock(
      "aws.s3::bucket_exists" = bucketexists,
      armadillo.delete_project("project")
    ), "Project 'project' does not exist."
  )
})

test_that("armadillo.delete_project deletes project", {
  deletebucket <- mock(TRUE)

  expect_message(
    with_mock(
      armadillo.delete_project("project"),
      "aws.s3::bucket_exists" = mock(TRUE),
      "aws.s3::delete_bucket" = deletebucket,
      "MolgenisArmadillo:::.use_https" = mock(TRUE, cycle = TRUE)
    ), "Deleted project 'project'"
  )

  expect_args(deletebucket, 1, "shared-project", use_https = TRUE)
})
