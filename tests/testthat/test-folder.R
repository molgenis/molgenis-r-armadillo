test_that("armadillo.create_folder checks folder name", {
  expect_error(
    session <- armadillo.create_folder("example_folder"),
    "Folder name must consist of lowercase letters and numbers\\."
  )
})

test_that("armadillo.create_folder creates a folder", {
  put_bucket <- mock(TRUE)
  use_https <- mock(FALSE)

  with_mock(
    result <- armadillo.create_folder("examplefolder"),
    "aws.s3::put_bucket" = put_bucket,
    "MolgenisArmadillo:::.use_https" = use_https
  )

  expect_true(result)
  expect_args(put_bucket, 1, "shared-examplefolder", use_https = FALSE)
})

test_that("armadillo.list_folders lists all buckets, user and shared", {
  bucketlist <- mock(buckets)
  use_https <- mock(FALSE)

  with_mock(
    folders <- armadillo.list_folders(),
    "aws.s3::bucketlist" = bucketlist,
    "MolgenisArmadillo:::.use_https" = use_https
  )

  expect_equal(
    folders,
    c(
      "shared-diabetes", "shared-gecko",
      "user-admin", "user-c5dde1e1-95c5-4eb7-8c34-1296ac53562e"
    )
  )
  expect_args(bucketlist, 1, use_https = FALSE)
})

test_that("armadillo.list_user_folders lists all user buckets", {
  bucketlist <- mock(buckets)
  use_https <- mock(FALSE)

  with_mock(
    folders <- armadillo.list_user_folders(),
    "aws.s3::bucketlist" = bucketlist,
    "MolgenisArmadillo:::.use_https" = use_https
  )

  expect_equal(
    folders,
    c("admin", "c5dde1e1-95c5-4eb7-8c34-1296ac53562e")
  )
  expect_args(bucketlist, 1, use_https = FALSE)
})

test_that("armadillo.list_shared_folders lists all shared buckets", {
  bucketlist <- mock(buckets)
  use_https <- mock(FALSE)

  with_mock(
    folders <- armadillo.list_shared_folders(),
    "aws.s3::bucketlist" = bucketlist,
    "MolgenisArmadillo:::.use_https" = use_https
  )

  expect_equal(
    folders,
    c("diabetes", "gecko")
  )
  expect_args(bucketlist, 1, use_https = FALSE)
})

test_that("armadillo.delete_user_folder checks if folder exists", {
  bucketexists <- mock(FALSE)

  expect_error(
    with_mock(
      "aws.s3::bucket_exists" = bucketexists,
      armadillo.delete_user_folder("admin")
    ), "Folder 'admin' does not exist."
  )
})
